{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Native.Fold
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Native.Fold
  where

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.Global

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Shape
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

-- standard library
import Control.Applicative
import Control.Monad
import GHC.Conc

#include "accelerate.h"


-- Reduce an array along the innermost dimension
--
mkFold
    :: forall t aenv sh e. (Shape sh, Elt e)
    => Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold aenv f z a
  -- Either (1) multidimensional fold; or
  --        (2) only using one CPU, so just execute sequentially
  | numCapabilities <= 1 || expDim (undefined::Exp aenv sh) > 0
  = mkFold' aenv f z a

  -- Parallel foldAll
  | otherwise
  = (++) <$> mkFold1' aenv f a <*> mkFoldAll' aenv f z


-- Reduce an array along the innermost dimension. The innermost dimension must
-- not be empty.
--
mkFold1
    :: forall t aenv sh e. (Shape sh, Elt e)
    => Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold1 aenv f a
  -- Either (1) multidimensional fold; or
  --        (2) only using one CPU, so just execute sequentially
  | numCapabilities <= 1 || expDim (undefined::Exp aenv sh) > 0
  = mkFold1' aenv f a

  -- Parallel foldAll
  | otherwise
  = (++) <$> mkFold1' aenv f a <*> mkFold1All' aenv f


-- Multidimensional reduction
-- --------------------------

-- Reduce a multidimensional array. Threads sequentially reduce along the
-- innermost dimensions, so don't need to communicate. Each thread is given a
-- range of innermost dimensions to iterate over, given by [start,end).
--
-- > fold start end = iter start (start * n)
-- >   where
-- >     iter sh sz
-- >       | sh >= end       = return ()
-- >       | otherwise       = do
-- >           let next = sz + n
-- >           writeArray out sh (reduce indexArray c z sz next)
-- >           iter (sh+1) next
--
mkFold'
    :: forall t aenv sh e. (Shape sh, Elt e)
    => Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold' aenv combine seed IRDelayed{..} = do
  code <- runBody
  return [ Kernel $ functionDefaults
             { returnType  = VoidType
             , name        = "fold"
             , parameters  = (gang ++ paramIn ++ paramOut, False)
             , basicBlocks = code
             } ]
  where
    (start, end, gang)  = gangParam
    paramIn             = envParam aenv
    arrOut              = arrayData  (undefined::Array sh e) "out"
    paramOut            = arrayParam (undefined::Array sh e) "out"
                       ++ [Parameter (typeOf (integralType :: IntegralType Int)) "ix.stride" []]

    ty_acc              = llvmOfTupleType (eltType (undefined::e))

    -- innermost dimension; length to reduce over
    n                   = local "ix.stride"

    runBody :: CodeGen [BasicBlock]
    runBody = do
      loop      <- newBlock "fold.top"
      exit      <- newBlock "fold.exit"

      seg       <- mul int start n
      c         <- gte int start end
      top       <- cbr c exit loop

      setBlock loop
      c_sh      <- freshName
      c_sz      <- freshName
      let sh    = local c_sh
          sz    = local c_sz

      next      <- add int sz n
      r         <- reduce ty_acc delayedLinearIndex combine seed sz next
      writeArray arrOut sh r

      sh'       <- add int sh (constOp $ num int 1)
      c'        <- gte int sh' end
      bot       <- cbr c' exit loop
      _         <- phi loop c_sz (typeOf (int :: IntegralType Int)) [ (seg,top),   (next,bot) ]
      _         <- phi loop c_sh (typeOf (int :: IntegralType Int)) [ (start,top), (sh',bot)  ]

      setBlock exit
      return_
      >>
      createBlocks


-- Reduce a non-empty array to a single element. Since there is no seed element
-- provided, initialise the loop accumulator with the first element of the
-- array.
--
mkFold1'
    :: forall t aenv sh e. (Shape sh, Elt e)
    => Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold1' aenv combine IRDelayed{..} = do
  code <- runBody
  return [ Kernel $ functionDefaults
             { returnType  = VoidType
             , name        = "fold1"
             , parameters  = (gang ++ paramIn ++ paramOut, False)
             , basicBlocks = code
             } ]
  where
    (start, end, gang)  = gangParam
    paramIn             = envParam aenv
    arrOut              = arrayData  (undefined::Array sh e) "out"
    paramOut            = arrayParam (undefined::Array sh e) "out"
                       ++ [Parameter (typeOf (integralType :: IntegralType Int)) "ix.stride" []]

    n                   = local "ix.stride"
    ty_acc              = llvmOfTupleType (eltType (undefined::e))

    runBody :: CodeGen [BasicBlock]
    runBody = do
      loop      <- newBlock "fold.top"
      exit      <- newBlock "fold.exit"

      seg       <- mul int start n
      c         <- gte int start end
      top       <- cbr c exit loop

      setBlock loop
      c_sh      <- freshName
      c_sz      <- freshName
      let sh    = local c_sh
          sz    = local c_sz

      sz'       <- add int sz (constOp $ num int 1)
      next      <- add int sz n
      r         <- reduce ty_acc delayedLinearIndex combine (delayedLinearIndex [sz]) sz' next
      writeArray arrOut sh r

      sh'       <- add int sh (constOp $ num int 1)
      c'        <- gte int sh' end
      bot       <- cbr c' exit loop
      _         <- phi loop c_sz (typeOf (int :: IntegralType Int)) [ (seg,top),   (next,bot) ]
      _         <- phi loop c_sh (typeOf (int :: IntegralType Int)) [ (start,top), (sh',bot) ]

      setBlock exit
      return_
      >>
      createBlocks


-- Scalar reduction
-- ----------------

-- Reduce an array to a single element, with all threads cooperating.
--
-- Since reductions consume arrays that have been fused into them, fold in a
-- multi-threaded context requires two passes. At an example, take vector
-- dot-product:
--
-- > dotp xs ys = fold (+) 0 (zipWith (*) xs ys)
--
--   1. The first pass reads in the fused array data, in this case corresponding
--   to the function (\i -> (xs!i) * (ys!i)).
--
--   2. The second pass just reads the manifest data from the first step
--   directly and reduces the array. This second step is just done by a single
--   thread.
--
mkFoldAll'
    :: forall t aenv sh e. (Shape sh, Elt e)
    => Gamma aenv
    -> IRFun2 aenv (e -> e -> e)
    -> IRExp  aenv e
    -> CodeGen [Kernel t aenv (Array sh e)]             -- types are bit odd, but required
mkFoldAll' aenv combine seed = do
  code <- runBody
  return [ Kernel $ functionDefaults
             { returnType  = VoidType
             , name        = "foldAll"
             , parameters  = (paramIn ++ paramOut, False)
             , basicBlocks = code
             } ]
  where
    paramIn             = envParam aenv ++
                          arrayParam (undefined::Array (sh:.Int) e) "in"
    arrIn               = arrayData  (undefined::Array (sh:.Int) e) "in"
    shIn                = arrayShape (undefined::Array (sh:.Int) e) "in"

    paramOut            = arrayParam (undefined::Array sh e) "out"
    arrOut              = arrayData  (undefined::Array sh e) "out"

    ty_acc              = llvmOfTupleType (eltType (undefined::e))
    get [i]             = readArray arrIn i
    get _               = INTERNAL_ERROR(error) "makeFoldAll" "expected single expression"

    one                 = constOp (num int 1)
    zero                = constOp (num int 0)

    runBody :: CodeGen [BasicBlock]
    runBody = do
      sh        <- foldM (mul int) one (map local shIn)
      r         <- reduce ty_acc get combine seed zero sh
      writeArray arrOut zero r
      return_
      >>
      createBlocks


-- Reduce an array to a single element, without a starting value.
--
-- This is just the second phase of the operation, which takes the manifest
-- array of partial summations from each thread and reduces them to a single
-- value.
--
mkFold1All'
    :: forall t aenv sh e. (Shape sh, Elt e)
    => Gamma aenv
    -> IRFun2 aenv (e -> e -> e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold1All' aenv combine = do
  code <- runBody
  return [ Kernel $ functionDefaults
             { returnType  = VoidType
             , name        = "foldAll"
             , parameters  = (paramIn ++ paramOut, False)
             , basicBlocks = code
             } ]
  where
    paramIn             = envParam aenv ++
                          arrayParam (undefined::Array (sh:.Int) e) "in"
    arrIn               = arrayData  (undefined::Array (sh:.Int) e) "in"
    shIn                = arrayShape (undefined::Array (sh:.Int) e) "in"

    paramOut            = arrayParam (undefined::Array sh e) "out"
    arrOut              = arrayData  (undefined::Array sh e) "out"

    ty_acc              = llvmOfTupleType (eltType (undefined::e))
    get [i]             = readArray arrIn i
    get _               = INTERNAL_ERROR(error) "makeFoldAll" "expected single expression"

    one                 = constOp (num int 1)
    zero                = constOp (num int 0)

    runBody :: CodeGen [BasicBlock]
    runBody = do
      sh        <- foldM (mul int) one (map local shIn)
      r         <- reduce ty_acc get combine (get [zero]) one sh
      writeArray arrOut zero r
      return_
      >>
      createBlocks


-- Reduction loops
-- ---------------

-- Sequentially reduce all elements between the start and end indices, using the
-- provided seed element, combination function, and function to retrieve each
-- element.
--
-- > reduce indexArray c z start end = iter start z
-- >   where
-- >     iter i acc
-- >       | i >= end        = acc
-- >       | otherwise       = iter (i+1) (acc `c` indexArray i)
--
reduce :: [Type]                                        -- Type of the accumulator
       -> ([Operand] -> CodeGen [Operand])              -- read an element of the array
       -> ([Operand] -> [Operand] -> CodeGen [Operand]) -- combine elements
       -> CodeGen [Operand]                             -- seed
       -> Operand                                       -- starting array index
       -> Operand                                       -- final index
       -> CodeGen [Operand]                             -- reduction
reduce ty get combine seed start end = do
  loop  <- newBlock "reduce.top"
  exit  <- newBlock "reduce.exit"

  z     <- seed
  c     <- gte int start end    -- segment may be empty: just the initial value
  top   <- cbr c exit loop

  setBlock loop
  -- First define the critical variables
  c_i   <- freshName
  c_acc <- replicateM (length ty) freshName
  let i   = local c_i
      acc = map local c_acc

  -- Get new element, add to accumulator
  x     <- get [i]
  acc'  <- combine acc x

  -- Determine the loop condition and branch
  i'    <- add int i (constOp $ num int 1)
  c'    <- gte int i' end
  bot   <- cbr c' exit loop

  -- set up the phi loop
  _     <- phi loop c_i (typeOf (int :: IntegralType Int)) [ (i',bot), (start,top) ]
  _     <- sequence $ zipWith3 (phi loop) c_acc ty
              [ [(t,top), (b,bot)] | t <- z | b <- acc' ]

  -- exit loop
  setBlock exit
  zipWithM phi' ty [ [(t,top), (b,bot) ] | t <- z | b <- acc' ]

