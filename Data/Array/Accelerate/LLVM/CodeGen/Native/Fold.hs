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
import Control.Monad


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
mkFold
    :: forall t aenv sh e. (Shape sh, Elt e)
    => Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold aenv combine seed IRDelayed{..} = do
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
mkFold1
    :: forall t aenv sh e. (Shape sh, Elt e)
    => Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold1 aenv combine IRDelayed{..} = do
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

