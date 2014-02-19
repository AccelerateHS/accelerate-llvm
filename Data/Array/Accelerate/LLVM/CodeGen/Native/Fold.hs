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


-- Reduce a multidimensional array. Each thread reduces along one innermost
-- dimension.
--
-- This function is invoked repeatedly by the host process, rather than having
-- the loop itself select which dimensions to reduce. This may impose a (too)
-- high overhead.
--
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
    arrOut              = arrayData  (undefined::Array sh e) "out"
    paramOut            = arrayParam (undefined::Array sh e) "out"
    paramIn             = envParam aenv
    (start, end, gang)  = gangParam

    ty_acc              = llvmOfTupleType (eltType (undefined::e))

    runBody :: CodeGen [BasicBlock]
    runBody = do
      loop      <- newBlock "loop.top"
      exit      <- newBlock "loop.exit"

      -- Entry
      -- -----
      z         <- seed
      c         <- lt int start end
      top       <- cbr c loop exit

      -- Main loop
      -- ---------
      setBlock loop
      crit_i    <- freshName                            -- define critical loop variables
      crit_acc  <- replicateM (length ty_acc) freshName
      let i     = local crit_i
          acc   = map local crit_acc

      x         <- delayedLinearIndex [i]               -- reduction step
      acc'      <- combine acc x

      i'        <- add int i (constOp $ num int 1)      -- next loop
      c'        <- eq int i' end
      bot       <- cbr c' exit loop

      _         <- phi loop crit_i (typeOf (int :: IntegralType Int)) [(i',bot), (start,top)]
      _         <- sequence $ zipWith3 (phi loop) crit_acc
                                                  ty_acc
                                                  [ [(b,bot), (t,top)] | b <- acc' | t <- z ]

      setBlock exit
      r         <- zipWithM phi' ty_acc [ [(b,bot), (t,top)] | b <- acc' | t <- z ]
      writeArray arrOut (constOp $ num int 0) r
      return_
      --
      >> createBlocks


-- Reduce a non-empty array to a single element. Since there is no seed element
-- provided, initialise the loop accumulator with the first element of the
-- array.
--
-- Note that we still do some amount of bounds checking because for small
-- arrays, a thread might process zero elements.
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
    arrOut              = arrayData  (undefined::Array sh e) "out"
    paramOut            = arrayParam (undefined::Array sh e) "out"
    paramIn             = envParam aenv
    (start, end, gang)  = gangParam

    ty_acc              = llvmOfTupleType (eltType (undefined::e))

    runBody :: CodeGen [BasicBlock]
    runBody = do
      loop      <- newBlock "loop.top"
      exit      <- newBlock "loop.exit"

      -- Initialise the loop counter with the first element of the array. The
      -- thread won't be launched if the interval is empty.
      z         <- delayedLinearIndex [start]
      start'    <- add int start (constOp $ num int 1)
      c         <- eq int start' end
      top       <- cbr c exit loop

      -- Main loop
      -- ---------
      setBlock loop
      crit_i    <- freshName                            -- define critical loop variables
      crit_acc  <- replicateM (length ty_acc) freshName
      let i     = local crit_i
          acc   = map local crit_acc

      x         <- delayedLinearIndex [i]               -- reduction step
      acc'      <- combine acc x

      i'        <- add int i (constOp $ num int 1)      -- next loop
      c'        <- eq int i' end
      bot       <- cbr c' exit loop

      _         <- phi loop crit_i (typeOf (int :: IntegralType Int)) [(i',bot), (start',top)]
      _         <- sequence $ zipWith3 (phi loop) crit_acc
                                                  ty_acc
                                                  [ [(b,bot), (t,top)] | b <- acc' | t <- z ]

      setBlock exit
      r         <- zipWithM phi' ty_acc [ [(b,bot), (t,top)] | b <- acc' | t <- z ]
      writeArray arrOut (constOp $ num int 0) r
      return_
      --
      >> createBlocks

