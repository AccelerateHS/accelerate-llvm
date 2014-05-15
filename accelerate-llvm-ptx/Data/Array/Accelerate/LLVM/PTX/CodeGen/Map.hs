{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Map
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Map
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Elt )

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.PTX.Target                    ( PTX )
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base

import LLVM.General.AST
import LLVM.General.Quote.LLVM

-- standard library
import Prelude                                                  hiding ( fromIntegral )


-- Apply a unary function to each element of an array. Each thread processes
-- multiple elements, striding the array by the grid size.
--
mkMap :: forall t aenv sh a b. Elt b
      => PTX
      -> Gamma aenv
      -> IRFun1    aenv (a -> b)
      -> IRDelayed aenv (Array sh a)
      -> CodeGen [Kernel t aenv (Array sh b)]
mkMap _nvvm aenv apply IRDelayed{..} = do
  let (start, end, paramGang)   = gangParam
      arrOut                    = arrayData  (undefined::Array sh b) "out"
      paramOut                  = arrayParam (undefined::Array sh b) "out"
      paramEnv                  = envParam aenv
  step  <- gridSize
  tid   <- globalThreadIdx
  k <- [llgM|
  define void @map (
    $params:(paramGang) ,
    $params:(paramOut) ,
    $params:(paramEnv)
    ) {
      $bbsM:(exec $ return ())               ;; splice in the BasicBlocks from above (step, tid)
      %z = add i32 $opr:(tid), $opr:(start)
      br label %nextblock
      for i32 %i in %z to $opr:(end) step $opr:(step) {
        $bbsM:("x" .=. delayedLinearIndex ("i" :: [Operand]))
        $bbsM:("y" .=. apply ("x" :: Name))
        $bbsM:(execRet_ (writeArray arrOut "i" ("y" :: Name)))
      }
      ret void
  }
  |]
  addMetadata "nvvm.annotations" [ Just $ global "map"
                                 , Just $ MetadataStringOperand "kernel"
                                 , Just $ constOp (num int32 1) ]
  return $ [Kernel k]
