{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.PTX.Target                    ( PTX )
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Loop

import LLVM.General.AST
import LLVM.General.Quote.LLVM

-- standard library
import Prelude                                                  hiding ( fromIntegral )


-- Construct a new array by applying a function to each index. Each thread
-- processes multiple adjacent elements.
--
mkGenerate
    :: forall arch aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRFun1 aenv (sh -> e)
    -> CodeGen [Kernel arch aenv (Array sh e)]
mkGenerate _dev aenv apply = do
  let
      arrOut                      = arrayData  (undefined::Array sh e) "out"
      shOut                       = arrayShape (undefined::Array sh e) "out"
      paramOut                    = arrayParam (undefined::Array sh e) "out"
      paramEnv                    = envParam aenv
      (start, end, paramGang)     = gangParam
      intType                     = (typeOf (integralType :: IntegralType Int))
  step  <- gridSize
  tid   <- globalThreadIdx
  k <- [llgM|
  define void @generate (
    $params:(paramGang) ,
    $params:(paramOut) ,
    $params:(paramEnv)
    ) {
      $bbsM:(exec $ return ())               ;; splice in the BasicBlocks from above (step, tid)
      %z = add i32 $opr:(tid), $opr:(start)
      br label %nextblock
      for i32 %i in %z to $opr:(end) step $opr:(step) {
        %ii = sext i32 %i to $type:(intType)
        br label %nextblock
        $bbsM:("ix" .=. indexOfInt (map local shOut) ("ii" :: Operand))
        $bbsM:("r" .=. apply ("ix" :: Name))
        $bbsM:(execRet_ (writeArray arrOut "i" ("r" :: Name)))
      }
      ret void
  }
  |]
  addMetadata "nvvm.annotations" [ Just $ global "generate"
                                 , Just $ MetadataStringOperand "kernel"
                                 , Just $ constOp (num int32 1) ]
  return $ [Kernel k]
