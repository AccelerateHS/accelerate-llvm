{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Generate
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.CodeGen.Generate
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.NVVM.Target                   ( NVVM )
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Loop

-- standard library
import Prelude                                                  hiding ( fromIntegral )


-- Construct a new array by applying a function to each index. Each thread
-- processes multiple adjacent elements.
--
mkGenerate
    :: forall arch aenv sh e. (Shape sh, Elt e)
    => NVVM
    -> Gamma aenv
    -> IRFun1 aenv (sh -> e)
    -> CodeGen [Kernel arch aenv (Array sh e)]
mkGenerate _dev aenv apply =
  let
      arrOut                      = arrayData  (undefined::Array sh e) "out"
      shOut                       = arrayShape (undefined::Array sh e) "out"
      paramOut                    = arrayParam (undefined::Array sh e) "out"
      paramEnv                    = envParam aenv
  in
  makeKernel "generate" (paramOut ++ paramEnv) $ do

    start <- return (constOp $ integral int32 0)
    end   <- shapeSize shOut

    imapFromTo start end $ \i -> do
      ii  <- fromIntegral int32 int i           -- keep the loop counter as i32, but do calculations in Int
      ix  <- indexOfInt (map local shOut) ii    -- convert to multidimensional index
      r   <- apply ix                           -- apply generator function
      writeArray arrOut i r                     -- write result

    return_

