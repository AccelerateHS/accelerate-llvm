{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Map
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.CodeGen.Map
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

import Data.Array.Accelerate.LLVM.NVVM.Target                   ( NVVM )
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Loop

-- standard library
import Prelude                                                  hiding ( fromIntegral )


-- Apply a unary function to each element of an array. Each thread processes
-- multiple elements, striding the array by the grid size.
--
mkMap :: forall t aenv sh a b. Elt b
      => NVVM
      -> Gamma aenv
      -> IRFun1    aenv (a -> b)
      -> IRDelayed aenv (Array sh a)
      -> CodeGen [Kernel t aenv (Array sh b)]
mkMap _nvvm aenv apply IRDelayed{..} =
  let
      arrOut      = arrayData  (undefined::Array sh b) "out"
      paramOut    = arrayParam (undefined::Array sh b) "out"
      paramEnv    = envParam aenv
  in
  makeKernel "map" (paramOut ++ paramEnv) $ do

    start <- return (constOp $ integral int32 0)
    end   <- shapeSize (undefined::Array sh b) "out"

    imapFromTo start end $ \i -> do
      xs <- delayedLinearIndex [i]
      ys <- apply xs
      writeArray arrOut i ys

    return_

