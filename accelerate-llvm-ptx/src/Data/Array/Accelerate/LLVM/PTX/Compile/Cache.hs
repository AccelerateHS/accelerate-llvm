{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile.Cache
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile.Cache (

  module Data.Array.Accelerate.LLVM.Compile.Cache

) where

import Data.Array.Accelerate.LLVM.Compile.Cache
import Data.Array.Accelerate.LLVM.PTX.Target

import Control.Monad.State
import Data.Version
import Foreign.CUDA.Analysis
import System.FilePath
import qualified Data.ByteString.Char8                              as B8
import qualified Data.ByteString.Short.Char8                        as S8

import Paths_accelerate_llvm_ptx


instance Persistent PTX where
  targetCacheTemplate = do
    dev <- gets ptxDeviceProperties
    let Compute m n = computeCapability dev
    --
    return $ "accelerate-llvm-ptx-" ++ showVersion version
         </> "llvm-hs-" ++ VERSION_llvm_hs
         </> S8.unpack ptxTargetTriple
         </> B8.unpack (ptxISAVersion m n)
         </> "morp.sass"

