{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Array.Data
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Array.Data (

  module Data.Array.Accelerate.LLVM.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Array.Data

import Data.Array.Accelerate.LLVM.NVVM.Target
import Data.Array.Accelerate.LLVM.NVVM.Array.Table
import qualified Data.Array.Accelerate.LLVM.NVVM.Array.Prim     as Prim

-- cuda
import qualified Foreign.CUDA.Driver                            as CUDA

-- standard library
import Control.Monad.State


-- Instance of remote array memory management for the NVVM target
--
instance Remote NVVM where

  indexArray arr i = do
    NVVM{..}    <- gets llvmTarget
    liftIO      $ runIndexArray (Prim.indexArray nvvmContext nvvmMemoryTable) arr i

  useArray arr = do
    NVVM{..}    <- gets llvmTarget
    liftIO      $ runUseArray (Prim.useArray nvvmContext nvvmMemoryTable) arr

