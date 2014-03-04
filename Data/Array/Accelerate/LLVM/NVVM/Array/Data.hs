{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-orphans #-}
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

  useArrayAsync,
  module Data.Array.Accelerate.LLVM.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array )

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.NVVM.Target
import qualified Data.Array.Accelerate.LLVM.NVVM.Array.Prim     as Prim

-- cuda
import qualified Foreign.CUDA.Driver.Stream                     as CUDA

-- standard library
import Control.Monad.State


-- Instance of remote array memory management for the NVVM target
--
instance Remote NVVM where

  indexArray adata i = do
    NVVM{..}    <- gets llvmTarget
    liftIO      $ runIndexArray (Prim.indexArray nvvmContext nvvmMemoryTable) adata i

  useArray adata = do
    NVVM{..}    <- gets llvmTarget
    liftIO      $ runUseArray (Prim.useArray nvvmContext nvvmMemoryTable) adata


-- | Upload an existing array to the device
--
useArrayAsync :: Array sh e -> Maybe CUDA.Stream -> LLVM NVVM ()
useArrayAsync arr st = do
  NVVM{..} <- gets llvmTarget
  liftIO    $ runUseArray (Prim.useArrayAsync nvvmContext nvvmMemoryTable st) arr

