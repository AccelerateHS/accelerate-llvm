{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Array.Data
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Array.Data (

  allocateArray,
  useArrayAsync,
  module Data.Array.Accelerate.LLVM.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array(..), Shape, Elt, size )
import qualified Data.Array.Accelerate.Array.Sugar              as Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim      as Prim

-- cuda
import qualified Foreign.CUDA.Driver.Stream                     as CUDA

-- standard library
import Control.Monad.State


-- Instance of remote array memory management for the PTX target
--
instance Remote PTX where

  {-# INLINEABLE copyToRemote #-}
  copyToRemote arrs = do
    PTX{..} <- gets llvmTarget
    liftIO $ runArrays (runUseArray (Prim.useArrayAsync ptxContext ptxMemoryTable Nothing)) arrs

  {-# INLINEABLE copyToHost #-}
  copyToHost arrs = do
    PTX{..} <- gets llvmTarget
    liftIO $ runArrays (\arr@(Array sh _) -> runArrayData1 (Prim.peekArray ptxContext ptxMemoryTable) arr (R.size sh)) arrs
    return arrs

  {-# INLINEABLE copyToPeer #-}
  copyToPeer peer arrs = do
    PTX dstCtx dstMT _ <- return peer
    PTX srcCtx srcMT _ <- gets llvmTarget

    liftIO . unless (srcCtx == dstCtx)
      $ runArrays (\arr@(Array sh _) -> runArrayData1 (Prim.copyArrayPeer srcCtx srcMT dstCtx dstMT) arr (R.size sh)) arrs

    return arrs

  -- | Read a single element from the array at a given row-major index
  --
  {-# INLINEABLE indexRemote #-}
  indexRemote arr i = do
    PTX{..} <- gets llvmTarget
    liftIO   $ runIndexArray (Prim.indexArray ptxContext ptxMemoryTable) arr i

-- | Upload an existing array to the device, asynchronously
--
{-# INLINEABLE useArrayAsync #-}
useArrayAsync :: Array sh e -> Maybe CUDA.Stream -> LLVM PTX ()
useArrayAsync arr st = do
  PTX{..} <- gets llvmTarget
  liftIO   $ runUseArray (Prim.useArrayAsync ptxContext ptxMemoryTable st) arr


-- | Allocate a new, uninitialised Accelerate array on the host and device.
--
{-# INLINEABLE allocateArray #-}
allocateArray :: (Shape sh, Elt e) => sh -> LLVM PTX (Array sh e)
allocateArray sh = do
  let arr = Sugar.allocateArray sh
  PTX{..} <- gets llvmTarget
  liftIO   $ runArrayData1 (\ad i -> void $ Prim.mallocArray ptxContext ptxMemoryTable ad i) arr (size sh)
  return arr

