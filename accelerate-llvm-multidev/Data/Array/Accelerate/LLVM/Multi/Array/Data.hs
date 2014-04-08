{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.Array.Data
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Multi.Array.Data (

  allocateArray,
  module Data.Array.Accelerate.LLVM.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array(..), Shape, Elt )

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.Multi.Target
import Data.Array.Accelerate.LLVM.Multi.Execute.Async           ()
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Data      as PTX ( allocateArray )


-- Instance of remote array memory management for the Multi-device target. We
-- just apply the operations of the PTX backend to ensure data is copied to the
-- GPU.
--
instance Remote Multi where

  {-# INLINEABLE copyToRemote #-}
  copyToRemote arrs =
    copyToRemote arrs `with` ptxTarget

  {-# INLINEABLE copyToRemoteAsync #-}
  copyToRemoteAsync arrs stream =
    copyToRemoteAsync arrs stream `with` ptxTarget

  {-# INLINEABLE copyToHost #-}
  copyToHost arrs =
    copyToHost arrs `with` ptxTarget

  {-# INLINEABLE copyToPeer #-}
  copyToPeer peer arrs =
    copyToPeer (ptxTarget peer) arrs `with` ptxTarget

  {-# INLINEABLE copyToPeerAsync #-}
  copyToPeerAsync peer async stream =
    copyToPeerAsync (ptxTarget peer) async stream `with` ptxTarget

  {-# INLINEABLE indexRemote #-}
  indexRemote arr i =
    indexRemote arr i `with` ptxTarget


-- | Allocate a new, uninitialised Accelerate array on the host and device.
--
{-# INLINEABLE allocateArray #-}
allocateArray :: (Shape sh, Elt e) => sh -> LLVM Multi (Array sh e)
allocateArray sh = do
  PTX.allocateArray sh `with` ptxTarget

