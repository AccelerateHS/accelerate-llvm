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

  module Data.Array.Accelerate.LLVM.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.Multi.Target
import Data.Array.Accelerate.LLVM.Multi.Execute.Async           ()
import Data.Array.Accelerate.LLVM.PTX                           ()


-- Instance of remote array memory management for the Multi-device target. We
-- just apply the operations of the PTX backend to ensure data is copied to the
-- GPU.
--
instance Remote Multi where

  {-# INLINEABLE allocateRemote #-}
  allocateRemote sh =
    allocateRemote sh `with` ptxTarget

  {-# INLINEABLE copyToRemoteR #-}
  copyToRemoteR from to stream arr =
    copyToRemoteR from to stream arr `with` ptxTarget

  {-# INLINEABLE copyToHostR #-}
  copyToHostR from to stream arr =
    copyToHostR from to stream arr `with` ptxTarget

  {-# INLINEABLE copyToPeerR #-}
  copyToPeerR from to peer stream arr =
    copyToPeerR from to (ptxTarget peer) stream arr `with` ptxTarget

  {-# INLINEABLE indexRemote #-}
  indexRemote arr i =
    indexRemote arr i `with` ptxTarget

