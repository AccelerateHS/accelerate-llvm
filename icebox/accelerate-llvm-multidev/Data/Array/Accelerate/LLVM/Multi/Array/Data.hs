{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.Array.Data
-- Copyright   : [2014..2015] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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


-- Instance of remote array memory management for the Multi-device target. Since
-- after the execution of each kernel the CPU and GPU memories are explicitly
-- synchronised, for the most part no additional copying here is required. The
-- only exception is when we Use an array, in which case we transfer it to all
-- remote targets.
--
instance Remote Multi where

  {-# INLINEABLE allocateRemote #-}
  allocateRemote sh =
    allocateRemote sh `with` ptxTarget

  {-# INLINEABLE useRemoteR #-}
  useRemoteR stream arr =
    useRemoteR stream arr `with` ptxTarget

