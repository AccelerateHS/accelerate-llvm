{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Array.Data
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Array.Data (

  module Data.Array.Accelerate.LLVM.Array.Data,

) where

import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Native.Execute.Async ()


-- | Data instance for arrays in the native backend. We assume a shared-memory
-- machine, and just manipulate the underlying Haskell array directly.
--
instance Remote Native

