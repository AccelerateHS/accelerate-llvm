{-# OPTIONS -fno-warn-orphans #-}
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

  indexArray,
  module Data.Array.Accelerate.LLVM.Array.Data,

) where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar                        ( Array(..), toElt )

import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.Native.Target


-- | Data instance for arrays in the native backend. We assume a shared-memory
-- machine, and just manipulate the underlying Haskell array directly.
--
instance Remote Native


-- | Read a single element from the array at a given row-major index
--
indexArray :: Array sh e -> Int -> e
indexArray (Array _ adata) i = toElt $ unsafeIndexArrayData adata i

