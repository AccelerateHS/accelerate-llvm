-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Array.Data
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Array.Data (

  indexArray, allocateArray,

  module Data.Array.Accelerate.Array.Data,

) where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar        ( Array(..), allocateArray, toElt )


-- | Read a single element from an array at a given row-major index.
--
indexArray :: Array sh e -> Int -> e
indexArray (Array _ adata) i = toElt (adata `unsafeIndexArrayData` i)

