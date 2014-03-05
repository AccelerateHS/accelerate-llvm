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

  newArray,
  allocateArray,

  useArrayAsync,

  module Data.Array.Accelerate.LLVM.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array(..), Shape, Elt, size )
import qualified Data.Array.Accelerate.Array.Sugar              as Sugar

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

  {-# INLINEABLE indexArray #-}
  indexArray arr i = do
    NVVM{..}    <- gets llvmTarget
    liftIO      $ runIndexArray (Prim.indexArray nvvmContext nvvmMemoryTable) arr i

  {-# INLINEABLE useArray #-}
  useArray arr = useArrayAsync arr Nothing



-- | Upload an existing array to the device, asynchronously
--
{-# INLINEABLE useArrayAsync #-}
useArrayAsync :: Array sh e -> Maybe CUDA.Stream -> LLVM NVVM ()
useArrayAsync arr st = do
  NVVM{..} <- gets llvmTarget
  liftIO    $ runUseArray (Prim.useArrayAsync nvvmContext nvvmMemoryTable st) arr


-- | Create an array from its representation function, uploading the result to
-- the device.
--
{-# INLINEABLE newArray #-}
newArray :: (Shape sh, Elt e) => sh -> (sh -> e) -> LLVM NVVM (Array sh e)
newArray sh f = do
  let arr = Sugar.newArray sh f
  useArray arr
  return arr


-- | Allocate a new, uninitialised Accelerate array on the host and device.
--
{-# INLINEABLE allocateArray #-}
allocateArray :: (Shape sh, Elt e) => sh -> LLVM NVVM (Array sh e)
allocateArray sh = do
  let arr = Sugar.allocateArray sh
  NVVM{..} <- gets llvmTarget
  liftIO    $ runArrayData1 (Prim.mallocArray nvvmContext nvvmMemoryTable) arr (size sh)
  return arr

