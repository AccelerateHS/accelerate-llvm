{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Array.Prim
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Array.Prim
  where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.LLVM.NVVM.Array.Table
import qualified Data.Array.Accelerate.LLVM.Debug               as Debug

-- CUDA
import qualified Foreign.CUDA.Driver                            as CUDA

-- standard library
import Prelude                                                  hiding ( lookup )
import Control.Monad
import Data.Maybe
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc                                    ( alloca )

#include "accelerate.h"


-- | A combination of 'mallocArray' and 'pokeArray', that allocates remotes
-- memory and uploads an existing array. This is specialised because if the
-- array is shared on the heap, we do not need to do anything.
--
{-# INLINEABLE useArray #-}
useArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => CUDA.Context
    -> MemoryTable
    -> ArrayData e
    -> Int
    -> IO ()
useArray !ctx !mt !ad !i = do
  let !src      = ptrsOfArrayData ad
      !bytes    = i * sizeOf (undefined::a)
  exists        <- isJust `liftM` (lookup mt ad :: IO (Maybe (CUDA.DevicePtr a)))
  unless exists $ do
    dst <- malloc ctx mt ad i
    transfer "useArray/malloc" bytes $ CUDA.pokeArray i src dst

-- | Read a single element from an array at a given row-major index
--
{-# INLINEABLE indexArray #-}
indexArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => CUDA.Context
    -> MemoryTable
    -> ArrayData e
    -> Int
    -> IO a
indexArray _ !mt !ad !i =
  alloca          $   \dst ->
  devicePtr mt ad >>= \src -> do
    message $ "indexArray: " ++ showBytes (sizeOf (undefined::a))
    CUDA.peekArray 1 (src `CUDA.advanceDevPtr` i) dst
    peek dst


-- | Lookup the device memory associated with a given host array
--
{-# INLINEABLE devicePtr #-}
devicePtr
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a, Typeable b)
    => MemoryTable
    -> ArrayData e
    -> IO (CUDA.DevicePtr b)
devicePtr !mt !ad = do
  mv <- lookup mt ad
  case mv of
    Just v      -> return v
    Nothing     -> INTERNAL_ERROR(error) "devicePtr" "lost device memory"


-- Debug
-- -----

{-# INLINE showBytes #-}
showBytes :: Int -> String
showBytes x = Debug.showFFloatSIBase (Just 0) 1024 (fromIntegral x :: Double) "B"

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = Debug.message Debug.dump_gc ("gc: " ++ msg) >> next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

{-# INLINE transfer #-}
transfer :: String -> Int -> IO () -> IO ()
transfer name bytes action
  = let showRate x t         = Debug.showFFloatSIBase (Just 3) 1024 (fromIntegral x / t) "B/s"
        msg wallTime cpuTime = "gc: " ++ name ++ ": "
                                      ++ showBytes bytes ++ " @ " ++ showRate bytes wallTime ++ ", "
                                      ++ Debug.elapsed wallTime cpuTime
    in
    Debug.timed Debug.dump_gc msg action

