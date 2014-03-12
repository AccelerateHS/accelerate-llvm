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

module Data.Array.Accelerate.LLVM.NVVM.Array.Prim (

  mallocArray,
  useArray, useArrayAsync,
  indexArray,
  peekArray, peekArrayAsync,
  copyArrayPeer, copyArrayPeerAsync,
  devicePtr,

) where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.LLVM.NVVM.Array.Table
import qualified Data.Array.Accelerate.LLVM.NVVM.Debug          as Debug

-- CUDA
import qualified Foreign.CUDA.Driver                            as CUDA
import qualified Foreign.CUDA.Driver.Stream                     as CUDA

-- standard library
import Prelude                                                  hiding ( lookup )
import Control.Monad
import Data.Maybe
import Data.Typeable
import Foreign.Marshal.Alloc                                    ( alloca )
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

#include "accelerate.h"

-- | Allocate a device-side array associated with the given host array. If the
-- allocation fails due to a memory error, we attempt some last-ditch memory
-- cleanup before trying again.
--
{-# INLINEABLE mallocArray #-}
mallocArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => CUDA.Context
    -> MemoryTable
    -> ArrayData e
    -> Int
    -> IO (CUDA.DevicePtr a)
mallocArray !ctx !mt !ad !n = do
#ifdef ACCELERATE_INTERNAL_CHECKS
  exists <- isJust `fmap` (lookup mt ad :: IO (Maybe (CUDA.DevicePtr a)))
  _      <- INTERNAL_CHECK(check) "mallocArray" "double malloc" (not exists) (return ())
#endif
  message ("mallocArray: " ++ showBytes (n * sizeOf (undefined::a)))
  malloc ctx mt ad n :: IO (CUDA.DevicePtr a)


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
useArray !ctx !mt !ad !n =
  useArrayAsync ctx mt Nothing ad n

{-# INLINEABLE useArrayAsync #-}
useArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => CUDA.Context
    -> MemoryTable
    -> Maybe (CUDA.Stream)
    -> ArrayData e
    -> Int
    -> IO ()
useArrayAsync !ctx !mt !st !ad !n = do
  let !src      = CUDA.HostPtr (ptrsOfArrayData ad)
      !bytes    = n * sizeOf (undefined::a)
  exists        <- isJust `liftM` (lookup mt ad :: IO (Maybe (CUDA.DevicePtr a)))
  unless exists $ do
    dst <- malloc ctx mt ad n
    transfer "useArray" bytes st $ CUDA.pokeArrayAsync n src dst st


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


-- | Copy data from the device into the associated host-side Accelerate array
--
{-# INLINEABLE peekArray #-}
peekArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => CUDA.Context
    -> MemoryTable
    -> ArrayData e
    -> Int
    -> IO ()
peekArray ctx !mt !ad !n =
  peekArrayAsync ctx mt ad n Nothing

{-# INLINEABLE peekArrayAsync #-}
peekArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => CUDA.Context
    -> MemoryTable
    -> ArrayData e
    -> Int
    -> Maybe CUDA.Stream
    -> IO ()
peekArrayAsync _ !mt !ad !n !st = do
  let !bytes    = n * sizeOf (undefined :: a)
      !dst      = CUDA.HostPtr (ptrsOfArrayData ad)
  src   <- devicePtr mt ad
  transfer "peekArray" bytes st $ CUDA.peekArrayAsync n src dst st


-- | Copy data from one device context into a _new_ array on the second context.
-- It is an error if the destination array already exists.
--
{-# INLINEABLE copyArrayPeer #-}
copyArrayPeer
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => CUDA.Context -> MemoryTable              -- source context
    -> CUDA.Context -> MemoryTable              -- destination context
    -> ArrayData e
    -> Int
    -> IO ()
copyArrayPeer !ctx1 !mt1 !ctx2 !mt2 !ad !n =
  copyArrayPeerAsync ctx1 mt1 ctx2 mt2 ad n Nothing

{-# INLINEABLE copyArrayPeerAsync #-}
copyArrayPeerAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => CUDA.Context -> MemoryTable              -- source context
    -> CUDA.Context -> MemoryTable              -- destination context
    -> ArrayData e
    -> Int
    -> Maybe CUDA.Stream
    -> IO ()
copyArrayPeerAsync !ctx1 !mt1 !ctx2 !mt2 !ad !n !st = do
  let !bytes    = n * sizeOf (undefined :: a)
  src   <- devicePtr mt1 ad
  dst   <- mallocArray ctx2 mt2 ad n
  transfer "copyArrayPeer" bytes st $ CUDA.copyArrayPeerAsync n src ctx1 dst ctx2 st


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
transfer :: String -> Int -> Maybe CUDA.Stream -> IO () -> IO ()
transfer name bytes stream action
  = let showRate x t         = Debug.showFFloatSIBase (Just 3) 1024 (fromIntegral x / t) "B/s"
        msg gpuTime wallTime = printf "gc: %s: %s bytes @ %s, %s"
                                  name
                                  (showBytes bytes)
                                  (showRate bytes wallTime)
                                  (Debug.elapsed gpuTime wallTime)
    in
    Debug.timed Debug.dump_gc msg stream action

