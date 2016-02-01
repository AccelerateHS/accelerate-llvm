{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Array.Prim
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Array.Prim (

  mallocArray,
  useArray, useArrayAsync,
  indexArray,
  peekArray, peekArrayR, peekArrayAsync, peekArrayAsyncR,
  pokeArray, pokeArrayR, pokeArrayAsync, pokeArrayAsyncR,
  copyArrayPeer, copyArrayPeerR, copyArrayPeerAsync, copyArrayPeerAsyncR,
  devicePtr,

) where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Execute.Event
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream
import Data.Array.Accelerate.LLVM.PTX.Array.Table
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

-- CUDA
import qualified Foreign.CUDA.Driver                            as CUDA
import qualified Foreign.CUDA.Driver.Stream                     as CUDA

-- standard library
import Prelude                                                  hiding ( lookup )
import Control.Monad
import Control.Exception
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import Text.Printf


-- | Allocate a device-side array associated with the given host array. If the
-- allocation fails due to a memory error, we attempt some last-ditch memory
-- cleanup before trying again.
--
{-# INLINEABLE mallocArray #-}
mallocArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> Int
    -> ArrayData e
    -> IO (CUDA.DevicePtr a)
mallocArray !ctx !mt !n !ad = do
#ifdef ACCELERATE_INTERNAL_CHECKS
  exists <- member mt ad
  _      <- $internalCheck "mallocArray" "double malloc" (not exists) (return ())
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
    => Context
    -> MemoryTable
    -> Reservoir
    -> Int
    -> ArrayData e
    -> IO ()
useArray !ctx !mt !rsv !n !ad =
  blocking ctx rsv $ \st ->
    useArrayAsync ctx mt st n ad

{-# INLINEABLE useArrayAsync #-}
useArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> CUDA.Stream
    -> Int
    -> ArrayData e
    -> IO ()
useArrayAsync !ctx !mt !st !n !ad = do
  let !src      = CUDA.HostPtr (ptrsOfArrayData ad)
      !bytes    = n * sizeOf (undefined::a)
  exists        <- member mt ad
  unless exists $ do
    dst <- malloc ctx mt ad n
    transfer "useArray" bytes (Just st) $ CUDA.pokeArrayAsync n src dst (Just st)


-- | Copy data from the host to an existing array on the device
--
{-# INLINEABLE pokeArray #-}
pokeArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> Reservoir
    -> Int
    -> ArrayData e
    -> IO ()
pokeArray !ctx !mt !rsv !n !ad =
  blocking ctx rsv $ \st ->
    pokeArrayAsync ctx mt st n ad

{-# INLINEABLE pokeArrayAsync #-}
pokeArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> CUDA.Stream
    -> Int
    -> ArrayData e
    -> IO ()
pokeArrayAsync _ !mt !st !n !ad = do
  let !bytes    = n * sizeOf (undefined :: a)
      !src      = CUDA.HostPtr (ptrsOfArrayData ad)
  dst   <- devicePtr mt ad
  transfer "pokeArray" bytes (Just st) $ CUDA.pokeArrayAsync n src dst (Just st)

{-# INLINEABLE pokeArrayR #-}
pokeArrayR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> Reservoir
    -> Int
    -> Int
    -> ArrayData e
    -> IO ()
pokeArrayR !ctx !mt !rsv !from !to !ad =
  blocking ctx rsv $ \st ->
    pokeArrayAsyncR ctx mt st from to ad

{-# INLINEABLE pokeArrayAsyncR #-}
pokeArrayAsyncR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> CUDA.Stream
    -> Int
    -> Int
    -> ArrayData e
    -> IO ()
pokeArrayAsyncR _ !mt !st !from !to !ad = do
  let !n        = to - from
      !bytes    = n    * sizeOf (undefined :: a)
      !offset   = from * sizeOf (undefined :: a)
      !src      = CUDA.HostPtr (ptrsOfArrayData ad)
  dst   <- devicePtr mt ad
  transfer "pokeArray" bytes (Just st) $
    CUDA.pokeArrayAsync n (src `CUDA.plusHostPtr` offset) (dst `CUDA.plusDevPtr` offset) (Just st)


-- | Read a single element from an array at a given row-major index
--
{-# INLINEABLE indexArray #-}
indexArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> Reservoir
    -> ArrayData e
    -> Int
    -> IO a
indexArray !ctx !mt !rsv !ad !i =
  blocking ctx rsv                                  $ \st  ->
  bracket (CUDA.mallocHostArray [] 1) CUDA.freeHost $ \dst -> do
    src   <- devicePtr mt ad
    message $ "indexArray: " ++ showBytes (sizeOf (undefined::a))
    CUDA.peekArrayAsync 1 (src `CUDA.advanceDevPtr` i) dst (Just st)
    peek (CUDA.useHostPtr dst)


-- | Copy data from the device into the associated host-side Accelerate array
--
{-# INLINEABLE peekArray #-}
peekArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> Reservoir
    -> Int
    -> ArrayData e
    -> IO ()
peekArray !ctx !mt !rsv !n !ad =
  blocking ctx rsv $ \st ->
    peekArrayAsync ctx mt st n ad

{-# INLINEABLE peekArrayAsync #-}
peekArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> CUDA.Stream
    -> Int
    -> ArrayData e
    -> IO ()
peekArrayAsync _ !mt !st !n !ad = do
  let !bytes    = n * sizeOf (undefined :: a)
      !dst      = CUDA.HostPtr (ptrsOfArrayData ad)
  src   <- devicePtr mt ad
  transfer "peekArray" bytes (Just st) $ CUDA.peekArrayAsync n src dst (Just st)

{-# INLINEABLE peekArrayR #-}
peekArrayR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> Reservoir
    -> Int
    -> Int
    -> ArrayData e
    -> IO ()
peekArrayR !ctx !mt !rsv !from !to !ad =
  blocking ctx rsv $ \st ->
    peekArrayAsyncR ctx mt st from to ad

{-# INLINEABLE peekArrayAsyncR #-}
peekArrayAsyncR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context
    -> MemoryTable
    -> CUDA.Stream
    -> Int
    -> Int
    -> ArrayData e
    -> IO ()
peekArrayAsyncR _ !mt !st !from !to !ad = do
  let !n        = to - from
      !bytes    = n    * sizeOf (undefined :: a)
      !offset   = from * sizeOf (undefined :: a)
      !dst      = CUDA.HostPtr (ptrsOfArrayData ad)
  src   <- devicePtr mt ad
  transfer "peekArray" bytes (Just st) $
    CUDA.peekArrayAsync n (src `CUDA.plusDevPtr` offset) (dst `CUDA.plusHostPtr` offset) (Just st)


-- | Copy data from one device context into a _new_ array on the second context.
-- It is an error if the destination array already exists.
--
{-# INLINEABLE copyArrayPeer #-}
copyArrayPeer
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context -> MemoryTable -> Reservoir            -- source context
    -> Context -> MemoryTable                         -- destination context
    -> Int
    -> ArrayData e
    -> IO ()
copyArrayPeer !ctx1 !mt1 !rsv1 !ctx2 !mt2 !n !ad =
  blocking ctx1 rsv1 $ \st ->
    copyArrayPeerAsync ctx1 mt1 ctx2 mt2 st n ad

{-# INLINEABLE copyArrayPeerAsync #-}
copyArrayPeerAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context -> MemoryTable             -- source context
    -> Context -> MemoryTable             -- destination context
    -> CUDA.Stream
    -> Int
    -> ArrayData e
    -> IO ()
copyArrayPeerAsync !ctx1 !mt1 !ctx2 !mt2 !st !n !ad = do
  let !bytes    = n * sizeOf (undefined :: a)
  src   <- devicePtr mt1 ad
  dst   <- mallocArray ctx2 mt2 n ad
  transfer "copyArrayPeer" bytes (Just st) $
    CUDA.copyArrayPeerAsync n src (deviceContext ctx1) dst (deviceContext ctx2) (Just st)

-- | Copy part of an array from one device context to another. Both source and
-- destination arrays must exist.
--
{-# INLINEABLE copyArrayPeerR #-}
copyArrayPeerR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context -> MemoryTable -> Reservoir            -- source context
    -> Context -> MemoryTable                         -- destination context
    -> Int
    -> Int
    -> ArrayData e
    -> IO ()
copyArrayPeerR !ctx1 !mt1 !rsv1 !ctx2 !mt2 !from !to !ad =
  blocking ctx1 rsv1 $ \st ->
    copyArrayPeerAsyncR ctx1 mt1 ctx2 mt2 st from to ad

{-# INLINEABLE copyArrayPeerAsyncR #-}
copyArrayPeerAsyncR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context -> MemoryTable             -- source context
    -> Context -> MemoryTable             -- destination context
    -> CUDA.Stream
    -> Int
    -> Int
    -> ArrayData e
    -> IO ()
copyArrayPeerAsyncR !ctx1 !mt1 !ctx2 !mt2 !st !from !to !ad = do
  let !n        = to - from
      !bytes    = n    * sizeOf (undefined :: a)
      !offset   = from * sizeOf (undefined :: a)
  src <- devicePtr mt1 ad       :: IO (CUDA.DevicePtr a)
  dst <- devicePtr mt2 ad       :: IO (CUDA.DevicePtr a)
  transfer "copyArrayPeer" bytes (Just st) $
    CUDA.copyArrayPeerAsync n (src `CUDA.plusDevPtr` offset) (deviceContext ctx1)
                              (dst `CUDA.plusDevPtr` offset) (deviceContext ctx2) (Just st)

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
    Nothing     -> $internalError "devicePtr" "lost device memory"


-- | Execute the given operation in a new stream, and wait for the operation to
-- complete before returning.
--
{-# INLINE blocking #-}
blocking
    :: Context
    -> Reservoir
    -> (Stream -> IO a)
    -> IO a
blocking !ctx !rsv !f =
  streaming ctx rsv f $ \e r -> do
    block e
    return r


-- Debug
-- -----

{-# INLINE showBytes #-}
showBytes :: Int -> String
showBytes x = Debug.showFFloatSIBase (Just 0) 1024 (fromIntegral x :: Double) "B"

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = Debug.traceIO Debug.dump_gc ("gc: " ++ msg) >> next

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

