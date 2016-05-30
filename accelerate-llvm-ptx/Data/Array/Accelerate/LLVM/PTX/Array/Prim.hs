{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
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
  withDevicePtr,

) where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Event
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream
import Data.Array.Accelerate.LLVM.PTX.Array.Table
import Data.Array.Accelerate.LLVM.PTX.Array.Remote              as Remote
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

-- CUDA
import qualified Foreign.CUDA.Driver                            as CUDA
import qualified Foreign.CUDA.Driver.Stream                     as CUDA

-- standard library
import Prelude                                                  hiding ( lookup )
import Control.Monad
import Control.Monad.State
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
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
    => Int
    -> ArrayData e
    -> LLVM PTX ()
mallocArray !n !ad = do
  message ("mallocArray: " ++ showBytes (n * sizeOf (undefined::a)))
  void $ malloc ad n False


-- | A combination of 'mallocArray' and 'pokeArray', that allocates remotes
-- memory and uploads an existing array. This is specialised because we tell the
-- allocator that the host-side array is frozen, and thus it is safe to evict
-- the remote memory and re-upload the data at any time.
--
{-# INLINEABLE useArray #-}
useArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
    => Int
    -> ArrayData e
    -> LLVM PTX ()
useArray !n !ad =
  blocking $ \st -> useArrayAsync st n ad

{-# INLINEABLE useArrayAsync #-}
useArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
    => Stream
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
useArrayAsync !st !n !ad = do
  alloc <- malloc ad n True
  when alloc $ pokeArrayAsync st n ad


-- | Copy data from the host to an existing array on the device
--
{-# INLINEABLE pokeArray #-}
pokeArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Storable a, Typeable a)
    => Int
    -> ArrayData e
    -> LLVM PTX ()
pokeArray !n !ad =
  blocking $ \st -> pokeArrayAsync st n ad

{-# INLINEABLE pokeArrayAsync #-}
pokeArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Storable a, Typeable a)
    => Stream
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
pokeArrayAsync !stream !n !ad = do
  let src      = CUDA.HostPtr (ptrsOfArrayData ad)
      bytes    = n * sizeOf (undefined :: a)
      st       = unsafeGetValue stream
  --
  withDevicePtr ad $ \dst ->
    nonblocking stream $
      transfer "pokeArray" bytes (Just st) $ CUDA.pokeArrayAsync n src dst (Just st)
  liftIO (touchLifetime stream)


{-# INLINEABLE pokeArrayR #-}
pokeArrayR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => Int
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
pokeArrayR !from !to !ad =
  blocking $ \st -> pokeArrayAsyncR st from to ad

{-# INLINEABLE pokeArrayAsyncR #-}
pokeArrayAsyncR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => Stream
    -> Int
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
pokeArrayAsyncR !stream !from !to !ad = do
  let !n        = to - from
      !bytes    = n    * sizeOf (undefined :: a)
      !offset   = from * sizeOf (undefined :: a)
      !src      = CUDA.HostPtr (ptrsOfArrayData ad)
      !st       = unsafeGetValue stream
  --
  withDevicePtr ad $ \dst ->
    nonblocking stream $
      transfer "pokeArray" bytes (Just st) $
        CUDA.pokeArrayAsync n (src `CUDA.plusHostPtr` offset) (dst `CUDA.plusDevPtr` offset) (Just st)
  liftIO (touchLifetime stream)


-- | Read a single element from an array at a given row-major index
--
{-# INLINEABLE indexArray #-}
indexArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => ArrayData e
    -> Int
    -> LLVM PTX a
indexArray !ad !i =
  blocking                                          $ \stream  ->
  withDevicePtr ad                                  $ \src     -> liftIO $
  bracket (CUDA.mallocHostArray [] 1) CUDA.freeHost $ \dst     -> do
    let !st = unsafeGetValue stream
    message $ "indexArray: " ++ showBytes (sizeOf (undefined::a))
    CUDA.peekArrayAsync 1 (src `CUDA.advanceDevPtr` i) dst (Just st)
    CUDA.block st
    touchLifetime stream
    r <- peek (CUDA.useHostPtr dst)
    return (Nothing, r)


-- | Copy data from the device into the associated host-side Accelerate array
--
{-# INLINEABLE peekArray #-}
peekArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => Int
    -> ArrayData e
    -> LLVM PTX ()
peekArray !n !ad =
  blocking $ \st -> peekArrayAsync st n ad

{-# INLINEABLE peekArrayAsync #-}
peekArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => Stream
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
peekArrayAsync !stream !n !ad = do
  let !bytes    = n * sizeOf (undefined :: a)
      !dst      = CUDA.HostPtr (ptrsOfArrayData ad)
      !st       = unsafeGetValue stream
  --
  withDevicePtr ad $ \src ->
    nonblocking stream $
      transfer "peekArray" bytes (Just st)  $ CUDA.peekArrayAsync n src dst (Just st)
  liftIO (touchLifetime stream)

{-# INLINEABLE peekArrayR #-}
peekArrayR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a, Typeable e, Storable a)
    => Int
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
peekArrayR !from !to !ad =
  blocking $ \st -> peekArrayAsyncR st from to ad

{-# INLINEABLE peekArrayAsyncR #-}
peekArrayAsyncR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => Stream
    -> Int
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
peekArrayAsyncR !stream !from !to !ad = do
  let !n        = to - from
      !bytes    = n    * sizeOf (undefined :: a)
      !offset   = from * sizeOf (undefined :: a)
      !dst      = CUDA.HostPtr (ptrsOfArrayData ad)
      !st       = unsafeGetValue stream
  --
  withDevicePtr ad     $ \src ->
    nonblocking stream $
      transfer "peekArray" bytes (Just st) $
        CUDA.peekArrayAsync n (src `CUDA.plusDevPtr` offset) (dst `CUDA.plusHostPtr` offset) (Just st)
  liftIO (touchLifetime stream)


-- | Copy data from one device context into a _new_ array on the second context.
-- It is an error if the destination array already exists.
--
{-# INLINEABLE copyArrayPeer #-}
copyArrayPeer
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context                            -- destination context
    -> MemoryTable                        -- destination memory table
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
copyArrayPeer !ctx2 !mt2 !n !ad =
  blocking $ \st -> copyArrayPeerAsync ctx2 mt2 st n ad

{-# INLINEABLE copyArrayPeerAsync #-}
copyArrayPeerAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context                            -- destination context
    -> MemoryTable                        -- destination memory table
    -> Stream
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
copyArrayPeerAsync = error "copyArrayPeerAsync"
{--
copyArrayPeerAsync !ctx2 !mt2 !st !n !ad = do
  let !bytes    = n * sizeOf (undefined :: a)
  src   <- devicePtr mt1 ad
  dst   <- mallocArray ctx2 mt2 n ad
  transfer "copyArrayPeer" bytes (Just st) $
    CUDA.copyArrayPeerAsync n src (deviceContext ctx1) dst (deviceContext ctx2) (Just st)
--}

-- | Copy part of an array from one device context to another. Both source and
-- destination arrays must exist.
--
{-# INLINEABLE copyArrayPeerR #-}
copyArrayPeerR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context                            -- destination context
    -> MemoryTable                        -- destination memory table
    -> Int
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
copyArrayPeerR !ctx2 !mt2 !from !to !ad =
  blocking $ \st -> copyArrayPeerAsyncR ctx2 mt2 st from to ad

{-# INLINEABLE copyArrayPeerAsyncR #-}
copyArrayPeerAsyncR
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context                            -- destination context
    -> MemoryTable                        -- destination memory table
    -> Stream
    -> Int
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
copyArrayPeerAsyncR = error "copyArrayPeerAsyncR"
{--
copyArrayPeerAsyncR !ctx2 !mt2 !st !from !to !ad = do
  let !n        = to - from
      !bytes    = n    * sizeOf (undefined :: a)
      !offset   = from * sizeOf (undefined :: a)
  src <- devicePtr mt1 ad       :: IO (CUDA.DevicePtr a)
  dst <- devicePtr mt2 ad       :: IO (CUDA.DevicePtr a)
  transfer "copyArrayPeer" bytes (Just st) $
    CUDA.copyArrayPeerAsync n (src `CUDA.plusDevPtr` offset) (deviceContext ctx1)
                              (dst `CUDA.plusDevPtr` offset) (deviceContext ctx2) (Just st)
--}

{--
-- | Lookup the device memory associated with a given host array
--
{-# INLINEABLE devicePtr #-}
devicePtr
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a, Typeable b)
    => ArrayData e
    -> LLVM PTX (CUDA.DevicePtr b)
devicePtr !ad = do
  undefined
  {--
  mv <- Table.lookup mt ad
  case mv of
    Just v      -> return v
    Nothing     -> $internalError "devicePtr" "lost device memory"
  --}
--}

-- Auxiliary
-- ---------

-- | Lookup the device memory associated with a given host array and do
-- something with it.
--
{-# INLINEABLE withDevicePtr #-}
withDevicePtr
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => ArrayData e
    -> (CUDA.DevicePtr a -> LLVM PTX (Maybe Event, r))
    -> LLVM PTX r
withDevicePtr !ad !f = do
  mr <- withRemote ad f
  case mr of
    Nothing -> $internalError "withDevicePtr" "array does not exist on the device"
    Just r  -> return r

-- | Execute the given operation in a new stream, and wait for the operation to
-- complete before returning.
--
{-# INLINE blocking #-}
blocking :: (Stream -> LLVM PTX a) -> LLVM PTX a
blocking !f = do
  PTX{..} <- gets llvmTarget
  streaming ptxContext ptxStreamReservoir f $ \e r -> do
    liftIO $ block e
    return r

-- | Execute a (presumable asynchronous) operation and return the result
-- together with an event recorded immediately afterwards in the given stream.
--
{-# INLINE nonblocking #-}
nonblocking :: Stream -> LLVM PTX a -> LLVM PTX (Maybe Event, a)
nonblocking !stream !f = do
  r <- f
  e <- liftIO $ waypoint stream
  return (Just e, r)


-- Debug
-- -----

{-# INLINE showBytes #-}
showBytes :: Int -> String
showBytes x = Debug.showFFloatSIBase (Just 0) 1024 (fromIntegral x :: Double) "B"

{-# INLINE trace #-}
trace :: MonadIO m => String -> m a -> m a
trace msg next = liftIO (Debug.traceIO Debug.dump_gc ("gc: " ++ msg)) >> next

{-# INLINE message #-}
message :: MonadIO m => String -> m ()
message s = s `trace` return ()

{-# INLINE transfer #-}
transfer :: MonadIO m => String -> Int -> Maybe CUDA.Stream -> IO () -> m ()
transfer name bytes stream action
  = let showRate x t         = Debug.showFFloatSIBase (Just 3) 1024 (fromIntegral x / t) "B/s"
        msg gpuTime wallTime = printf "gc: %s: %s bytes @ %s, %s"
                                  name
                                  (showBytes bytes)
                                  (showRate bytes wallTime)
                                  (Debug.elapsed gpuTime wallTime)
    in
    liftIO (Debug.timed Debug.dump_gc msg stream action)

