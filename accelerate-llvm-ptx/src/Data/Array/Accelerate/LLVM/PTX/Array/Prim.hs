{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnboxedTuples       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Array.Prim
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Array.Prim (

  mallocArray,
  useArrayAsync,
  indexArrayAsync,
  peekArrayAsync,
  pokeArrayAsync,
  copyArrayAsync,
  -- copyArrayPeerAsync,
  memsetArrayAsync,
  withDevicePtr,

) where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime                           hiding ( withLifetime )
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import Data.Array.Accelerate.LLVM.PTX.Execute.Event
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream
import Data.Array.Accelerate.LLVM.PTX.Array.Remote              as Remote
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

-- CUDA
import qualified Foreign.CUDA.Driver                            as CUDA
import qualified Foreign.CUDA.Driver.Stream                     as CUDA

-- standard library
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import GHC.Base
import GHC.TypeLits
import Text.Printf
import Prelude


-- | Allocate a device-side array associated with the given host array. If the
-- allocation fails due to a memory error, we attempt some last-ditch memory
-- cleanup before trying again. If it still fails; error.
--
{-# INLINEABLE mallocArray #-}
mallocArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
    => Int
    -> ArrayData e
    -> LLVM PTX ()
mallocArray !n !ad = do
  message ("mallocArray: " ++ showBytes (n * sizeOf (undefined::a)))
  void $ Remote.malloc ad n False


-- | A combination of 'mallocArray' and 'pokeArray', that allocates remotes
-- memory and uploads an existing array. This is specialised because we tell the
-- allocator that the host-side array is frozen, and thus it is safe to evict
-- the remote memory and re-upload the data at any time.
--
{-# INLINEABLE useArrayAsync #-}
useArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
    => Int
    -> ArrayData e
    -> Par PTX (Future (ArrayData e))
useArrayAsync !n !ad = do
  message ("useArrayAsync: " ++ showBytes (n * sizeOf (undefined::a)))
  alloc <- liftPar $ Remote.malloc ad n True
  if alloc
    then pokeArrayAsync n ad
    else newFull ad


-- | Copy data from the host to an existing array on the device
--
{-# INLINEABLE pokeArrayAsync #-}
pokeArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Storable a, Typeable a)
    => Int
    -> ArrayData e
    -> Par PTX (Future (ArrayData e))
pokeArrayAsync !n !ad = do
  let !src      = CUDA.HostPtr (ptrsOfArrayData ad)
      !bytes    = n * sizeOf (undefined :: a)
  --
  stream <- asks ptxStream
  result <- liftPar $
    withLifetime stream $ \st  ->
      withDevicePtr ad  $ \dst ->
        nonblocking stream $ do
          transfer "pokeArray" bytes (Just st) $ do
            CUDA.pokeArrayAsync n src dst (Just st)
            Debug.didCopyBytesToRemote (fromIntegral bytes)
          return ad
  --
  return result


-- | Read an element from an array at the given row-major index.
--
-- This copies the data via a temporary array on the host, so that packed AoS
-- elements can be copied in a single transfer.
--
{-# INLINEABLE indexArrayAsync #-}
indexArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => ArrayData e
    -> Int              -- element index
    -> Int              -- actual number of values per element (i.e. this is >1 for SIMD types)
    -> Par PTX (Future (ArrayData e))
indexArrayAsync !ad_src !i !n = do
  ad_dst <- liftIO $ newArrayData n
  let !bytes  = n * sizeOf (undefined::a)
      !dst    = CUDA.HostPtr (ptrsOfArrayData ad_dst)
  --
  stream <- asks ptxStream
  result <- liftPar $
    withLifetime stream  $ \st  ->
    withDevicePtr ad_src $ \src ->
      nonblocking stream $ do
        transfer "indexArray" bytes (Just st) $ do
          CUDA.peekArrayAsync n (src `CUDA.advanceDevPtr` (i*n)) dst (Just st)
          Debug.didCopyBytesFromRemote (fromIntegral bytes)
        return ad_dst
  --
  return result


-- | Copy data from the device into the associated host-side Accelerate array
--
{-# INLINEABLE peekArrayAsync #-}
peekArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => Int
    -> ArrayData e
    -> Par PTX (Future (ArrayData e))
peekArrayAsync !n !ad = do
  let !bytes    = n * sizeOf (undefined :: a)
      !dst      = CUDA.HostPtr (ptrsOfArrayData ad)
  --
  stream <- asks ptxStream
  result <- liftPar $
    withLifetime stream $ \st  ->
      withDevicePtr ad  $ \src ->
        nonblocking stream $ do
          transfer "peekArray" bytes (Just st) $ do
            CUDA.peekArrayAsync n src dst (Just st)
            Debug.didCopyBytesFromRemote (fromIntegral bytes)
          return ad
  --
  return result


-- | Copy data between arrays in the same context
--
{-# INLINEABLE copyArrayAsync #-}
copyArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Storable a, Typeable a)
    => Int
    -> ArrayData e
    -> ArrayData e
    -> Par PTX (Future (ArrayData e))
copyArrayAsync !n !ad_src !ad_dst = do
  let !bytes    = n * sizeOf (undefined :: a)
  --
  stream <- asks ptxStream
  result <- liftPar $
    withLifetime stream      $ \st ->
      withDevicePtr ad_src   $ \src ->
        withDevicePtr ad_dst $ \dst -> do
          (e,r) <- nonblocking stream $ do
                     transfer "copyArray" bytes (Just st) $ CUDA.copyArrayAsync n src dst (Just st)
                     return ad_dst
          return (e, (e,r))
  --
  return result


{--
-- | Copy data from one device context into a _new_ array on the second context.
-- It is an error if the destination array already exists.
--
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
{-# INLINEABLE copyArrayPeerAsyncR #-}
copyArrayPeerAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a)
    => Context                            -- destination context
    -> MemoryTable                        -- destination memory table
    -> Stream
    -> Int
    -> Int
    -> ArrayData e
    -> LLVM PTX ()
copyArrayPeerAsync = error "copyArrayPeerAsyncR"
{--
copyArrayPeerAsyncR !ctx2 !mt2 !st !from !n !ad = do
  let !bytes    = n    * sizeOf (undefined :: a)
      !offset   = from * sizeOf (undefined :: a)
  src <- devicePtr mt1 ad       :: IO (CUDA.DevicePtr a)
  dst <- devicePtr mt2 ad       :: IO (CUDA.DevicePtr a)
  transfer "copyArrayPeer" bytes (Just st) $
    CUDA.copyArrayPeerAsync n (src `CUDA.plusDevPtr` offset) (deviceContext ctx1)
                              (dst `CUDA.plusDevPtr` offset) (deviceContext ctx2) (Just st)
--}
--}

-- | Set elements of the array to the specified value. Only 8-, 16-, and 32-bit
-- values are supported.
--
{-# INLINEABLE memsetArrayAsync #-}
memsetArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a, BitSize a <= 32)
    => Int
    -> a
    -> ArrayData e
    -> Par PTX (Future (ArrayData e))
memsetArrayAsync !n !v !ad = do
  let !bytes = n * sizeOf (undefined :: a)
  --
  stream <- asks ptxStream
  result <- liftPar $
    withLifetime stream $ \st  ->
      withDevicePtr ad  $ \ptr ->
        nonblocking stream $ do
          transfer "memset" bytes (Just st) $ CUDA.memsetAsync ptr n v (Just st)
          return ad
  --
  return result


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

-- | Execute a (presumable asynchronous) operation and return the result
-- together with an event recorded immediately afterwards in the given stream.
--
{-# INLINE nonblocking #-}
nonblocking :: Stream -> LLVM PTX a -> LLVM PTX (Maybe Event, Future a)
nonblocking !stream !action = do
  result <- action
  event  <- waypoint stream
  ready  <- liftIO (query event)
  if ready
    then do
      future <- Future <$> liftIO (newIORef (Full result))
      return (Nothing, future)

    else do
      future <- Future <$> liftIO (newIORef (Pending event Nothing result))
      return (Just event, future)

{-# INLINE withLifetime #-}
withLifetime :: MonadIO m => Lifetime a -> (a -> m b) -> m b
withLifetime (Lifetime ref _ a) f = do
  r <- f a
  liftIO (touchIORef ref)
  return r

{-# INLINE touchIORef #-}
touchIORef :: IORef a -> IO ()
touchIORef r = IO $ \s -> case touch# r s of s' -> (# s', () #)


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
transfer name bytes stream action =
  let showRate x t      = Debug.showFFloatSIBase (Just 3) 1024 (fromIntegral x / t) "B/s"
      msg wall cpu gpu  = printf "gc: %s: %s @ %s, %s"
                            name
                            (showBytes bytes)
                            (showRate bytes wall)
                            (Debug.elapsed wall cpu gpu)
  in
  liftIO (Debug.timed Debug.dump_gc msg stream action)

