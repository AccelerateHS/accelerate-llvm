{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Array.Remote
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Array.Remote (

  withRemote, malloc,

) where

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.PTX.Target
import {-# SOURCE #-} Data.Array.Accelerate.LLVM.PTX.Execute.Event
import {-# SOURCE #-} Data.Array.Accelerate.LLVM.PTX.Execute.Stream

import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Array.Data
import qualified Data.Array.Accelerate.Array.Remote                     as Remote
import qualified Data.Array.Accelerate.LLVM.PTX.Debug                   as Debug

import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Ptr                                       as CUDA
import qualified Foreign.CUDA.Driver                                    as CUDA
import qualified Foreign.CUDA.Driver.Stream                             as CUDA

import Control.Exception
import Control.Monad.State
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

import GHC.Base
import GHC.Int


-- Events signal once a computation has completed
--
instance Remote.Task (Maybe Event) where
  completed Nothing  = return True
  completed (Just e) = query e

instance Remote.RemoteMemory (LLVM PTX) where
  type RemotePtr (LLVM PTX) = CUDA.DevicePtr
  --
  mallocRemote n
    | n <= 0    = return (Just CUDA.nullDevPtr)
    | otherwise = liftIO $ do
        ep <- try (CUDA.mallocArray n)
        case ep of
          Right p                     -> do liftIO (Debug.didAllocateBytesRemote (i64 n))
                                            return (Just p)
          Left (ExitCode OutOfMemory) -> do return Nothing
          Left e                      -> do message ("malloc failed with error: " ++ show e)
                                            throwIO e

  peekRemote n src ad =
    let bytes = n * sizeOfPtr src
        dst   = CUDA.HostPtr (ptrsOfArrayData ad)
    in
    blocking            $ \stream ->
    withLifetime stream $ \st     -> do
      Debug.didCopyBytesFromRemote (i64 bytes)
      transfer "peekRemote" bytes (Just st) $ CUDA.peekArrayAsync n src dst (Just st)

  pokeRemote n dst ad =
    let bytes = n * sizeOfPtr dst
        src   = CUDA.HostPtr (ptrsOfArrayData ad)
    in
    blocking            $ \stream ->
    withLifetime stream $ \st     -> do
      Debug.didCopyBytesToRemote (i64 bytes)
      transfer "pokeRemote" bytes (Just st) $ CUDA.pokeArrayAsync n src dst (Just st)

  castRemotePtr        = CUDA.castDevPtr
  availableRemoteMem   = liftIO $ fst `fmap` CUDA.getMemInfo
  totalRemoteMem       = liftIO $ snd `fmap` CUDA.getMemInfo
  remoteAllocationSize = return 4096



-- | Allocate an array in the remote memory space sufficient to hold the given
-- number of elements, and associated with the given host side array. Space will
-- be freed from the remote device if necessary.
--
{-# INLINEABLE malloc #-}
malloc
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => ArrayData e
    -> Int
    -> Bool
    -> LLVM PTX Bool
malloc !ad !n !frozen = do
  PTX{..} <- gets llvmTarget
  Remote.malloc ptxMemoryTable ad frozen n


-- | Lookup up the remote array pointer for the given host-side array
--
{-# INLINEABLE withRemote #-}
withRemote
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => ArrayData e
    -> (CUDA.DevicePtr a -> LLVM PTX (Maybe Event, r))
    -> LLVM PTX (Maybe r)
withRemote !ad !f = do
  PTX{..} <- gets llvmTarget
  Remote.withRemote ptxMemoryTable ad f


-- Auxiliary
-- ---------

-- | Execute the given operation in a new stream, and wait for the operation to
-- complete before returning.
--
{-# INLINE blocking #-}
blocking :: (Stream -> IO a) -> LLVM PTX a
blocking !fun =
  streaming (liftIO . fun) $ \e r -> do
    liftIO $ block e
    return r

{-# INLINE sizeOfPtr #-}
sizeOfPtr :: forall a. Storable a => CUDA.DevicePtr a -> Int
sizeOfPtr _ = sizeOf (undefined :: a)

{-# INLINE i64 #-}
i64 :: Int -> Int64
i64 (I# i#) = I64# i#

{-# INLINE double #-}
double :: Int -> Double
double (I# i#) = D# (int2Double# i#)


-- Debugging
-- ---------

{-# INLINE showBytes #-}
showBytes :: Int -> String
showBytes x = Debug.showFFloatSIBase (Just 0) 1024 (double x) "B"

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = Debug.traceIO Debug.dump_gc ("gc: " ++ msg) >> next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

{-# INLINE transfer #-}
transfer :: String -> Int -> Maybe CUDA.Stream -> IO () -> IO ()
transfer name bytes stream action
  = let showRate x t      = Debug.showFFloatSIBase (Just 3) 1024 (double x / t) "B/s"
        msg wall cpu gpu  = printf "gc: %s: %s bytes @ %s, %s"
                              name
                              (showBytes bytes)
                              (showRate bytes wall)
                              (Debug.elapsed wall cpu gpu)
    in
    Debug.timed Debug.dump_gc msg stream action

