{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Array.Remote
-- Copyright   : [2014..2020] The Accelerate Team
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

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Remote                 as Remote
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Ptr                                   as CUDA
import qualified Foreign.CUDA.Driver                                as CUDA
import qualified Foreign.CUDA.Driver.Stream                         as CUDA

import Control.Exception
import Control.Monad.State
import Data.Text.Lazy.Builder
import Formatting                                                   hiding ( bytes )
import qualified Formatting                                         as F

import GHC.Base                                                     ( Int(..), Double(..), int2Double# )


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
    | otherwise = do
        name <- gets ptxDeviceName
        liftIO $ do
          ep <- try (CUDA.mallocArray n)
          case ep of
            Right p                     -> do Debug.remote_memory_alloc name (CUDA.useDevicePtr p) n
                                              return (Just p)
            Left (ExitCode OutOfMemory) -> do return Nothing
            Left e                      -> do message ("malloc failed with error: " % shown) e
                                              throwIO e

  peekRemote t n src ad
    | SingleArrayDict <- singleArrayDict t
    , SingleDict      <- singleDict t
    = let bytes = n * bytesElt (TupRsingle (SingleScalarType t))
          dst   = CUDA.HostPtr (unsafeUniqueArrayPtr ad)
      in
      blocking            $ \stream ->
      withLifetime stream $ \st     -> do
        Debug.memcpy_from_remote bytes
        transfer "peekRemote" bytes (Just st) $ CUDA.peekArrayAsync n src dst (Just st)

  pokeRemote t n dst ad
    | SingleArrayDict <- singleArrayDict t
    , SingleDict      <- singleDict t
    = let bytes = n * bytesElt (TupRsingle (SingleScalarType t))
          src   = CUDA.HostPtr (unsafeUniqueArrayPtr ad)
      in
      blocking            $ \stream ->
      withLifetime stream $ \st     -> do
        Debug.memcpy_to_remote bytes
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
    :: SingleType e
    -> ArrayData e
    -> Int
    -> Bool
    -> LLVM PTX Bool
malloc !tp !ad !n !frozen = do
  PTX{..} <- gets llvmTarget
  Remote.malloc ptxMemoryTable tp ad frozen n


-- | Lookup up the remote array pointer for the given host-side array
--
{-# INLINEABLE withRemote #-}
withRemote
    :: SingleType e
    -> ArrayData e
    -> (CUDA.DevicePtr (ScalarArrayDataR e) -> LLVM PTX (Maybe Event, r))
    -> LLVM PTX (Maybe r)
withRemote !tp !ad !f = do
  PTX{..} <- gets llvmTarget
  Remote.withRemote ptxMemoryTable tp ad f


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

{-# INLINE double #-}
double :: Int -> Double
double (I# i#) = D# (int2Double# i#)


-- Debugging
-- ---------

{-# INLINE message #-}
message :: Format (IO ()) a -> a
message fmt = Debug.traceM Debug.dump_gc ("gc: " % fmt)

{-# INLINE transfer #-}
transfer :: Builder -> Int -> Maybe CUDA.Stream -> IO () -> IO ()
transfer name bytes stream action =
  let fmt wall cpu gpu =
        message (builder % ": " % F.bytes @Double shortest % " @ " % Debug.formatSIBase (Just 3) 1024 % "B/s, " % Debug.elapsed)
          name bytes (double bytes / wall) wall cpu gpu
  in
  Debug.timed Debug.dump_gc fmt stream action

