{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Stream
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Stream (

  Reservoir, new,
  Stream, create, destroy, streaming,

) where

import Data.Array.Accelerate.Lifetime
import qualified Data.Array.Accelerate.Array.Remote.LRU             as Remote

import Data.Array.Accelerate.LLVM.PTX.Array.Remote                  ( )
import Data.Array.Accelerate.LLVM.PTX.Execute.Event                 ( Event )
import Data.Array.Accelerate.LLVM.PTX.Target                        ( PTX(..) )
import Data.Array.Accelerate.LLVM.State
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Event       as Event
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream.Reservoir      as RSV

import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver.Stream                         as Stream

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Text.Lazy.Builder
import Formatting


-- | A 'Stream' represents an independent sequence of computations executed on
-- the GPU. Operations in different streams may be executed concurrently with
-- each other, but operations in the same stream can never overlap.
-- 'Data.Array.Accelerate.LLVM.PTX.Execute.Event.Event's can be used for
-- efficient cross-stream synchronisation.
--
type Stream = Lifetime Stream.Stream


-- Executing operations in streams
-- -------------------------------

-- | Execute an operation in a unique execution stream. The (asynchronous)
-- result is passed to a second operation together with an event that will be
-- signalled once the operation is complete. The stream and event are released
-- after the second operation completes.
--
{-# INLINEABLE streaming #-}
streaming
    :: (Stream -> LLVM PTX a)
    -> (Event -> a -> LLVM PTX b)
    -> LLVM PTX b
streaming !action !after = do
  stream  <- create
  first   <- action stream
  end     <- Event.waypoint stream
  final   <- after end first
  liftIO $ do
    destroy stream
    Event.destroy end
  return final


-- Primitive operations
-- --------------------

{--
-- | Delete all execution streams from the reservoir
--
{-# INLINEABLE flush #-}
flush :: Context -> Reservoir -> IO ()
flush !Context{..} !ref = do
  mc <- deRefWeak weakContext
  case mc of
    Nothing     -> message "delete reservoir/dead context"
    Just ctx    -> do
      message "flush reservoir"
      old <- swapMVar ref Seq.empty
      bracket_ (CUDA.push ctx) CUDA.pop $ Seq.mapM_ Stream.destroy old
--}


-- | Create a CUDA execution stream. If an inactive stream is available for use,
-- use that, otherwise generate a fresh stream.
--
-- Note: [Finalising execution streams]
--
-- We don't actually ensure that the stream has executed all of its operations
-- to completion before attempting to return it to the reservoir for reuse.
-- Doing so increases overhead of the LLVM RTS due to 'forkIO', and consumes CPU
-- time as 'Stream.block' busy-waits for the stream to complete. It is quicker
-- to optimistically return the streams to the end of the reservoir immediately,
-- and just check whether the stream is done before reusing it.
--
-- > void . forkIO $ do
-- >   Stream.block stream
-- >   modifyMVar_ ref $ \rsv -> return (rsv Seq.|> stream)
--
{-# INLINEABLE create #-}
create :: LLVM PTX Stream
create = do
  PTX{..} <- gets llvmTarget
  s       <- create'
  stream  <- liftIO $ newLifetime s
  liftIO $ addFinalizer stream (RSV.insert ptxStreamReservoir s)
  return stream

create' :: LLVM PTX Stream.Stream
create' = do
  PTX{..} <- gets llvmTarget
  ms      <- attempt "create/reservoir" (liftIO $ RSV.malloc ptxStreamReservoir)
             `orElse`
             attempt "create/new"       (liftIO . catchOOM $ Stream.create [])
             `orElse` do
               Remote.reclaim ptxMemoryTable
               liftIO $ do
                 message "create/new: failed (purging)"
                 catchOOM $ Stream.create []
  case ms of
    Just s  -> return s
    Nothing -> liftIO $ do
      message "create/new: failed (non-recoverable)"
      throwIO (ExitCode OutOfMemory)

  where
    catchOOM :: IO a -> IO (Maybe a)
    catchOOM it =
      liftM Just it `catch` \e -> case e of
                                    ExitCode OutOfMemory -> return Nothing
                                    _                    -> throwIO e

    attempt :: MonadIO m => Builder -> m (Maybe a) -> m (Maybe a)
    attempt msg ea = do
      ma <- ea
      case ma of
        Nothing -> return Nothing
        Just a  -> do message builder msg
                      return (Just a)

    orElse :: MonadIO m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
    orElse ea eb = do
      ma <- ea
      case ma of
        Just a  -> return (Just a)
        Nothing -> eb


-- | Merge a stream back into the reservoir. This must only be done once all
-- pending operations in the stream have completed.
--
{-# INLINEABLE destroy #-}
destroy :: Stream -> IO ()
destroy = finalize


-- Debug
-- -----

{-# INLINE message #-}
message :: MonadIO m => Format (m ()) a -> a
message fmt = Debug.traceM Debug.dump_sched ("stream: " % fmt)

