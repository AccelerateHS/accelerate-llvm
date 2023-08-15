{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Event
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Event (

  Event,
  create, destroy, query, waypoint, after, block,

) where

import Data.Array.Accelerate.Lifetime
import qualified Data.Array.Accelerate.Array.Remote.LRU             as Remote

import Data.Array.Accelerate.LLVM.PTX.Array.Remote                  ( )
import Data.Array.Accelerate.LLVM.PTX.Target                        ( PTX(..) )
import Data.Array.Accelerate.LLVM.State
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug
import {-# SOURCE #-} Data.Array.Accelerate.LLVM.PTX.Execute.Stream

import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver.Event                          as Event
import qualified Foreign.CUDA.Driver.Stream                         as Stream

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Text.Lazy.Builder
import Formatting


-- | Events can be used for efficient device-side synchronisation between
-- execution streams and between the host.
--
type Event = Lifetime Event.Event


-- | Create a new event. It will not be automatically garbage collected, and is
-- not suitable for timing purposes.
--
{-# INLINEABLE create #-}
create :: LLVM PTX Event
create = do
  e     <- create'
  event <- liftIO $ newLifetime e
  liftIO $ addFinalizer event $ do message ("destroy " % formatEvent) e
                                   Event.destroy e
  return event

create' :: LLVM PTX Event.Event
create' = do
  PTX{ptxMemoryTable} <- gets llvmTarget
  me      <- attempt "create/new" (liftIO . catchOOM $ Event.create [Event.DisableTiming])
             `orElse` do
               Remote.reclaim ptxMemoryTable
               liftIO $ do
                 message "create/new: failed (purging)"
                 catchOOM $ Event.create [Event.DisableTiming]
  case me of
    Just e  -> return e
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


-- | Delete an event
--
{-# INLINEABLE destroy #-}
destroy :: Event -> IO ()
destroy = finalize

-- | Create a new event marker that will be filled once execution in the
-- specified stream has completed all previously submitted work.
--
{-# INLINEABLE waypoint #-}
waypoint :: Stream -> LLVM PTX Event
waypoint stream = do
  event <- create
  liftIO $
    withLifetime stream  $ \s -> do
      withLifetime event $ \e -> do
        message ("add waypoint " % formatEvent % " in stream " % formatStream) e s
        Event.record e (Just s)
        return event

-- | Make all future work submitted to the given stream wait until the event
-- reports completion before beginning execution.
--
{-# INLINEABLE after #-}
after :: Event -> Stream -> IO ()
after event stream =
  withLifetime stream $ \s ->
  withLifetime event  $ \e -> do
    message ("after " % formatEvent % " in stream " % formatStream) e s
    Event.wait e (Just s) []

-- | Block the calling thread until the event is recorded
--
{-# INLINEABLE block #-}
block :: Event -> IO ()
block event =
  withLifetime event $ \e -> do
    message ("blocked on event " % formatEvent) e
    Event.block e

-- | Test whether an event has completed
--
{-# INLINEABLE query #-}
query :: Event -> IO Bool
query event = withLifetime event Event.query


-- Debug
-- -----

{-# INLINE message #-}
message :: MonadIO m => Format (m ()) a -> a
message fmt = Debug.traceM Debug.dump_sched ("event: " % fmt)

{-# INLINE formatEvent #-}
formatEvent :: Format r (Event.Event -> r)
formatEvent = later $ \(Event.Event e) -> bformat shown e

{-# INLINE formatStream #-}
formatStream :: Format r (Stream.Stream -> r)
formatStream = later $ \(Stream.Stream s) -> bformat shown s

