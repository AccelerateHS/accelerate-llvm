{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Stream
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Stream (

  Stream, Reservoir, new, create, destroy, streaming,

) where

-- accelerate
import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.PTX.Context                   ( Context(..) )
import Data.Array.Accelerate.LLVM.PTX.Execute.Event             ( Event, Stream )
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Event   as Event
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

-- cuda
import qualified Foreign.CUDA.Driver.Stream                     as Stream

-- standard library
import Control.Concurrent.MVar
import Control.Monad.Trans
import Data.Sequence                                            ( Seq )
import qualified Data.Sequence                                  as Seq


-- Reservoir
-- ---------
--
-- The reservoir is a place to store CUDA execution streams that are currently
-- inactive. When a new stream is requested one is provided from the reservoir
-- if available, otherwise a fresh execution stream is created.
--
-- TLM: In a multi-threaded multi-device environment, the Reservoir might need
--      to be augmented to explicitly push/pop the surrounding any operation.
--
type Reservoir  = MVar (Seq Stream.Stream)


-- Executing operations in streams
-- -------------------------------

-- | Execute an operation in a unique execution stream. The (asynchronous)
-- result is passed to a second operation together with an event that will be
-- signalled once the operation is complete. The stream and event are released
-- after the second operation completes.
--
{-# INLINEABLE streaming #-}
streaming
    :: MonadIO m
    => Context
    -> Reservoir
    -> (Stream -> m a)
    -> (Event -> a -> m b)
    -> m b
streaming !ctx !rsv !action !after = do
  stream <- liftIO $ create ctx rsv
  first  <- action stream
  end    <- liftIO $ Event.waypoint stream
  final  <- after end first
  liftIO $ do destroy ctx rsv stream
              Event.destroy end
  return final


-- Primitive operations
-- --------------------

-- | Generate a new empty reservoir. It is not necessary to pre-populate it with
-- any streams because stream creation does not cause a device synchronisation.
--
-- Additionally, we do not need to finalise any of the streams. A reservoir is
-- tied to a specific execution context, so when the reservoir dies it is
-- because the PTX state and contained CUDA context have died, so there is
-- nothing more to do.
--
{-# INLINEABLE new #-}
new :: Context -> IO Reservoir
new _ctx = newMVar ( Seq.empty )

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
{-# INLINEABLE create #-}
create :: Context -> Reservoir -> IO Stream
create _ctx !ref = do
  s      <- create' ref
  stream <- newLifetime s
  addFinalizer stream (finaliser ref s)
  return stream

create' :: Reservoir -> IO Stream.Stream
create' !ref = do
  ms <- modifyMVar ref (search Seq.empty)
  case ms of
    Just s  -> return s
    Nothing -> Stream.create []
  where
    -- Search through the streams in the reservoir looking for the first
    -- inactive one. Optimistically adding the streams to the end of the
    -- reservoir as soon as we stop assigning new work to them (c.f. async), and
    -- just checking they have completed before reusing them, is quicker than
    -- having a finaliser thread block until completion before retiring them.
    --
    search !acc !rsv =
      case Seq.viewl rsv of
        Seq.EmptyL  -> return (acc, Nothing)
        s Seq.:< ss -> do
          done <- Stream.finished s
          case done of
            True  -> return (acc Seq.>< ss, Just s)
            False -> search (acc Seq.|> s) ss


-- | Merge a stream back into the reservoir. This must only be done once all
-- pending operations in the stream have completed.
--
{-# INLINEABLE destroy #-}
destroy :: Context -> Reservoir -> Stream -> IO ()
destroy _ctx _ref = finalize


-- | Finaliser to run when garbage collecting streams.
--
-- Note: [Finalising execution streams]
--
-- We don't actually ensure that the stream is complete before attempting to
-- return it to the reservoir for reuse. Doing so increases overhead of the LLVM
-- RTS due to 'forkIO', and consumes CPU time as 'Stream.block' busy-waits for
-- the stream to complete. It is quicker to optimistically return the streams to
-- the end of the reservoir immediately, and just check whether the stream is
-- done before reusing it.
--
-- > void . forkIO $ do
-- >   Stream.block stream
-- >   modifyMVar_ ref $ \rsv -> return (rsv Seq.|> stream)
--
{-# INLINEABLE finaliser #-}
finaliser :: Reservoir -> Stream.Stream -> IO ()
finaliser !ref !stream = do
  message ("stash stream " ++ showStream stream)
  modifyMVar_ ref $ \rsv -> return (rsv Seq.|> stream)


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = do
  Debug.when Debug.verbose $ Debug.traceIO Debug.dump_exec ("stream: " ++ msg)
  next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

{-# INLINE showStream #-}
showStream :: Stream.Stream -> String
showStream (Stream.Stream s) = show s

