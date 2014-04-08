{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Stream
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Stream (

  Stream, Reservoir, new, streaming,

) where

-- accelerate
import Data.Array.Accelerate.LLVM.PTX.Context                   ( Context(..) )
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Event   as Event
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

-- cuda
import Foreign.CUDA.Driver.Event                                ( Event(..) )
import Foreign.CUDA.Driver.Stream                               ( Stream(..) )
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
type Reservoir  = MVar (Seq Stream)


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
new :: Context -> IO Reservoir
new _ctx = newMVar ( Seq.empty )

{--
flush :: Context -> Reservoir -> IO ()
flush !Context{..} !ref = do
  mc <- deRefWeak weakContext
  case mc of
    Nothing     -> message "delete reservoir/dead context"
    Just ctx    -> do
      message "flush reservoir"
      withMVar ref $ \rsv ->
        let delete st = trace ("free " ++ show st) (Stream.destroy st)
        in  bracket_ (CUDA.push ctx) CUDA.pop (mapM_ delete (Seq.toList rsv))
--}

-- | Create a CUDA execution stream. If an inactive stream is available for use,
-- use that, otherwise generate a fresh stream.
--
{-# INLINEABLE create #-}
create :: Context -> Reservoir -> IO Stream
create _ctx !ref = modifyMVar ref $ \rsv ->
  case Seq.viewl rsv of
    s Seq.:< ss -> do
      message ("reuse " ++ showStream s)
      return (ss, s)

    Seq.EmptyL  -> do
      s <- Stream.create []
      message ("new " ++ showStream s)
      return (Seq.empty, s)


-- | Merge a stream back into the reservoir. This must only be done once all
-- pending operations in the stream have completed.
--
{-# INLINEABLE destroy #-}
destroy :: Context -> Reservoir -> Stream -> IO ()
destroy _ctx !ref !stream = do
  message ("stash stream " ++ showStream stream)

  -- Wait for all preceding operations submitted to the stream to complete.
  --
  -- Note: Placing the block here seems to force _all_ streams to block, or at
  --       least significantly impacts the chances of other streams overlapping.
  --       However by using the 'streaming' interface, we avoid the need to
  --       block the stream here, because the binding will always be evaluated
  --       before the body begins execution.
  --
  -- Stream.block stream

  -- Note: We use a sequence here so that the old stream can be placed onto the
  --       end of the reservoir, while new streams are popped from the front.
  --       Since we don't block on the stream when returning it (see above),
  --       in this configuration most likely the stream won't be required
  --       immediately, and so it has some time yet to finish any outstanding
  --       operations (just in case).
  --
  modifyMVar_ ref $ \rsv -> return (rsv Seq.|> stream)


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = do
  Debug.when Debug.verbose $ Debug.message Debug.dump_exec ("stream: " ++ msg)
  next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

{-# INLINE showStream #-}
showStream :: Stream -> String
showStream (Stream s) = show s

