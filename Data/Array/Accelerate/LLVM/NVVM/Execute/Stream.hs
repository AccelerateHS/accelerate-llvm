{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Execute.Stream
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Execute.Stream (

  Stream, Reservoir, new, streaming,

) where

-- accelerate
import qualified Data.Array.Accelerate.LLVM.NVVM.Execute.Event  as Event
import qualified Data.Array.Accelerate.LLVM.Debug               as Debug

-- cuda
import Foreign.CUDA.Driver.Event                                ( Event(..) )
import Foreign.CUDA.Driver.Stream                               ( Stream(..) )
import qualified Foreign.CUDA.Driver                            as CUDA
import qualified Foreign.CUDA.Driver.Stream                     as Stream

-- standard library
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Trans
import Data.Sequence                                            ( Seq )
import qualified Data.Foldable                                  as Seq ( toList )
import qualified Data.Sequence                                  as Seq

import GHC.Base
import GHC.Ptr


-- Reservoir
-- ---------
--
-- The reservoir is a place to store CUDA execution streams that are currently
-- inactive. When a new stream is requested one is provided from the reservoir
-- if available, otherwise a fresh execution stream is created.
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
    => CUDA.Context
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
new :: CUDA.Context -> IO Reservoir
new !ctx = do
  ref    <- newMVar ( Seq.empty )
  _      <- mkWeakMVar ref (flush ctx ref)
  return ref

flush :: CUDA.Context -> Reservoir -> IO ()
flush !ctx !ref =
  withMVar ref $ \rsv ->
    trace "flush reservoir" $
      let delete st = trace ("free " ++ show st) (Stream.destroy st)
      in  bracket_ (CUDA.push ctx) CUDA.pop (mapM_ delete (Seq.toList rsv))


-- | Create a CUDA execution stream. If an inactive stream is available for use,
-- use that, otherwise generate a fresh stream.
--
{-# INLINEABLE create #-}
create :: CUDA.Context -> Reservoir -> IO Stream
create _ctx !ref = modifyMVar ref $ \rsv ->
  case Seq.viewl rsv of
    s Seq.:< ss -> return (ss, s)
    Seq.EmptyL  -> do   s <- Stream.create []
                        message ("new " ++ showStream s)
                        return (Seq.empty, s)


-- | Merge a stream back into the reservoir. This must only be done once all
-- pending operations in the stream have completed.
--
{-# INLINEABLE destroy #-}
destroy :: CUDA.Context -> Reservoir -> Stream -> IO ()
destroy _ctx !ref !stream = do
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
#ifdef ACCELERATE_DEBUG
  Debug.when Debug.verbose $ Debug.message Debug.dump_exec ("stream: " ++ msg)
#endif
  next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

{-# INLINE showStream #-}
showStream :: Stream -> String
showStream (Stream (Ptr a#)) =
  let !i = I# (addr2Int# a#)
      !n = 128  -- XXX: dodgy hack
  in
  '#' : show (i `quotInt` n + 1)

