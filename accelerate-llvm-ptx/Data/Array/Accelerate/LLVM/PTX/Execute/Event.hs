-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Event
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Event (

  Event, Stream,
  create, destroy, query, waypoint, after, block,

) where

-- accelerate
import Data.Array.Accelerate.Lifetime
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

-- cuda
import qualified Foreign.CUDA.Driver.Event                      as Event
import qualified Foreign.CUDA.Driver.Stream                     as Stream


type Event  = Lifetime Event.Event
type Stream = Lifetime Stream.Stream


-- | Create a new event. It will not be automatically garbage collected, and is
-- not suitable for timing purposes.
--
{-# INLINEABLE create #-}
create :: IO Event
create = do
  e     <- Event.create [Event.DisableTiming]
  event <- newLifetime e
  message ("create " ++ showEvent e)
  addFinalizer event $ do message $ "destroy " ++ showEvent e
                          Event.destroy e
  return event

-- | Delete an event
--
{-# INLINEABLE destroy #-}
destroy :: Event -> IO ()
destroy = finalize

-- | Create a new event marker that will be filled once execution in the
-- specified stream has completed all previously submitted work.
--
{-# INLINEABLE waypoint #-}
waypoint :: Stream -> IO Event
waypoint stream = do
  event <- create
  withLifetime stream $ \s -> do
  withLifetime event  $ \e -> do
    message $ "add waypoint " ++ showEvent e ++ " in stream " ++ showStream s
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
    message $ "after " ++ showEvent e ++ " in stream " ++ showStream s
    Event.wait e (Just s) []

-- | Block the calling thread until the event is recorded
--
{-# INLINEABLE block #-}
block :: Event -> IO ()
block event =
  withLifetime event $ \e -> do
    message $ "blocked on event " ++ showEvent e
    Event.block e

-- | Test whether an event has completed
--
{-# INLINEABLE query #-}
query :: Event -> IO Bool
query event = withLifetime event Event.query


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = do
  Debug.when Debug.verbose $ Debug.traceIO Debug.dump_exec ("event: " ++ msg)
  next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

{-# INLINE showEvent #-}
showEvent :: Event.Event -> String
showEvent (Event.Event e) = show e

{-# INLINE showStream #-}
showStream :: Stream.Stream -> String
showStream (Stream.Stream s) = show s

