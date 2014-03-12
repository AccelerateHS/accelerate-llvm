-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Execute.Event
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Execute.Event (

  Event, create, destroy, waypoint, after, block,

) where

-- accelerate
import qualified Data.Array.Accelerate.LLVM.NVVM.Debug          as Debug

-- cuda
import Foreign.CUDA.Driver.Event                                ( Event(..) )
import Foreign.CUDA.Driver.Stream                               ( Stream(..) )
import qualified Foreign.CUDA.Driver.Event                      as Event


-- | Create a new event. It will not be automatically garbage collected, and is
-- not suitable for timing purposes.
--
{-# INLINEABLE create #-}
create :: IO Event
create = do
  event <- Event.create [Event.DisableTiming]
  message ("create " ++ showEvent event)
  return event

-- | Delete an event
--
{-# INLINEABLE destroy #-}
destroy :: Event -> IO ()
destroy e = do
  message $ "destroy " ++ showEvent e
  Event.destroy e

-- | Create a new event marker that will be filled once execution in the
-- specified stream has completed all previously submitted work.
--
{-# INLINEABLE waypoint #-}
waypoint :: Stream -> IO Event
waypoint stream = do
  event <- create
  message $ "add waypoint " ++ showEvent event ++ " in stream " ++ showStream stream
  Event.record event (Just stream)
  return event

-- | Make all future work submitted to the given stream wait until the event
-- reports completion before beginning execution.
--
{-# INLINEABLE after #-}
after :: Event -> Stream -> IO ()
after event stream = do
  message $ "after " ++ showEvent event ++ " in stream " ++ showStream stream
  Event.wait event (Just stream) []

-- | Block the calling thread until the event is recorded
--
{-# INLINEABLE block #-}
block :: Event -> IO ()
block e = do
  message $ "blocked on event " ++ showEvent e
  Event.block e


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = do
  Debug.when Debug.verbose $ Debug.message Debug.dump_exec ("event: " ++ msg)
  next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

{-# INLINE showEvent #-}
showEvent :: Event -> String
showEvent (Event e) = show e

{-# INLINE showStream #-}
showStream :: Stream -> String
showStream (Stream s) = show s

