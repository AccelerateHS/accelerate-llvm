{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Debug
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Debug (

  module Data.Array.Accelerate.Debug,
  module Data.Array.Accelerate.LLVM.PTX.Debug,

) where

import Data.Array.Accelerate.Debug                      hiding ( timed, elapsed )

import Foreign.CUDA.Driver.Stream                       ( Stream )
import qualified Foreign.CUDA.Driver.Event              as Event

import Control.Monad.Trans
import Control.Concurrent
import System.CPUTime
import Text.Printf

import GHC.Float


-- | Execute an action and time the results. The second argument specifies how
-- to format the output string given elapsed GPU and CPU time respectively
--
timed
    :: Flag
    -> (Double -> Double -> Double -> String)
    -> Maybe Stream
    -> IO ()
    -> IO ()
{-# INLINE timed #-}
timed f msg =
  monitorProcTime (getFlag f) (\t1 t2 t3 -> traceIO f (msg t1 t2 t3))

monitorProcTime
    :: MonadIO m
    => IO Bool
    -> (Double -> Double -> Double -> IO ())
    -> Maybe Stream
    -> m a
    -> m a
{-# INLINE monitorProcTime #-}
monitorProcTime enabled display stream action = do
  yes <- if debuggingIsEnabled then liftIO enabled else return False
  if yes
    then do
      gpuBegin  <- liftIO $ Event.create []
      gpuEnd    <- liftIO $ Event.create []
      wallBegin <- liftIO $ getMonotonicTime
      cpuBegin  <- liftIO $ getCPUTime
      _         <- liftIO $ Event.record gpuBegin stream
      result    <- action
      _         <- liftIO $ Event.record gpuEnd stream
      cpuEnd    <- liftIO $ getCPUTime
      wallEnd   <- liftIO $ getMonotonicTime

      -- Wait for the GPU to finish executing then display the timing execution
      -- message. Do this in a separate thread so that the remaining kernels can
      -- be queued asynchronously.
      --
      _         <- liftIO . forkIO $ do
        Event.block gpuEnd
        diff    <- Event.elapsedTime gpuBegin gpuEnd
        let gpuTime  = float2Double $ diff * 1E-3                   -- milliseconds
            cpuTime  = fromIntegral (cpuEnd - cpuBegin) * 1E-12     -- picoseconds
            wallTime = wallEnd - wallBegin                          -- seconds

        Event.destroy gpuBegin
        Event.destroy gpuEnd
        --
        display wallTime cpuTime gpuTime
      --
      return result

    else
      action


{-# INLINE elapsed #-}
elapsed :: Double -> Double -> Double -> String
elapsed wallTime cpuTime gpuTime =
  printf "%s (wall), %s (cpu), %s (gpu)"
    (showFFloatSIBase (Just 3) 1000 wallTime "s")
    (showFFloatSIBase (Just 3) 1000 cpuTime "s")
    (showFFloatSIBase (Just 3) 1000 gpuTime "s")

-- accelerate/cbits/clock.c
foreign import ccall unsafe "clock_gettime_monotonic_seconds" getMonotonicTime :: IO Double

