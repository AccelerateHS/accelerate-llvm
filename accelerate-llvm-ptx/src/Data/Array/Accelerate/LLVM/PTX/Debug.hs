{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Debug
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Debug (

  module Data.Array.Accelerate.Debug.Internal,
  module Data.Array.Accelerate.LLVM.PTX.Debug,

) where

import Data.Array.Accelerate.Debug.Internal                         hiding ( timed, elapsed )
import qualified Data.Array.Accelerate.Debug.Internal               as D

import Foreign.CUDA.Driver.Stream                                   ( Stream )
import qualified Foreign.CUDA.Driver.Event                          as Event

import Control.Concurrent
import Control.Monad.Trans
import Data.Text.Lazy.Builder
import Formatting
import System.CPUTime

import GHC.Float


-- | Execute an action and time the results. The second argument specifies how
-- to format the output string given elapsed GPU and CPU time respectively
--
timed
    :: MonadIO m
    => Flag
    -> (Double -> Double -> Double -> IO ())
    -> Maybe Stream
    -> m a
    -> m a
{-# INLINE timed #-}
timed f fmt =
  monitorProcTime (getFlag f) fmt

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
elapsed :: Format r (Double -> Double -> Double -> r)
elapsed = formatSIBase (Just 3) 1000 % "s (wall), "
        % formatSIBase (Just 3) 1000 % "s (cpu), "
        % formatSIBase (Just 3) 1000 % "s (gpu)"

-- accelerate/cbits/clock.c
foreign import ccall unsafe "clock_gettime_monotonic_seconds" getMonotonicTime :: IO Double

data Phase = Compile | Link | Execute

buildPhase :: Phase -> Builder
buildPhase = \case
  Compile -> "compile"
  Link    -> "link"
  Execute -> "execute"

phase :: MonadIO m => Phase -> m a -> m a
phase p = D.timed dump_phases (now ("phase " <> buildPhase p <> ": ") % D.elapsed)

