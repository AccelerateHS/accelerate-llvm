{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Debug
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

import Control.Concurrent
import Data.Label
import Data.Time.Clock
import Text.Printf

import GHC.Float


-- | Execute an action and time the results. The second argument specifies how
-- to format the output string given elapsed GPU and CPU time respectively
--
timed
    :: (Flags :-> Bool)
    -> (Double -> Double -> String)
    -> Maybe Stream
    -> IO ()
    -> IO ()
{-# INLINE timed #-}
timed f msg =
  monitorProcTime (queryFlag f) (\t1 t2 -> traceIO f (msg t1 t2))

monitorProcTime
    :: IO Bool
    -> (Double -> Double -> IO ())
    -> Maybe Stream
    -> IO ()
    -> IO ()
{-# INLINE monitorProcTime #-}
#if ACCELERATE_DEBUG
monitorProcTime enabled display stream action = do
  yes <- enabled
  if yes
    then do
      gpuBegin  <- Event.create []
      gpuEnd    <- Event.create []
      wallBegin <- getCurrentTime
      Event.record gpuBegin stream
      action
      Event.record gpuEnd stream
      wallEnd   <- getCurrentTime

      -- Wait for the GPU to finish executing then display the timing execution
      -- message. Do this in a separate thread so that the remaining kernels can
      -- be queued asynchronously.
      --
      _         <- forkIO $ do
        Event.block gpuEnd
        diff    <- Event.elapsedTime gpuBegin gpuEnd
        let gpuTime  = float2Double $ diff * 1E-3       -- milliseconds
            wallTime = realToFrac (diffUTCTime wallEnd wallBegin)

        Event.destroy gpuBegin
        Event.destroy gpuEnd
        --
        display wallTime gpuTime
      --
      return ()

    else
      action
#else
monitorProcTime _ _ _ action = action
#endif

{-# INLINE elapsed #-}
elapsed :: Double -> Double -> String
elapsed wallTime gpuTime =
  printf "%s (wall), %s (gpu)"
    (showFFloatSIBase (Just 3) 1000 wallTime "s")
    (showFFloatSIBase (Just 3) 1000 gpuTime "s")

