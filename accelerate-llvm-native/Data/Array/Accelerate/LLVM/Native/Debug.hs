{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Debug
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Debug (

  module Data.Array.Accelerate.Debug,
  module Data.Array.Accelerate.LLVM.Native.Debug,

) where

import Data.Array.Accelerate.Debug

import Data.Label
import Data.Time.Clock
import System.CPUTime
import Text.Printf


-- | Execute an action and time the results.
timed :: (Flags :-> Bool)
      -> (Double -> Double -> String)
      -> IO a
      -> IO a
#ifdef ACCELERATE_DEBUG
{-# NOINLINE timed #-}
timed f msg action = do
  enabled <- queryFlag f
  if enabled
    then do
      -- We will measure both wall clock as well as CPU time.
      wall0     <- getCurrentTime
      cpu0      <- getCPUTime

      -- Run the action in the main thread. For the native backend there is no
      -- need to time this asynchronously.
      result    <- action

      wall1     <- getCurrentTime
      cpu1      <- getCPUTime

      let wallTime = realToFrac (diffUTCTime wall1 wall0)
          cpuTime  = fromIntegral (cpu1 - cpu0) * 1E-12

      traceIO f (msg wallTime cpuTime)
      return result

    else
      action
#else
{-# INLINE timed #-}
timed _ _ action = action
#endif

{-# INLINE elapsed #-}
elapsed :: Double -> Double -> String
elapsed wallTime cpuTime =
  printf "wall: %s, cpu: %s, speedup: %.2f"
    (showFFloatSIBase (Just 3) 1000 wallTime "s")
    (showFFloatSIBase (Just 3) 1000 cpuTime  "s")
    (cpuTime / wallTime)

