{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Debug
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Debug (

  module Data.Array.Accelerate.LLVM.Debug,
  module Data.Array.Accelerate.LLVM.Native.Debug,

) where

import Data.Array.Accelerate.LLVM.Debug

import Control.Monad.Trans
import Data.Label
import Data.Time.Clock
import System.CPUTime
import Text.Printf


{-# INLINE timed #-}
timed :: MonadIO m
      => (Flags :-> Bool)
      -> (Double -> Double -> String)
      -> m ()
      -> m ()
timed _f _msg action
#ifdef ACCELERATE_DEBUG
  | mode _f
  = do
        -- We will measure both wall clock as well as CPU time.
        wall0   <- liftIO getCurrentTime
        cpu0    <- liftIO getCPUTime

        -- Run the action in the main thread. For the native backend there is no
        -- need to time this asynchronously.
        action

        wall1   <- liftIO getCurrentTime
        cpu1    <- liftIO getCPUTime

        let wallTime = realToFrac (diffUTCTime wall1 wall0)
            cpuTime  = fromIntegral (cpu1 - cpu0) * 1E-12

        message _f (_msg wallTime cpuTime)

  | otherwise
#endif
  = action

{-# INLINE elapsed #-}
elapsed :: Double -> Double -> String
elapsed wallTime cpuTime =
  printf "wall: %s, cpu: %s, speedup: %.2f"
    (showFFloatSIBase (Just 3) 1000 wallTime "s")
    (showFFloatSIBase (Just 3) 1000 cpuTime  "s")
    (cpuTime / wallTime)

