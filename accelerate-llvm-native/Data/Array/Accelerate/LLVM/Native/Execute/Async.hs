{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Async
-- Copyright   : [2014..2016] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Async (

  Async, Stream, Event,
  module Data.Array.Accelerate.LLVM.Execute.Async,

) where

-- accelerate
import Data.Array.Accelerate.LLVM.Execute.Async                     hiding ( Async )
import qualified Data.Array.Accelerate.LLVM.Execute.Async           as A

import Data.Array.Accelerate.LLVM.Native.State
import Data.Array.Accelerate.LLVM.Native.Target

import Control.Monad.Trans
import Data.Time.Clock
import System.CPUTime


type Async a = A.AsyncR  Native a
type Stream  = A.StreamR Native
type Event   = A.EventR  Native

-- The native backend does everything synchronously.
--
instance A.Async Native where
  type StreamR Native = ()
  type EventR  Native = ()

  {-# INLINE fork #-}
  fork = return ()

  {-# INLINE join #-}
  join () = return ()

  {-# INLINE checkpoint #-}
  checkpoint () = return ()

  {-# INLINE after #-}
  after () () = return ()

  {-# INLINE block #-}
  block () = return ()

  {-# INLINE timed #-}
  timed action = do
    wall0 <- liftIO getCurrentTime
    cpu0  <- liftIO getCPUTime
    res   <- action ()
    wall1 <- liftIO getCurrentTime
    cpu1  <- liftIO getCPUTime
    --
    let wallTime = realToFrac (diffUTCTime wall1 wall0)
        cpuTime  = fromIntegral (cpu1 - cpu0) * 1E-12
    --
    return (wallTime / cpuTime, res)

  {-# INLINE unsafeInterleave #-}
  unsafeInterleave = unsafeInterleaveNative

