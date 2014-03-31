-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.LBS
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.LBS
  where

-- accelerate
import Control.Parallel.Meta
import Control.Parallel.Meta.Worker
import qualified Control.Parallel.Meta.Trans.LBS                as LBS
import qualified Control.Parallel.Meta.Resource.SMP             as SMP
import qualified Control.Parallel.Meta.Resource.Backoff         as EB

import qualified Data.Array.Accelerate.LLVM.Native.Debug        as Debug

-- standard library
import Data.Monoid
import System.IO.Unsafe

import GHC.Conc                                                 ( numCapabilities )


-- | The globally shared thread gang is auto-initialised on startup and shared
-- by all computations.
--
-- In a data parallel setting, it does not help to have multiple gangs running
-- at the same time. This is because a single data parallel computation should
-- already be able to keep all threads busy. If we had multiple gangs running at
-- the same time, then the system as a whole would run slower as the gangs
-- contend for cache and thrash the scheduler.
--
{-# NOINLINE theGang #-}
theGang :: Gang
theGang
  = unsafePerformIO
  $ forkGang numCapabilities


-- Some default values for the profitable parallelism threshold (PPT). These are
-- chosen as to reduce the frequency of deque checks. Since a deque check also
-- requires returning from the foreign LLVM function back to the scheduler code,
-- it is important to combine fine-grained iterations via the PPT.
--
-- The large PPT is meant for operations such as @map@ and @generate@, where the
-- input length equates the total number of elements to process. The small PPT
-- is meant for operations such as multidimensional reduction, where each input
-- index corresponds to a non-unit amount of work.
--
defaultLargePPT :: Int
defaultLargePPT = 2048

defaultSmallPPT :: Int
defaultSmallPPT = 256


-- | Fill an array in parallel.
--
-- A lazy binary splitting work-stealing scheduler is used to balance the load
-- amongst the available processors. A suitable PPT should be chosen to balance
-- scheduler overhead with fine-grained function calls.
--
fillP :: Int    -- ^ Profitable parallelism threshold
      -> Int    -- ^ Total number of elements
      -> (Int -> Int -> Int -> IO ())
                -- ^ Function to execute. The first parameters are the start and
                -- end indices of the array this action should process, and the
                -- final is the ID of the thread.
      -> IO ()
fillP ppt len fill =
  let retries   = numCapabilities
      smp       = LBS.mkResource ppt $ SMP.mkResource retries theGang -- <> EB.mkResource
  in
  timed $ runParIO smp theGang len fill


{-# INLINE timed #-}
timed :: IO () -> IO ()
timed f = Debug.timed Debug.dump_exec elapsed f

{-# INLINE elapsed #-}
elapsed :: Double -> Double -> String
elapsed x y = "exec: " ++ Debug.elapsed x y

