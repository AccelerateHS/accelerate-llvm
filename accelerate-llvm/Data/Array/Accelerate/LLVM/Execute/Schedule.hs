{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Schedule
-- Copyright   : [2016] Manuel M T Chakravarty, Robert Clifton-Everest,
--                      Gabriele Keller
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Data.Array.Accelerate.LLVM.Execute.Schedule (

  Schedule(index), chunked, nextChunked,

) where

import Data.Array.Accelerate.Debug
import System.IO.Unsafe                              ( unsafePerformIO )

data Change = Decrease | Maintain | Increase

-- We schedule a step of a sequence computations by keeping the time per
-- element of the previous step and trying to improve it. The time measurement
-- is unit-less but needs to be consistent.
--
data Schedule index = Schedule
  { index :: !index
  , prevCost :: !(Maybe (Double))
  , prevChange :: !Change
  }

chunked :: (Int, Int) -> Schedule (Int, Int)
chunked p = Schedule p Nothing Maintain

-- |Compute the next schedule given the previous schedule and the total time it
-- took to compute all the work in the schedule.
--
nextChunked :: Schedule (Int, Int) -> Double -> Schedule (Int, Int)
nextChunked Schedule{..} _
  | Just n <- fixedChunkSize
  , (s,n') <- index
  = Schedule (s+n',n) Nothing Maintain
nextChunked Schedule{..} time
  = case prevCost of
      Nothing | n == 1    -> Schedule (s+n,2*n) (Just timePerElement) Increase
              | otherwise -> Schedule (s+n,n) (Just timePerElement) Maintain
      Just c  ->
        let change = computeChange c
        in Schedule (s+n, applyChange change n) (Just timePerElement) change
  where
    (s,n) = index
    timePerElement = time / fromIntegral n

    applyChange :: Change -> Int  -> Int
    applyChange Increase = (2*)
    applyChange Decrease = (`div` 2)
    applyChange Maintain = id

    computeChange :: Double -> Change
    computeChange prev
      = let smoothed = timePerElement * smoothingFactor + prev * (1 - smoothingFactor)
            comp = compare' smoothed prev
        in case prevChange of
             Maintain -> afterMaintain comp
             Increase -> afterIncrease comp
             Decrease -> afterDecrease comp

    afterMaintain comp
      = case comp of
          -- The time taken is the same as before, keep on using this chunk size
          EQ -> trace dump_sched "maintain chunk size (1)" Maintain
          -- The current chunk took significantly longer to process than the
          -- last one. This implies that the average size of the elements has
          -- increased. Assuming that this is likely to be true for the next
          -- chunk as well, increase the number of elements in the chunk.
          GT -> trace dump_sched "decreasing chunk size (1)" Decrease
          -- The current chunk took significantly less time to process than the
          -- last one. Similar to above, the size of the elements has likely
          -- decreased, so we should process more elements next time.
          LT -> trace dump_sched "increasing chunk size (1)" Increase

    afterIncrease comp
      = case comp of
          -- We got no parallel speedup, keep on processing this many elements.
          EQ -> trace dump_sched "maintain chunk size (2)" Maintain
          -- We're not seeing any stability. Keep on increasing the size till
          -- it stabilises.
          _ -> trace dump_sched "increasing chunk size (2)" Increase

    afterDecrease comp
      = case comp of
          -- After decreasing our processing rate, we have not slowed down
          -- significantly, keep the rate the same.
          EQ -> trace dump_sched "maintain chunk size (3)" Maintain
          -- We've seen a speedup. This very likely means the element size has
          -- decreased. We should increase the rate.
          LT -> trace dump_sched "increasing chunk size (3)" Increase
          -- After reducing the rate, we find that it has slowed down
          -- significantly. The element size must be getting larger, keep on
          -- reducing the rate till it stabilises.
          GT -> trace dump_sched "decreasing chunk size (3)" Decrease

    compare' :: Double -> Double -> Ordering
    compare' u v
      | isInfinite u  = EQ          -- Close small time differences
      | isInfinite v  = EQ
      | isNaN u       = timingError
      | isNaN v       = timingError
      | abs u > abs v = if abs ((u-v) / u) < epsilon then EQ else GT
      | otherwise     = if abs ((v-u) / v) < epsilon then EQ else LT

    epsilon :: Double
    epsilon = 0.05

    smoothingFactor = 0.8

    timingError = error "Impossible time measurements"

{-# NOINLINE fixedChunkSize #-}
fixedChunkSize :: Maybe Int
fixedChunkSize = unsafePerformIO $ queryFlag seq_chunk_size

