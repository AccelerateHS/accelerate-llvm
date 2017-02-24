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

  Schedule(..), sequential, sequentialChunked,
  doubleSizeChunked,

) where

import Data.Array.Accelerate.Debug
import System.IO.Unsafe                              ( unsafePerformIO )

-- How to schedule the next chunk of the sequence.
--
data Schedule index = Schedule
  { index :: !index
  , next  :: !(Double -> Schedule index)
  }

sequential :: Int -> Schedule Int
sequential i = Schedule i (const (f i))
  where
    f i' = Schedule (i'+1) (const (f (i'+1)))

sequentialChunked :: (Int, Int) -> Schedule (Int, Int)
sequentialChunked p@(i,_) = Schedule p (const (f i))
  where
    f i' = Schedule (i'+1,1) (const (f (i' + 1)))

doubleSizeChunked :: (Int, Int) -> Schedule (Int, Int)
doubleSizeChunked (s,n) = Schedule (s,n) (f s initLog initLog Nothing)
  where
    initLog = floor (logBase 2 (fromIntegral n) :: Double)

    f :: Int -> Int -> Int -> Maybe Double -> Double -> Schedule (Int, Int)
    f start logn logn' t t' =
      let logn'' = step logn logn' t t'
          start' = start + 2^(logn' `max` 0)
      in
      Schedule (start', 2^(logn'' `max` 0))
               (f start' logn' logn'' (Just t'))

    step :: Int -> Int -> Maybe Double -> Double -> Int
    step _ _ _ _
      | Just n <- fixedChunkSize
      = floor (logBase 2 (fromIntegral n) :: Double)
    step _ logn' Nothing _
      = logn' + 1
    step logn logn' (Just t) t'
      | logn == logn'
      = case compare' t' t of
          -- The time taken is the same as before, keep on using this chunk size
          EQ -> trace dump_sched "maintain chunk size (1)" logn'
          -- The current chunk took significantly longer to process than the
          -- last one. This implies that the average size of the elements has
          -- increased. Assuming that this is likely to be true for the next
          -- chunk as well, increase the number of elements in the chunk.
          GT -> trace dump_sched "decreasing chunk size (1)" (logn' - 1)
          -- The current chunk took significantly less time to process than the
          -- last one. Similar to above, the size of the elements has likely
          -- decreased, so we should process more elements next time.
          LT -> trace dump_sched "increasing chunk size (1)" (logn' + 1)
      | logn' > logn
      = case compare' t' (2*t) of
          -- We got no parallel speedup, keep on processing this many elements.
          EQ -> trace dump_sched "maintain chunk size (2)" logn'
          -- We got a parallel speedup, increasing our processing rate to see if
          -- it continues
          LT -> trace dump_sched "increasing chunk size (2)" (logn' + 1)
          -- Not only did we not get a parallel speedup, we actually got a
          -- significant slowdown. In such a case, we need to reduce our
          -- processing rate.
          GT -> trace dump_sched "decreasing chunk size (2)" (logn' - 1)
      | otherwise
      = case compare' (2*t') t of
          -- After decreasing our processing rate, we have not slowed down
          -- significantly, keep the rate the same.
          EQ -> trace dump_sched "maintain chunk size (3)" logn'
          -- We've seen a speedup. This very likely means the element size has
          -- decreased. We should increase the rate.
          LT -> trace dump_sched "increasing chunk size (3)" (logn' + 1)
          -- After reducing the rate, we find that it has slowed down
          -- significantly. The element size must be getting larger, keep on
          -- reducing the rate till it stabilises.
          GT -> trace dump_sched "decreasing chunk size (3)" (logn' - 1)

    compare' :: Double -> Double -> Ordering
    compare' u v
      | isInfinite u  = timingError
      | isInfinite v  = timingError
      | isNaN u       = timingError
      | isNaN v       = timingError
      | abs u > abs v = if abs ((u-v) / u) < epsilon then EQ else GT
      | otherwise     = if abs ((v-u) / v) < epsilon then EQ else LT

    epsilon :: Double
    epsilon = 0.05

    timingError = error "Impossible time measurements"

{-# NOINLINE fixedChunkSize #-}
fixedChunkSize :: Maybe Int
fixedChunkSize = unsafePerformIO $ queryFlag seq_chunk_size

