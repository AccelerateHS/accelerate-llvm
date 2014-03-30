-- |
-- Module      : Data.Range.Range
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Range.Range
  where

import Prelude                                  hiding ( take, splitAt )
import GHC.Base                                 ( quotInt )


-- | A simple range data type. The intervals is inclusive.
--
data Range
  = SingletonRange {-# UNPACK #-} !Int
  | InclusiveRange {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Show)

singleton :: Int -> Range
singleton = SingletonRange

inclusive :: Int -> Int -> Range
inclusive u v
  | v < u       = error "invalid inclusive range"
  | u == v      = singleton u
  | otherwise   = InclusiveRange u v


-- | The number of elements defined by the range interval
--
size :: Range -> Int
size r =
  case r of
    SingletonRange _    -> 1
    InclusiveRange u v  -> v - u + 1

-- | Split an interval into two roughly equally sized ranges. If the interval is
-- odd then the first interval gets the extra element.
--
halve :: Range -> (Range, Maybe Range)
halve range =
  case range of
    SingletonRange _    -> (range, Nothing)
    InclusiveRange u v  ->
      let n = size range
          m = (n + 1) `quotInt` 2

          x = inclusive u (u+m-1)
          y = if n - m > 0
               then Just (inclusive m v)
               else Nothing
      in
      (x,y)


-- | Return the first @n@ elements of the range, or the range itself if @n >
-- size@
--
take :: Int -> Range -> Range
take n r =
  case r of
    SingletonRange{}    -> r
    InclusiveRange u v  -> inclusive u (u + (min (n-1) v))

-- | A tuple where the first element is the first @n@ elements of the range, and
-- the second is the remainder of the list (if any).
--
splitAt :: Int -> Range -> (Range, Maybe Range)
splitAt n r =
  case r of
    SingletonRange{}    -> (r, Nothing)
    InclusiveRange u v
      | size r <= n     -> (r, Nothing)
      | otherwise       -> ( inclusive u (u+n-1)
                           , Just $ inclusive (u+n) v)

