{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- |
-- Module      : Data.Range.Range
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Range.Range
  where

import Prelude                                  hiding ( take, splitAt )
import Text.Printf
import GHC.Base                                 ( quotInt )


-- | A simple range data type
--
data Range
  = Empty               -- ^ The empty range
  | IE !Int !Int        -- ^ A range span with inclusive left, exclusive right
  deriving Eq

instance Show Range where
  show Empty    = "empty"
  show (IE u v)
    | u == pred v       = printf "singleton %d" u
    | otherwise         = printf "[%d...%d]" u (pred v) -- note display with inclusive ends


-- | An empty interval
{-# INLINE empty #-}
empty :: Range
empty = Empty

-- | Check if an interval is empty
--
{-# INLINE null #-}
null :: Range -> Bool
null Empty = True
null _     = False

-- | A singleton point
--
{-# INLINE singleton #-}
singleton :: Int -> Range
singleton !a = IE a (succ a)

-- | A range span with exclusive endpoint [u,v).
--
{-# INLINE (...) #-}
(...) :: Int -> Int -> Range
u ... v
  | u <= v      = IE u (succ v)
  | otherwise   = Empty
infix 3 ...


-- | The number of elements defined by the range interval
--
{-# INLINE size #-}
size :: Range -> Int
size range =
  case range of
    Empty       -> 0
    IE u v      -> v - u


-- | Split an interval into two roughly equally sized ranges. If the interval is
-- odd then the first interval gets the extra element.
--
{-# INLINE bisect #-}
bisect :: Range -> (Range, Range)
bisect range =
  case range of
    Empty  -> (Empty, Empty)
    IE u v ->
      let n = size range
          m = (n + 1) `quotInt` 2
          o = u+m

          x             = IE u o
          y | o < v     = IE   o v
            | otherwise = Empty
      in
      (x, y)


-- | Return the first @n@ elements of the range, or the range itself if
-- @n > size@.
--
{-# INLINE take #-}
take :: Int -> Range -> Range
take !n !_     | n <= 0 = Empty
take !n !range =
  case range of
    Empty  -> Empty
    IE u v -> IE u ((u+n) `min` v)


-- | A tuple where the first element is the first @n@ elements of the range, and
-- the second is the remainder of the list (if any).
--
{-# INLINE splitAt #-}
splitAt :: Int -> Range -> (Range, Range)
splitAt !n !range | n <= 0 = (Empty, range)
splitAt !n !range =
  case range of
    Empty  -> (Empty, Empty)
    IE u v ->
      let m = u+n
          x             = IE u (m `min` v)
          y | m < v     = IE m v
            | otherwise = Empty
      in
      (x, y)

