-- |
-- Module      : Data.Range.Range
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover
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


-- | A simple range data type with inclusive ends.
--
data Range
  = Empty
  | Inclusive {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving Eq

instance Show Range where
  show Empty           = "empty"
  show (Inclusive u v) = printf "[%d...%d]" u v


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
singleton a = Inclusive a a

-- | An inclusive range
{-# INLINE (...) #-}
(...) :: Int -> Int -> Range
u ... v
  | u <= v      = Inclusive u v
  | otherwise   = Empty
infix 3 ...


-- | The number of elements defined by the range interval
--
{-# INLINE size #-}
size :: Range -> Int
size range =
  case range of
    Empty         -> 0
    Inclusive u v -> v - u + 1


-- | Split an interval into two roughly equally sized ranges. If the interval is
-- odd then the first interval gets the extra element.
--
{-# INLINE bisect #-}
bisect :: Range -> (Range, Range)
bisect range =
  case range of
    Empty         -> (empty, empty)
    Inclusive u v ->
      let n = size range
          m = (n + 1) `quotInt` 2

          x             = u   ... u+m-1
          y | n - m > 0 = u+m ... v
            | otherwise = empty
      in
      (x, y)


-- | Return the first @n@ elements of the range, or the range itself if
-- @n > size@.
--
{-# INLINE take #-}
take :: Int -> Range -> Range
take n range =
  case range of
    Empty         -> empty
    Inclusive u v -> u ... u + (min (n-1) v)


-- | A tuple where the first element is the first @n@ elements of the range, and
-- the second is the remainder of the list (if any).
--
{-# INLINE splitAt #-}
splitAt :: Int -> Range -> (Range, Range)
splitAt n range =
  case range of
    Empty               -> (empty, empty)
    Inclusive u v
      | size range <= n -> (range, empty)
      | otherwise       -> (u ... u+n-1, u+n ... v)

