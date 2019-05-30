{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Range
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Range
  where

-- accelerate
import Data.Array.Accelerate.Error

-- standard library
import Prelude                                          hiding ( take, splitAt )
import GHC.Base                                         ( quotInt )
import Text.Printf

import Data.Sequence                                    ( Seq )
import qualified Data.Sequence                          as Seq


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


-- | /O(1)/. The number of elements defined by the range interval
--
{-# INLINE size #-}
size :: Range -> Int
size range =
  case range of
    Empty       -> 0
    IE u v      -> v - u


-- | /O(1)/. Split an interval into two roughly equally sized ranges. If the interval is
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


-- | /O(1)/. Return the first @n@ elements of the range, or the range itself if
-- @n > size@.
--
{-# INLINE take #-}
take :: Int -> Range -> Range
take !n !_     | n <= 0 = Empty
take !n !range =
  case range of
    Empty  -> Empty
    IE u v -> IE u ((u+n) `min` v)


-- | /O(1)/. A tuple where the first element is the first @n@ elements of the range, and
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


-- | If the two ranges are adjacent, return one combined range. The ranges must
-- not be empty.
--
{-# INLINE merge #-}
merge :: Range -> Range -> Maybe Range
merge (IE u v) (IE x y)
  | v == x      = Just (IE u y)
  | otherwise   = Nothing
merge _ _       = $internalError "merge" "empty range encountered"


-- | /O(1)/. Add a new range to the end of the given sequence. We assume that
-- ranges are non-overlapping and non-empty. If the new range is adjacent to the
-- last range on the sequence, the ranges are appended.
--
{-# INLINEABLE append #-}
append :: Seq Range -> Range -> Seq Range
append rs Empty = rs
append rs next  =
  case Seq.viewr rs of
    Seq.EmptyR                          -> Seq.singleton next
    rs' Seq.:> prev
      | Just r <- merge prev next       -> rs' Seq.|> r
      | otherwise                       -> rs  Seq.|> next


-- | /O(n log n)/. Compress the given ranges into the fewest number of sections
-- as possible. The ranges must not be empty.
--
{-# INLINEABLE compress #-}
compress :: Seq Range -> Seq Range
compress = squash . Seq.unstableSortBy cmp
  where
    -- Compare by the lower bound. Assume ranges are non-overlapping.
    --
    cmp (IE u _) (IE v _) = compare u v
    cmp _        _        = $internalError "compress" "empty range encountered"

    -- Look at the first two elements, compress them if they are adjacent, and
    -- continue walking down the sequence doing the same. If we merge a range,
    -- be sure to continue attempting to merge that with subsequent ranges
    --
    squash rrs =
      case Seq.viewl rrs of
        Seq.EmptyL      -> Seq.empty
        r1 Seq.:< rs    -> case Seq.viewl rs of
                             Seq.EmptyL                     -> rrs
                             r2 Seq.:< rs'
                               | Just r12 <- merge r1 r2    -> squash $ r12 Seq.<| rs'
                               | otherwise                  ->          r1  Seq.<| squash rs

