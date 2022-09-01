{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Divide
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Divide (

  divideWork, divideWork1

) where

import Data.Array.Accelerate.Representation.Shape

import Data.Bits
import Data.Sequence                                                ( Seq )
import qualified Data.Sequence                                      as Seq
import qualified Data.Vector.Unboxed                                as U
import qualified Data.Vector.Unboxed.Mutable                        as M


-- Divide the given multidimensional index range into a sequence of work pieces.
-- Splits will be made on the outermost (left-most) index preferentially, so
-- that spans are longest on the innermost dimension (because caches).
--
-- No dimension will be made smaller than the given minimum.
--
-- The number of subdivisions a hint (at most, it should generate a number of
-- pieces rounded up to the next power-of-two).
--
-- Full pieces will occur first in the resulting sequence, with smaller pieces
-- at the end (suitable for work-stealing). Note that the pieces are not sorted
-- according by size, and are ordered in the resulting sequence depending only
-- on whether all dimensions are above the minimum threshold or not. The integer
-- parameter to the apply action can be used to access the chunks linearly (for
-- example, this is useful when evaluating non-commutative operations).
--
-- {-# INLINABLE divideWork #-}
divideWork
    :: ShapeR sh
    -> Int                        -- #subdivisions (hint)
    -> Int                        -- minimum size of a dimension (must be a power of two)
    -> sh                         -- start index (e.g. top-left)
    -> sh                         -- end index   (e.g. bottom-right)
    -> (Int -> sh -> sh -> a)     -- action given start/end index range, and split number in the range [0..]
    -> Seq a
divideWork ShapeRz              = divideWork0
divideWork (ShapeRsnoc ShapeRz) = divideWork1
divideWork shr                  = divideWorkN shr
  --
  -- It is slightly faster to use lists instead of a Sequence here (though the
  -- difference is <1us on 'divideWork empty (Z:.2000) nop 8 32'). However,
  -- later operations will benefit from more efficient append, etc.

divideWork0 :: Int -> Int -> DIM0 -> DIM0 -> (Int -> DIM0 -> DIM0 -> a) -> Seq a
divideWork0 _ _ () () action = Seq.singleton (action 0 () ())

divideWork1 :: Int -> Int -> DIM1 -> DIM1 -> (Int -> DIM1 -> DIM1 -> a) -> Seq a
divideWork1 !n !minsize ((), (!from)) ((), (!to)) action =
  let
      split 0 !u !v !i !f !s
        | v - u < minsize = (i+1, f, s Seq.|> apply i u v)
        | otherwise       = (i+1, f Seq.|> apply i u v, s)
      --
      split !s !u !v !i0 !f0 !s0 =
        case findSplitPoint1 u v minsize of
          Nothing       -> (i0+1, f0, s0 Seq.|> apply i0 u v)
          Just (u', v') ->
            let s'         = unsafeShiftR s 1
                (i1,f1,s1) = split s' u  v' i0 f0 s0
                (i2,f2,s2) = split s' u' v  i1 f1 s1
            in
            (i2, f2, s2)

      apply i u v = action i ((), u) ((), v)
      (_, fs, ss) = split n from to 0 Seq.empty Seq.empty
  in
  fs Seq.>< ss

{-# INLINE findSplitPoint1 #-}
findSplitPoint1
    :: Int
    -> Int
    -> Int
    -> Maybe (Int, Int)
findSplitPoint1 !u !v !minsize =
  let a = v - u in
  if a <= minsize
    then Nothing
    else
      let b = unsafeShiftR (a+1) 1
          c = minsize - 1
          d = (b+c) .&. complement c
      in
      Just (d+u, v-a+d)


divideWorkN :: ShapeR sh -> Int -> Int -> sh -> sh -> (Int -> sh -> sh -> a) -> Seq a
divideWorkN !shr !n !minsize !from !to action =
  let
      -- Is it worth checking whether the piece is full? Doing so ensures that
      -- full pieces are assigned to threads first, with the non-full blocks
      -- being the ones at the end of the work queue to be stolen.
      --
      split 0 !u !v !i !f !s
        | U.any (< minsize) (U.zipWith (-) v u) = (i+1, f, s Seq.|> apply i u v)
        | otherwise                             = (i+1, f Seq.|> apply i u v, s)
      --
      split !s !u !v !i0 !f0 !s0 =
        case findSplitPointN u v minsize of
          Nothing       -> (i0+1, f0, s0 Seq.|> apply i0 u v)
          Just (u', v') ->
            let s'      = unsafeShiftR s 1
                (i1,f1,s1) = split s' u  v' i0 f0 s0
                (i2,f2,s2) = split s' u' v  i1 f1 s1
            in
            (i2, f2, s2)

      apply i u v = action i (vecToShape shr u) (vecToShape shr v)
      (_, fs, ss) = split n (shapeToVec shr from) (shapeToVec shr to) 0 Seq.empty Seq.empty
  in
  fs Seq.>< ss


-- Determine if and where to split the given index range. Returns new start and
-- end indices if found.
--
{-# INLINE findSplitPointN #-}
findSplitPointN
    :: U.Vector Int           -- start
    -> U.Vector Int           -- end
    -> Int                    -- minimum size of a dimension (must be power of 2)
    -> Maybe (U.Vector Int, U.Vector Int)
findSplitPointN !from !to !minsize =
  let
      mix = U.ifoldr' combine Nothing
          $ U.zipWith (-) to from

      combine i v old =
        if v <= minsize
          then old
          else case old of
                 Nothing    -> Just (i,v)
                 Just (_,u) -> if v < u
                                 then Just (i,v)
                                 else old
  in
  case mix of
    Nothing     -> Nothing
    Just (i,a)  ->
      let b     = unsafeShiftR (a+1) 1    -- divide by 2 (rounded up)
          c     = minsize - 1
          d     = (b+c) .&. complement c  -- round up to next multiple of chunk size
          e     = U.unsafeIndex from i
          f     = U.unsafeIndex to   i
          --
          from' = U.modify (\mv -> M.unsafeWrite mv i (d+e))   from
          to'   = U.modify (\mv -> M.unsafeWrite mv i (f-a+d)) to
      in
      Just (from', to')

{-# INLINE vecToShape #-}
vecToShape :: ShapeR sh -> U.Vector Int -> sh
vecToShape shr = listToShape shr . U.toList

{-# INLINE shapeToVec #-}
shapeToVec :: ShapeR sh -> sh -> U.Vector Int
shapeToVec shr sh = U.fromListN (rank shr) (shapeToList shr sh)

