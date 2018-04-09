{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Divide
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Divide (

  divideWork,

) where

import Data.Array.Accelerate.LLVM.Analysis.Match
import Data.Array.Accelerate.Array.Sugar

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
-- at the end (suitable for work-stealing).
--
{-# SPECIALISE divideWork :: DIM0 -> DIM0 -> (DIM0 -> DIM0 -> a) -> Int -> Int -> (Seq a) #-}
{-# SPECIALISE divideWork :: DIM1 -> DIM1 -> (DIM1 -> DIM1 -> a) -> Int -> Int -> (Seq a) #-}
-- {-# SPECIALISE divideWork :: DIM2 -> DIM2 -> (DIM2 -> DIM2 -> a) -> Int -> Int -> (Seq a) #-}
-- {-# SPECIALISE divideWork :: DIM3 -> DIM3 -> (DIM3 -> DIM3 -> a) -> Int -> Int -> (Seq a) #-}
divideWork
    :: forall sh a. Shape sh
    => sh                   -- start index (e.g. top-left)
    -> sh                   -- end index   (e.g. bottom-right)
    -> (sh -> sh -> a)      -- action given start/end index range
    -> Int                  -- #subdivisions (hint)
    -> Int                  -- minimum size of a dimension (must be a power of two)
    -> Seq a
divideWork
  | Just Refl <- matchShapeType (undefined::DIM0) (undefined::sh) = divideWork0
  | Just Refl <- matchShapeType (undefined::DIM1) (undefined::sh) = divideWork1
  | otherwise                                                     = divideWorkN
  --
  -- It is slightly faster to use lists instead of a Sequence here (though the
  -- difference is <1us on 'divideWork empty (Z:.2000) nop 8 32'). However,
  -- later operations will benefit from more efficient append, etc.

divideWork0 :: DIM0 -> DIM0 -> (DIM0 -> DIM0 -> a) -> Int -> Int -> Seq a
divideWork0 Z Z action !_ !_ = Seq.singleton (action Z Z)

divideWork1 :: DIM1 -> DIM1 -> (DIM1 -> DIM1 -> a) -> Int -> Int -> Seq a
divideWork1 (Z :. from) (Z :. to) action !pieces !sz =
  let
      split 0  !u !v !f0 !s0
        | v - u < sz  = (f0, s0 Seq.|> apply u v)
        | otherwise   = (f0 Seq.|> apply u v, s0)
      --
      split !s !u !v !f0 !s0 =
        case findSplitPoint1 u v sz of
          Nothing       -> (f0, s0 Seq.|> apply u v)
          Just (u', v') ->
            let s'      = unsafeShiftR s 1
                (f1,s1) = split s' u  v' f0 s0
                (f2,s2) = split s' u' v  f1 s1
            in
            (f2, s2)

      apply u v = action (Z:.u) (Z:.v)
      (fs, ss)  = split pieces from to Seq.empty Seq.empty
  in
  fs Seq.>< ss

{-# INLINE findSplitPoint1 #-}
findSplitPoint1
    :: Int
    -> Int
    -> Int
    -> Maybe (Int, Int)
findSplitPoint1 !u !v !sz =
  let !a = v - u in
  if a <= sz
    then Nothing
    else
      let !b = unsafeShiftR (a+1) 1
          !c = sz - 1
          !d = (b+c) .&. complement c
      in
      Just (d+u, v-a+d)


divideWorkN :: Shape sh => sh -> sh -> (sh -> sh -> a) -> Int -> Int -> Seq a
divideWorkN from to action !pieces !sz =
  let
      -- Is it worth checking whether the piece is full? Doing so ensures that
      -- full pieces are assigned to threads first, with the non-full blocks
      -- being the ones at the end of the work queue to be stolen.
      --
      split 0  !u !v !f0 !s0
        | U.any (< sz) (U.zipWith (-) v u)  = (f0, s0 Seq.|> apply u v)
        | otherwise                         = (f0 Seq.|> apply u v, s0)
      --
      split !s !u !v !f0 !s0 =
        case findSplitPointN u v sz of
          Nothing       -> (f0, s0 Seq.|> apply u v)
          Just (u', v') ->
            let s'      = unsafeShiftR s 1
                (f1,s1) = split s' u  v' f0 s0
                (f2,s2) = split s' u' v  f1 s1
            in
            (f2, s2)

      apply u v   = action (vecToShape u) (vecToShape v)
      (fs, ss)    = split pieces (shapeToVec from) (shapeToVec to) Seq.empty Seq.empty
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
findSplitPointN !from !to !sz =
  let
      mix = U.ifoldr' combine Nothing
          $ U.zipWith (-) to from

      combine i v old =
        if v <= sz
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
          c     = sz - 1
          d     = (b+c) .&. complement c  -- round up to next multiple of chunk size
          e     = U.unsafeIndex from i
          f     = U.unsafeIndex to   i
          --
          from' = U.modify (\mv -> M.unsafeWrite mv i (d+e))   from
          to'   = U.modify (\mv -> M.unsafeWrite mv i (f-a+d)) to
      in
      Just (from', to')

{-# INLINE vecToShape #-}
vecToShape :: Shape sh => U.Vector Int -> sh
vecToShape = listToShape . U.toList

{-# INLINE shapeToVec #-}
shapeToVec :: Shape sh => sh -> U.Vector Int
shapeToVec sh = U.fromListN (rank sh) (shapeToList sh)

