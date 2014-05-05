-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.LBS
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.LBS
  where

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

