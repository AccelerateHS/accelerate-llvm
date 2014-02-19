-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Fill
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Originally written by Ben Lippmeier <benl@ouroborus.net> for Repa
-- <https://github.com/DDCSF/repa>. Repa has a BSD3 license.
--

module Data.Array.Accelerate.LLVM.Native.Execute.Fill
  where

import Data.Array.Accelerate.LLVM.Native.Execute.Gang

import GHC.Base


-- Fill an array in parallel.
--
-- The array is split into linear equal sized chunks, and each chunk is filled
-- by its thread sequentially.
--
fillP :: Int    -- ^ total number of elements
      -> (Int -> Int -> IO ())
                -- ^ function to execute. The two parameters are the start and
                -- end indices of the array that this thread should process.
      -> IO ()
fillP len fill
  = gangIO theGang
  $ \thread ->
        let start       = splitIx thread
            end         = splitIx (thread + 1)
        in
        fill start end
  where
    -- Decide now to split the work across the threads. If the length of the
    -- vector doesn't divide evenly among the threads, then the first few get an
    -- extra element.
    workers             = gangSize theGang
    chunkLen            = len `quotInt` workers
    chunkLeftover       = len `remInt`  workers

    splitIx thread
      | thread < chunkLeftover
      = thread * (chunkLen + 1)

      | otherwise
      = thread * chunkLen + chunkLeftover

