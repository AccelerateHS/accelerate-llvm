-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Fill
-- Copyright   : [2014..2015] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Originally written by Ben Lippmeier <benl@ouroborus.net> for Repa
-- <https://github.com/DDCSF/repa>. Repa has a BSD3 license.
--

module Data.Array.Accelerate.LLVM.Native.Execute.Fill
  where

-- accelerate
import Data.Array.Accelerate.LLVM.Native.Execute.Gang
import qualified Data.Array.Accelerate.LLVM.Native.Debug                as Debug

-- standard library
import Control.Monad
import GHC.Base
import Text.Printf


-- Fill an array in parallel.
--
-- The array is split into linear equal sized chunks, and each chunk is filled
-- by its thread sequentially. The action is not called if the interval is
-- empty (start >= end).
--
fillP :: Int    -- ^ total number of elements
      -> (Int -> Int -> Int -> IO ())
                -- ^ function to execute. The first parameters are the start and
                -- end indices of the array that this thread should process, and
                -- the final is the ID of this thread.
      -> IO ()
fillP len fill
  = gangIO theGang
  $ \thread ->
        let start       = splitIx thread
            end         = splitIx (thread + 1)
        in
        when (start < end) $ do
          Debug.traceIO Debug.dump_sched (printf "gang/fillP: thread %d -> [%d,%d)" thread start end)
          fill start end thread
  where
    -- Decide now to split the work across the threads. If the length of the
    -- vector doesn't divide evenly among the threads, then the first few get an
    -- extra element.
    --
    -- TLM: Update calculations so that chunks align with the SIMD vector width.
    --
    workers             = gangSize theGang
    chunkLen            = len `quotInt` workers
    chunkLeftover       = len `remInt`  workers

    splitIx thread
      | thread < chunkLeftover
      = thread * (chunkLen + 1)

      | otherwise
      = thread * chunkLen + chunkLeftover

