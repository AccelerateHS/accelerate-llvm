{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Async
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Async
  where

import Data.Array.Accelerate.LLVM.State


-- Asynchronous operations
-- -----------------------

-- | The result of a potentially parallel computation which will be available at
-- some point (presumably, in the future). This is essentially a write-once
-- IVar.
--
data AsyncR arch a = AsyncR !(EventR arch) !a

class Async arch where
  -- | Streams (i.e. threads) can execute concurrently with other streams, but
  -- operations within the same stream proceed sequentially.
  --
  type StreamR arch

  -- | An Event marks a point in the execution stream, possibly in the future.
  -- Since execution within a stream is sequential, events can be used to test
  -- the progress of a computation and synchronise between different streams.
  --
  type EventR arch

  -- | Create a new execution stream that can be used to track (potentially
  -- parallel) computations
  --
  fork        :: LLVM arch (StreamR arch)

  -- | Mark the given execution stream as closed. The stream may still be
  -- executing in the background, but no new work may be submitted to it.
  --
  join        :: StreamR arch -> LLVM arch ()

  -- | Generate a new event at the end of the given execution stream. It will be
  -- filled once all prior work submitted to the stream has completed.
  --
  checkpoint  :: StreamR arch -> LLVM arch (EventR arch)

  -- | Make all future work submitted to the given execution stream wait until
  -- the given event has passed. Typically the event is from a different
  -- execution stream, therefore this function is intended to enable
  -- non-blocking cross-stream coordination.
  --
  after       :: StreamR arch -> EventR arch -> LLVM arch ()

  -- | Block execution of the calling thread until the given event has been
  -- recorded.
  --
  block       :: EventR arch -> LLVM arch ()


-- | Wait for an asynchronous operation to complete, then return it.
--
get :: Async arch => AsyncR arch a -> LLVM arch a
get (AsyncR e a) = block e >> return a

-- | Execute the given operation asynchronously in a new execution stream.
--
async :: Async arch
      => (StreamR arch -> LLVM arch a)
      -> LLVM arch (AsyncR arch a)
async f = do
  s <- fork
  r <- f s
  e <- checkpoint s
  join s
  return $ AsyncR e r

