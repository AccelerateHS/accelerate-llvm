{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Control.Parallel.Meta.Worker
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Control.Parallel.Meta.Worker (

  Gang, Worker(..), Req(..),
  forkGang,

) where

-- accelerate
import qualified Data.Array.Accelerate.LLVM.Debug               as Debug

-- standard library
import Control.Applicative
import Control.Concurrent
import Data.IORef
import Data.Range.Range
import Data.Vector                                              ( Vector )
import System.Mem.Weak
import System.Random.MWC
import System.IO.Unsafe
import Text.Printf
import qualified Data.Vector                                    as V

import Data.Concurrent.Deque.Class
import Data.Concurrent.Deque.ChaseLev.DequeInstance             ()
-- import Data.Concurrent.Deque.Reference.DequeInstance            ()


-- | The 'Gang' structure tracks the state of all workers in the program. It
-- starts empty, and workers append to it as they are brought online. Although
-- the vector append operation is expensive, it is expected it is only called
-- occasionally; e.g. at program initialisation. So, we prioritise for constant
-- lookup of the worker structure, which will be done frequently during the work
-- search.
--
type Gang = Vector Worker


-- | The 'Worker' is the per-worker-thread state.
--
-- If the worker has work that can be stolen by other processors, it is stored
-- in the 'workpool'. In the lazy binary splitting scheduler implementation
--
--
-- this is store
-- in 'stealableWork'. A worker can take this range and leave the MVar empty.
-- The chunk this worker is currently processing is stored in 'localWork'. In
-- the lazy-binary-splitting scheduler implementation, as the stealable work is
-- depleted, the worker will split its local work to make more available for
-- other processors.
--
-- This is a simplification on work stealing queues because we are only
-- partitioning a single data-parallel operation: we do not spawn additional
-- parallel work. Thus, 'stealableWork' is a singleton, not a deque.
--
data Worker = Worker {
    workerId            :: {-# UNPACK #-} !Int
  , requestVar          :: {-# UNPACK #-} !(MVar Req)
  , resultVar           :: {-# UNPACK #-} !(MVar ())

  -- Work scheduling
  , workpool            :: {-# UNPACK #-} !(WSDeque Range)
  , consecutiveFailures :: {-# UNPACK #-} !(IORef Int)
  , rngState            :: {-# UNPACK #-} !GenIO        -- don't unpack: too large?
  }

instance Eq Worker where
  w1 == w2 = workerId w1 == workerId w2


-- | The 'Req' type encapsulates work requests for individual workers
--
data Req
  -- | Instruct the worker to run the given action
  = ReqDo (Int -> IO ())

  -- | Tell the worker to exit. The worker should signal that it received the
  -- request by writing its result var before exiting.
  | ReqShutdown


-- A global name supply. This is not strictly necessary, but useful for ensuring
-- that each worker thread has a unique identifier. We can't just use the
-- threadId the worker is spawned on, because we might have multiple work groups
-- (i.e. for CPUs and GPUs)
--
{-# NOINLINE uniqueSupply #-}
uniqueSupply :: IORef Int
uniqueSupply = unsafePerformIO $ newIORef 0

-- Generate  a fresh identifier. Note that the bang pattern is important.
freshId :: IO Int
freshId = atomicModifyIORef uniqueSupply (\n -> let !n' = n+1 in (n', n))


-- | Create a set of workers. This is a somewhat expensive function, so it is
-- expected that it is called only occasionally (e.g. once per program
-- execution).
--
forkGang :: Int -> IO Gang
forkGang n = do
  gang <- V.replicateM n
              $ Worker <$> freshId              -- identifier
                       <*> newEmptyMVar         -- work request
                       <*> newEmptyMVar         -- work complete
                       <*> newQ                 -- work stealing deque
                       <*> newIORef 0           -- consecutive steal failure count
                       <*> createSystemRandom   -- random generator for stealing

  V.forM_ (V.indexed gang)
          (\(tid,w) -> do message (printf "fork %d on capability %d" (workerId w) tid)
                          addFinalizer w (finaliseWorker w)
                          forkOn tid $ gangWorker tid w)
  return gang


-- | The main worker loop for a thread in the gang.
--
-- Threads block on the MVar waiting for work requests, until told to exit.
--
gangWorker :: Int -> Worker -> IO ()
gangWorker threadId st@Worker{..} = do

  -- Wait for a request
  req   <- takeMVar requestVar

  case req of
    ReqShutdown ->
        putMVar resultVar ()

    ReqDo action -> do
        action threadId         -- Run the action we were given
        putMVar resultVar ()    -- Signal that the action is complete
        gangWorker threadId st  -- Wait for more requests


-- | The finaliser for worker threads.
--
-- Without this programs can complain about "Blocked indefinitely on an MVar"
-- because worker threads are still blocked on the request MVars when the
-- program ends. Whether the finalizer is called or not is very racey.
--
-- We're relying on the comment in System.Mem.Weak that says:
--
--     "If there are no other threads to run, the runtime system will check for
--      runnable finalizers before declaring the system to be deadlocked."
--
-- If we were creating and destroying the gang cleanly we wouldn't need this,
-- but 'theGang' is created with a top-level unsafePerformIO. Hacks beget hacks
-- beget hacks...
--
finaliseWorker :: Worker -> IO ()
finaliseWorker Worker{..} = do
  message (printf "%d shutting down" workerId)
  putMVar requestVar ReqShutdown
  takeMVar resultVar


-- Debugging
-- ---------

{-# INLINE message #-}
message :: String -> IO ()
message msg = Debug.message Debug.dump_sched ("gang: " ++ msg)

