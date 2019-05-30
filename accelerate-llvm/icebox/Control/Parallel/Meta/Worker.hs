{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Control.Parallel.Meta.Worker
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Control.Parallel.Meta.Worker (

  Gang, Workers, Worker(..), Req(..),
  gangIO, forkGang, forkGangOn, workerIO, exhausted,

) where

-- accelerate
import Data.Range
import qualified Data.Array.Accelerate.Debug                    as Debug

-- standard library
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector                                              ( Vector )
import System.IO.Unsafe
import System.Random.MWC                                        ( GenIO, createSystemRandom )
import Text.Printf
import Prelude
import qualified Data.Vector                                    as V

import Data.Concurrent.Deque.Class
#ifdef CHASELEV_DEQUE
import Data.Concurrent.Deque.ChaseLev.DequeInstance             ()
#else
import Data.Concurrent.Deque.Reference.DequeInstance            ()
#endif


-- | The 'Gang' structure tracks the state of all workers in the program. It
-- starts empty, and workers append to it as they are brought online. Although
-- the vector append operation is expensive, it is expected it is only called
-- occasionally; e.g. at program initialisation. So, we prioritise for constant
-- lookup of the worker structure, which will be done frequently during the work
-- search.
--
type Gang     = MVar Workers
type Workers  = Vector Worker


-- | The 'Worker' is the per-worker-thread state.
--
-- If the worker has work that can be stolen by other processors, it is stored
-- in the 'workpool'. Thieves treat the workpool as a stack which can be popped
-- on the right, where as the owner can both push and pop on the left.
--
-- In the lazy binary splitting work stealing setup, a worker processes its
-- range in chunks, checking the state of its workpool periodically. Whenever
-- the queue is empty, it splits it's current workload in two so that the second
-- half can be stolen by another processor.
--
data Worker = Worker {
    workerId            :: {-# UNPACK #-} !Int

  -- Coordinating with the host thread
  , requestVar          :: {-# UNPACK #-} !(MVar Req)
  , resultVar           :: {-# UNPACK #-} !(MVar ())

  -- Work scheduling
  , workpool            :: {-# UNPACK #-} !(WSDeque Range)
  , consecutiveFailures :: {-# UNPACK #-} !(MutVar RealWorld Int)
  , rngState            :: {-# UNPACK #-} !GenIO            -- TLM: don't unpack: too large?

  -- TODO: debug/work statistics
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
-- TLM: This isn't a bottleneck, but it would have been better to use something
-- like 'Data.Atomic' as in the base Accelerate package.
--
{-# NOINLINE uniqueSupply #-}
uniqueSupply :: MVar Int
uniqueSupply = unsafePerformIO $ newMVar 0

-- Generate  a fresh identifier. Note that the bang pattern is important.
freshId :: IO Int
freshId = modifyMVar uniqueSupply (\n -> let !n' = n+1 in return (n', n))


-- | Create a set of workers. This is a somewhat expensive function, so it is
-- expected that it is called only occasionally (e.g. once per program
-- execution).
--
forkGang :: Int -> IO Gang
forkGang n = forkGangOn [0..n-1]


-- | Create a set of workers on specific capabilities. Note that the thread ID
-- passed to the 'gangWorker' is the index of this worker in the gang structure,
-- not necessarily the capability is is spawned on.
--
forkGangOn :: [Int] -> IO Gang
forkGangOn caps = do
  ws <- V.forM (V.indexed (V.fromList caps)) $ \(i, cap) -> do
          worker <- Worker <$> freshId                -- identifier
                           <*> newEmptyMVar           -- work request
                           <*> newEmptyMVar           -- work complete
                           <*> newQ                   -- work stealing deque
                           <*> newMutVar 0            -- consecutive steal failure count
                           <*> createSystemRandom     -- random generator for stealing
          --
          message (printf "fork %d on capability %d" (workerId worker) cap)
          void $ mkWeakMVar (requestVar worker) (finaliseWorker worker)
          void $ forkOn cap $ gangWorker i worker
          return worker
  newMVar ws


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
        putMVar resultVar ()    -- signal that we got the shutdown order

    ReqDo action -> do
        action threadId         -- Run the action we were given
        putMVar resultVar ()    -- Signal that the action is complete
        gangWorker threadId st  -- Wait for more requests


-- | Gain control of the gang and use it to do some work
--
gangIO :: Gang -> (Workers -> IO ()) -> IO ()
gangIO = withMVar

-- | Issue work requests to the threads and wait until they complete
--
workerIO :: Workers -> (Int -> IO ()) -> IO ()
workerIO workers action = mask $ \restore -> do
  main  <- myThreadId

  -- Send requests to the threads
  V.forM_ workers $ \Worker{..} -> do
    writeMutVar consecutiveFailures 0
    putMVar requestVar $ ReqDo (reflectExceptionsTo main . restore . action)

  -- Wait for all requests to complete
  V.forM_ workers $ \Worker{..} -> takeMVar resultVar

reflectExceptionsTo :: ThreadId -> IO () -> IO ()
reflectExceptionsTo tid action =
  catchNonThreadKilled action (throwTo tid)

catchNonThreadKilled :: IO a -> (SomeException -> IO a) -> IO a
catchNonThreadKilled action handler =
  action `catch` \e ->
    case fromException e of
      Just ThreadKilled -> throwIO e
      _                 -> handler e


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
  message (printf "worker %d shutting down" workerId)
  putMVar requestVar ReqShutdown
  takeMVar resultVar


-- | Check whether the work queues of all workers in a gang are empty
--
exhausted :: Workers -> IO Bool
exhausted workers =
  V.and <$> V.mapM (\Worker{..} -> nullQ workpool) workers


-- Debugging
-- ---------

{-# INLINE message #-}
message :: String -> IO ()
message msg = Debug.traceIO Debug.dump_sched ("gang: " ++ msg)

