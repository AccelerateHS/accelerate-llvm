{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnboxedTuples       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Scheduler
-- Copyright   : [2018..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Scheduler (

  Action, Job(..), Workers,

  schedule,
  hireWorkers, hireWorkersOn, retireWorkers, fireWorkers, numWorkers,

) where

import qualified Data.Array.Accelerate.LLVM.Native.Debug            as D

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Concurrent.Queue.MichaelScott
import Data.IORef
import Data.Int
import Data.Sequence                                                ( Seq )
import Text.Printf
import qualified Data.Sequence                                      as Seq

import GHC.Base

#include "MachDeps.h"


-- An individual computation is a job consisting of a sequence of actions to be
-- executed by the worker threads in parallel.
--
type Action = IO ()

data Task
  = Work Action
  | Retire

data Job = Job
  { jobTasks  :: !(Seq Action)    -- actions required to complete this job
  , jobDone   :: !(Maybe Action)  -- execute after the last action is completed
  }

data Workers = Workers
  { workerCount       :: {-# UNPACK #-} !Int                      -- number of worker threads (length workerThreadIds)
  , workerActive      :: {-# UNPACK #-} !(IORef (MVar ()))        -- fill to signal to the threads to wake up
  , workerTaskQueue   :: {-# UNPACK #-} !(LinkedQueue Task)       -- tasks currently being executed; may be from different jobs
  , workerThreadIds   :: ![ThreadId]                              -- to send signals to / kill
  , workerException   :: !(MVar (Seq (ThreadId, SomeException)))  -- XXX: what should we do with these?
  }


-- Schedule a job to be executed by the worker threads. May be called
-- concurrently.
--
{-# INLINEABLE schedule #-}
schedule :: Workers -> Job -> IO ()
schedule workers Job{..} = do
  -- Generate the work list. If there is a finalisation action, there is a bit
  -- of extra work to do at each step.
  --
  tasks <- case jobDone of
             Nothing    -> return $ fmap Work jobTasks
             Just done  -> do
                -- The thread which finishes the last task runs the finalisation
                -- action, so keep track of how many items have been completed.
                --
                count <- newAtomic (Seq.length jobTasks)
                return $ flip fmap jobTasks $ \io -> Work $ do
                  _result   <- io
                  remaining <- fetchSubAtomic count -- returns old value
                  when (remaining == 1) done

  -- Submit the tasks to the queue to be executed by the worker threads.
  --
  pushTasks workers tasks


-- Workers can either be executing tasks (real work), waiting for work, or
-- going into retirement (whence the thread will exit).
--
-- So that threads don't spin endlessly on an empty queue waiting for work,
-- they will automatically sleep waiting on the signal MVar after several
-- failed retries. Note that 'readMVar' is multiple wake up, so all threads
-- will be efficiently woken up when new work is added via 'submit'.
--
-- The MVar is stored in an IORef. When scheduling new work, we resolve the
-- old MVar by putting a value in it, and we put a new, at that moment
-- unresolved, MVar in the IORef. If the queue is empty in runWorker, then
-- we will after some attempts wait on an MVar. It is essential that we
-- first get the MVar out of the IORef, before trying to read from the
-- queue. If this would have been done the other way around, we could have
-- a race condition, where new work is pushed after we tried to dequeue
-- work and before we wait on an MVar. We then wait on the new MVar, which
-- may cause that this thread stalls forever.
--
runWorker :: ThreadId -> IORef (MVar ()) -> LinkedQueue Task -> IO ()
runWorker tid ref queue = loop 0
  where
    loop :: Int16 -> IO ()
    loop !retries = do
      -- Extract the activation MVar from the IORef, before trying to claim
      -- an item from the work queue
      var <- readIORef ref
      req <- tryPopR queue
      case req of
        -- The number of retries and thread delay on failure are knobs which can
        -- be tuned. Having these values too high results in busy work which
        -- will detract from time spent adding new work thus reducing
        -- productivity, whereas low values will reduce responsiveness and thus
        -- also reduce productivity.
        --
        -- TODO: Tune these values a bit
        --
        Nothing   -> if retries < 16
                       then loop (retries+1)
                       else do
                         -- This thread will sleep, by waiting on the MVar (var) we previously
                         -- extracted from the IORef (ref)
                         --
                         -- When some other thread pushes new work, it will also write to that MVar
                         -- and this thread will wake up.
                         message $ printf "sched: %s sleeping" (show tid)

                         -- blocking, wake-up when new work is available
                         () <- readMVar var
                         loop 0
        --
        Just task -> case task of
                       Work io -> io >> loop 0
                       Retire  -> message $ printf "sched: %s shutting down" (show tid)


-- Spawn a new worker thread for each capability
--
hireWorkers :: IO Workers
hireWorkers = do
  ncpu    <- getNumCapabilities
  workers <- hireWorkersOn [0 .. ncpu-1]
  return workers

-- Spawn worker threads on the specified capabilities
--
hireWorkersOn :: [Int] -> IO Workers
hireWorkersOn caps = do
  active          <- newEmptyMVar
  workerActive    <- newIORef active
  workerException <- newEmptyMVar
  workerTaskQueue <- newQ
  workerThreadIds <- forM caps $ \cpu -> do
                       tid <- mask_ $ forkOnWithUnmask cpu $ \restore -> do
                                tid <- myThreadId
                                catch
                                  (restore $ runWorker tid workerActive workerTaskQueue)
                                  (appendMVar workerException . (tid,))
                       --
                       message $ printf "sched: fork %s on capability %d" (show tid) cpu
                       return tid
  --
  workerThreadIds `deepseq` return Workers { workerCount = length workerThreadIds, ..}


-- Submit a job telling every worker to retire. Currently pending tasks will be
-- completed first.
--
retireWorkers :: Workers -> IO ()
retireWorkers workers@Workers{..} =
  pushTasks workers (Seq.replicate workerCount Retire)


-- Pushes work to the task queue
--
-- Wakes up the worker threads if needed, by writing to the old MVar in
-- workerActive. We replace workerActive with a new, empty MVar, such that
-- we can wake them up later when we again have new work.
--
pushTasks :: Workers -> Seq Task -> IO ()
pushTasks Workers{..} tasks = do
  -- Push work to the queue
  mapM_ (pushL workerTaskQueue) tasks

  -- Create a new MVar, which we use in a later call to pushTasks to wake
  -- up the threads, then swap the MVar in the IORef workerActive, with the
  -- new MVar.
  --
  -- This must be atomic, to prevent race conditions when two threads are
  -- adding new work. Without the atomic, it may occur that some MVar is
  -- never resolved, causing that a worker thread which waits on that MVar
  -- to stall.
  new <- newEmptyMVar
  old <- atomicModifyIORef' workerActive (new,)

  -- Resolve the old MVar to wake up all waiting threads
  putMVar old ()


-- Kill worker threads immediately.
--
fireWorkers :: Workers -> IO ()
fireWorkers Workers{..} =
  mapM_ killThread workerThreadIds

-- Number of workers
--
numWorkers :: Workers -> Int
numWorkers = workerCount


-- Utility
-- -------

data Atomic = Atomic !(MutableByteArray# RealWorld)

{-# INLINE newAtomic #-}
newAtomic :: Int -> IO Atomic
newAtomic (I# n#) = IO $ \s0 ->
  case SIZEOF_HSINT                 of { I# size#       ->
  case newByteArray# size# s0       of { (# s1, mba# #) ->
  case writeIntArray# mba# 0# n# s1 of { s2             ->  -- non-atomic is ok
    (# s2, Atomic mba# #) }}}

{-# INLINE fetchSubAtomic #-}
fetchSubAtomic :: Atomic -> IO Int
fetchSubAtomic (Atomic mba#) = IO $ \s0 ->
  case fetchSubIntArray# mba# 0# 1# s0 of { (# s1, old# #) ->
    (# s1, I# old# #) }

{-# INLINE appendMVar #-}
appendMVar :: MVar (Seq a) -> a -> IO ()
appendMVar mvar a =
  mask_ $ do
    ma <- tryTakeMVar mvar
    case ma of
      Nothing -> putMVar mvar (Seq.singleton a)
      Just as -> putMVar mvar (as Seq.|> a)


-- Debug
-- -----

message :: String -> IO ()
message = D.traceIO D.dump_sched

