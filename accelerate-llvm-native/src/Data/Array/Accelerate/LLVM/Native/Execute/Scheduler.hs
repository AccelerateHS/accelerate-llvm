{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Scheduler
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Scheduler (

  Action, Task(..), Job(..), Workers,

  schedule, divideWork,
  hireWorkers, hireWorkersOn, retireWorkers, fireWorkers,

) where

import Data.Array.Accelerate.LLVM.Native.Execute.Divide
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as D

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Concurrent.Deque.ChaseLev
import Data.IORef
import Data.Sequence                                                ( Seq )
import Text.Printf
import qualified Data.Sequence                                      as Seq


-- An individual computation is a job consisting of a sequence of actions to be
-- executed by the worker threads in parallel.
--
type Action = IO ()

data Task
  = Work Action
  | Retire

data Job = Job
  { jobTasks  :: !(Seq Action)  -- actions required to complete this job
  , jobDone   :: !Action        -- execute after the last action is completed
  }

data Workers = Workers
  { workerThreadIds   :: ![ThreadId]            -- to send signals to / kill
  , workerActive      :: !(MVar ())             -- fill to signal to the threads to wake up
  , workerJobQueue    :: !(MVar (Seq Job))      -- pending jobs
  , workerTaskQueue   :: !(ChaseLevDeque Task)  -- tasks currently being executed; may be from different jobs
  , workerException   :: !(MVar SomeException)  -- if full, worker received an exception
  }

-- instance NFData Task where
--   rnf Retire = ()
--   rnf Work{} = ()


-- Schedule a job to be executed by the workers. The thread which finishes the
-- last action in this job runs the final jobDone action.
--
schedule :: Workers -> Job -> IO ()
schedule workers Job{..} = do
  count <- newIORef (force $ Seq.length jobTasks)
  let
      tasks = flip fmap jobTasks $ \io -> Work $ do
                _result   <- io
                remaining <- atomicModifyIORef' count (\i -> let !i' = i-1 in (i', i'))
                when (remaining == 0) jobDone -- reschedule next job?
  --
  submit workers tasks


-- Submit the tasks to the queue to be executed by the worker threads. Note that
-- only the main thread (which owns the workers) may call this.
--
-- The signal MVar is filled to indicate to workers that new tasks are
-- available. We signal twice so that threads can start work immediately, but
-- since this is racey we signal again after adding all items to the queue, just
-- in case a thread woke up and failed too many times before being able to
-- successfully pop an item from the queue.
--
submit :: Workers -> Seq Task -> IO ()
submit Workers{..} tasks = do
  _ <- tryPutMVar workerActive ()
  mapM_ (pushL workerTaskQueue) tasks
  _ <- tryPutMVar workerActive ()
  return ()


-- Workers can either be executing tasks (real work), waiting for work, or going
-- into retirement (whence the thread will exit).
--
-- So that threads don't spin endlessly on an empty deque waiting for work, they
-- will automatically sleep waiting on the signal MVar after several failed
-- retries. Note that 'readMVar' is multiple wake up, so all threads will be
-- efficiently woken up when new work is added via 'submit'.
--
runWorker :: MVar () -> ChaseLevDeque Task -> IO ()
runWorker active deq = loop 0
  where
    loop :: Int -> IO ()
    loop !retries = do
      mio <- tryPopR deq
      case mio of
        -- The number of retries and thread delay on failure are knobs which can
        -- be tuned. Having these values too small results in busy work which
        -- will detract from time spent adding new work thus reducing
        -- productivity, whereas high values will reduce responsiveness and thus
        -- also reduce productivity.
        --
        Nothing   -> if retries < 3
                       then threadDelay 5 >> loop (retries+1)
                       else do
                         D.when D.dump_sched $ do
                           tid <- myThreadId
                           message $ printf "sched: %s sleeping" (show tid)
                         --
                         _  <- tryTakeMVar active -- another worker might have already signalled sleep
                         () <- readMVar active    -- blocking, multiple wake-up
                         loop 0
        --
        Just task -> case task of
                       Work io -> io >> loop 0
                       Retire  ->
                         D.when D.dump_sched $ do
                           tid <- myThreadId
                           message $ printf "sched: %s shutting down" (show tid)


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
  workerJobQueue  <- newEmptyMVar
  workerException <- newEmptyMVar
  workerActive    <- newEmptyMVar
  workerTaskQueue <- newQ
  workerThreadIds <- forM caps $ \cpu -> do
                       tid <- mask_ $ forkOnWithUnmask cpu $ \restore ->
                                catch
                                  (restore $ runWorker workerActive workerTaskQueue)
                                  (restore . putMVar workerException)   -- tryPutMVar?
                       --
                       message $ printf "sched: fork %s on capability %d" (show tid) cpu
                       return tid
  --
  workerThreadIds `deepseq` return Workers {..}


-- Submit a job telling every worker to retire. Currently pending tasks will be
-- completed first.
--
retireWorkers :: Workers -> IO ()
retireWorkers workers@Workers{..} =
  submit workers (Seq.replicate (length workerThreadIds) Retire)

-- Kill worker threads immediately
--
fireWorkers :: Workers -> IO ()
fireWorkers Workers{..} =
  mapM_ killThread workerThreadIds


-- Debug
-- -----

message :: String -> IO ()
message = D.traceIO D.dump_sched

