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
import Data.IORef
import Data.Sequence                                                ( Seq, ViewL(..) )
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
  , workerJobQueue    :: !(MVar (Seq Job))      -- pending jobs
  , workerTaskQueue   :: !(MVar (Seq Task))     -- tasks currently being executed; may be from different jobs
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

-- Submit the tasks to the queue to be executed by the worker threads.
--
-- TODO: This is a single MVar shared by all worker threads, which means there
--       will be high contention for work stealing, especially at higher thread
--       counts. Use something like a chase-lev dequeue instead?
--
submit :: Workers -> Seq Task -> IO ()
submit Workers{..} tasks =
  mask_ $ do
    mq <- tryTakeMVar workerTaskQueue
    case mq of
      Nothing    -> putMVar workerTaskQueue tasks
      Just queue -> putMVar workerTaskQueue (queue Seq.>< tasks)


-- Workers can either be executing tasks (real work), waiting for work, or going
-- into retirement (whence the thread will exit).
--
runWorker :: MVar (Seq Task) -> IO ()
runWorker ref = do
  tasks <- takeMVar ref
  case Seq.viewl tasks of
    Work io :< rest -> putMVar ref rest >> io >> runWorker ref
    Retire  :< rest -> putMVar ref rest
    EmptyL          -> runWorker ref

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
  workerTaskQueue <- newEmptyMVar
  workerException <- newEmptyMVar
  workerThreadIds <- forM caps $ \cpu -> do
                       tid <- mask_ $ forkOnWithUnmask cpu $ \restore ->
                                catch
                                  (restore $ runWorker workerTaskQueue)
                                  (restore . putMVar workerException)   -- tryPutMVar?
                       message $ printf "fork %s on capability %d" (show tid) cpu
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

