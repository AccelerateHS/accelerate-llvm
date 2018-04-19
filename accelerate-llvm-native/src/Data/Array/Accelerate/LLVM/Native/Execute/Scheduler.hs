{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Scheduler
-- Copyright   : [2018] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Scheduler (

  Action, Task(..), Job(..), Workers,

  schedule, divideWork,
  hireWorkers, hireWorkersOn, retireWorkers, fireWorkers, numWorkers,

) where

import Data.Array.Accelerate.LLVM.Native.Execute.Divide
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as D

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Concurrent.Queue.MichaelScott
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
  { jobTasks  :: !(Seq Action)    -- actions required to complete this job
  , jobDone   :: !(Maybe Action)  -- execute after the last action is completed
  }

data Workers = Workers
  { workerCount       :: {-# UNPACK #-} !Int    -- number of worker threads (length workerThreadIds)
  , workerThreadIds   :: ![ThreadId]            -- to send signals to / kill
  , workerActive      :: !(MVar ())             -- fill to signal to the threads to wake up
  , workerTaskQueue   :: !(LinkedQueue Task)    -- tasks currently being executed; may be from different jobs
  , workerException   :: !(MVar (Seq (ThreadId, SomeException)))  -- XXX: what should we do with these?
  }


-- Schedule a job to be executed by the worker threads. May be called
-- concurrently.
--
schedule :: Workers -> Job -> IO ()
schedule Workers{..} Job{..} = do
  -- Generate the work list. If there is a finalisation action, there is a bit
  -- of extra work to do at each step.
  --
  tasks <- case jobDone of
             Nothing    -> return $ fmap Work jobTasks
             Just done  -> do
                -- The thread which finishes the last task runs the finalisation
                -- action, so keep track of how many items have been completed.
                --
                count <- newIORef (Seq.length jobTasks)
                return $ flip fmap jobTasks $ \io -> Work $ do
                  _result   <- io
                  remaining <- atomicModifyIORef' count (\i -> let i' = i-1 in (i',i'))
                  when (remaining == 0) done

  -- Submit the tasks to the queue to be executed by the worker threads.
  --
  -- The signal MVar is filled to indicate to workers that new tasks are
  -- available. We signal twice so that threads can start work immediately, but
  -- since this is racey we signal again after adding all items to the queue,
  -- just in case a thread woke up and failed too many times before being able
  -- to successfully pop an item from the queue.
  --
  _ <- tryPutMVar workerActive ()
  mapM_ (pushL workerTaskQueue) tasks
  _ <- tryPutMVar workerActive ()
  return ()


-- Workers can either be executing tasks (real work), waiting for work, or going
-- into retirement (whence the thread will exit).
--
-- So that threads don't spin endlessly on an empty queue waiting for work, they
-- will automatically sleep waiting on the signal MVar after several failed
-- retries. Note that 'readMVar' is multiple wake up, so all threads will be
-- efficiently woken up when new work is added via 'submit'.
--
runWorker :: MVar () -> LinkedQueue Task -> IO ()
runWorker activate queue = loop 0
  where
    loop :: Int -> IO ()
    loop !retries = do
      req <- tryPopR queue
      case req of
        -- The number of retries and thread delay on failure are knobs which can
        -- be tuned. Having these values too small results in busy work which
        -- will detract from time spent adding new work thus reducing
        -- productivity, whereas high values will reduce responsiveness and thus
        -- also reduce productivity.
        --
        -- TODO: Tune these values a bit
        --
        Nothing   -> if retries < 3
                       then threadDelay 5 >> loop (retries+1)
                       else do
                         D.when D.dump_sched $ do
                           tid <- myThreadId
                           message $ printf "sched: %s sleeping" (show tid)
                         --
                         _  <- tryTakeMVar activate -- another worker might have already signalled sleep
                         () <- readMVar activate    -- blocking, multiple wake-up
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
  workerActive    <- newEmptyMVar
  workerException <- newEmptyMVar
  workerTaskQueue <- newQ
  workerThreadIds <- forM caps $ \cpu -> do
                       tid <- mask_ $ forkOnWithUnmask cpu $ \restore ->
                                catch
                                  (restore $ runWorker workerActive workerTaskQueue)
                                  (\e -> do tid <- myThreadId
                                            appendMVar workerException (tid, e))
                       --
                       message $ printf "sched: fork %s on capability %d" (show tid) cpu
                       return tid
  --
  workerThreadIds `deepseq` return Workers { workerCount = length workerThreadIds, ..}


-- Submit a job telling every worker to retire. Currently pending tasks will be
-- completed first.
--
retireWorkers :: Workers -> IO ()
retireWorkers Workers{..} = do
  mapM_ (pushL workerTaskQueue) (Seq.replicate workerCount Retire)
  void $ tryPutMVar workerActive ()

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

