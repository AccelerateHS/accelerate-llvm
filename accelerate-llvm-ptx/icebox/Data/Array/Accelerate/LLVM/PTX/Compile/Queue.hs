{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Compile.Queue
-- Copyright   : [2014..2017] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Compile.Queue (

  enqueue

) where

import qualified Data.Array.Accelerate.LLVM.NVVM.Debug  as Debug

import Control.Exception
import Control.Concurrent
import Control.Concurrent.MSem                          ( MSem )
import Data.Time.Clock
import System.IO.Unsafe
import Text.Printf
import qualified Control.Concurrent.MSem                as Q

import GHC.Conc                                         ( getNumProcessors )


data Queue = Queue {
    queueThreads        :: {-# UNPACK #-} !Int          -- ^ number of worker threads
  , queueWorkers        :: {-# UNPACK #-} !(MSem Int)   -- ^ the worker threads
  }

instance Show Queue where
  showsPrec p q = showString "<<"
                . showsPrec p (queueThreads q)
                . showString " workers>>"


-- | This globally shared worker queue is auto-initialised on startup and shared
-- by all computations.
--
{-# NOINLINE theQueue #-}
theQueue :: Queue
theQueue
  = unsafePerformIO
  $ forkQueue =<< getNumProcessors


-- | Fork the worker queue with the given number of maximum simultaneous tasks
-- (at least 1).
--
forkQueue :: Int -> IO Queue
forkQueue n
  = assert (n > 0)
  $ do
        message ("creating compiler queue with " ++ show n ++ " workers")
        queue   <- Q.new n
        return  $! Queue n queue


-- | Run an action using the worker queue
--
withQueue :: IO a -> IO a
withQueue = Q.with (queueWorkers theQueue)

-- | Queue a process to be executed and return an MVar that will be filled once
-- the process completes. The task will only begin once there is a worker
-- available from the pool.
--
enqueue :: IO a -> IO (MVar a)
enqueue action = do
  resultVar <- newEmptyMVar
  _         <- forkIO $ do

    (workT, queueT) <- time . withQueue $ do    -- wait for a worker to become available
      (r, workT)    <- time action              -- Run the action...
      putMVar resultVar r                       -- ...and write the result into the MVar
      return workT

    message (printf "queue: %s, execute: %s"
      (Debug.showFFloatSIBase (Just 3) 1000 queueT "s")
      (Debug.showFFloatSIBase (Just 3) 1000 workT  "s"))

  return resultVar


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = do
  Debug.message Debug.dump_cc ("cc: " ++ msg)
  next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

{-# INLINE time #-}
time :: IO a -> IO (a, Double)
#ifdef ACCELERATE_DEBUG
time action = do
  t0 <- getCurrentTime
  r  <- action
  t1 <- getCurrentTime
  return $ (r, realToFrac (diffUTCTime t1 t0))
#else
time action = do
  r <- action
  return (r, 0)
#endif

