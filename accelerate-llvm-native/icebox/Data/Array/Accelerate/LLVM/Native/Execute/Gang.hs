{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Gang
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

module Data.Array.Accelerate.LLVM.Native.Execute.Gang (

    Gang, theGang,
    forkGang, gangSize, gangIO, gangST

) where

import Control.Concurrent.MVar
import Control.Exception                                        ( assert )
import Control.Monad
import GHC.Conc                                                 ( forkOn, numCapabilities )
import GHC.IO
import GHC.ST

import qualified Data.Array.Accelerate.LLVM.Native.Debug        as Debug


-- The Gang --------------------------------------------------------------------
-- | This globally shared gang is auto-initialised at startup and shared by all
--   computations.
--
--   In a data parallel setting, it does not help to have multiple gangs running
--   at the same time. This is because a single data parallel computation should
--   already be able to keep all threads busy. If we had multiple gangs running
--   at the same time, then the system as a whole would run slower as the gangs
--   would contend for cache and thrash the scheduler.
--
{-# NOINLINE theGang #-}
theGang :: Gang
theGang
  = unsafePerformIO
  $ forkGang numCapabilities


-- Requests --------------------------------------------------------------------
-- | The 'Req' type encapsulates work requests for individual members of a gang.
--
data Req
    -- | Instruct the worker to run the given action.
    = ReqDo (Int -> IO ())

    -- | Tell the worker that we're shutting the gang down. The worker
    -- should signal that it's received the request by writing to its result
    -- var before returning to the caller (forkGang).
    | ReqShutdown


-- Gang ------------------------------------------------------------------------
-- | A 'Gang' is a group of threads that execute arbitrary work requests.
--
data Gang
    = Gang
    { -- | Number of threads in the gang.
      gangThreads       :: {-# UNPACK #-} !Int

      -- | Workers listen for requests on these vars.
    , gangRequestVars   :: [MVar Req]

      -- | Workers put their results in these vars.
    , gangResultVars    :: [MVar ()]

#ifdef ACCELERATE_INTERNAL_CHECKS
      -- | Indicates that the gang is busy.
    , gangBusy          :: MVar Bool
#endif
    }

instance Show Gang where
  showsPrec p gang
        = showString "<<"
        . showsPrec p (gangSize gang)
        . showString " threads>>"


-- | O(1). Yield the number of threads in the 'Gang'.
--
gangSize :: Gang -> Int
gangSize Gang{..} = gangThreads


-- | Fork a 'Gang' with the given number of threads (at least 1).
--
forkGang :: Int -> IO Gang
forkGang n
  = assert (n > 0)
  $ do
        message ("creating with " ++ show n ++ " threads")

        -- Create the vars we'll use to issue work requests.
        mvsRequest      <- replicateM n newEmptyMVar

        -- Create the vars we'll use to signal that threads are done.
        mvsDone         <- replicateM n newEmptyMVar

        -- Add finalisers so we can shut the workers down cleanly if they become
        -- unreachable. It is very racy as to which MVar is finalised first. If
        -- we don't put the finaliser on both MVars required by the finaliser,
        -- and the wrong one is GC'd first, the thread never receives the
        -- shutdown signal.
        zipWithM_ (\varReq varDone -> do
                          void $ mkWeakMVar varReq  (finaliseWorker varReq varDone)
                          void $ mkWeakMVar varDone (finaliseWorker varReq varDone))
                mvsRequest
                mvsDone

        -- Create all the worker threads
        zipWithM_ forkOn [0..]
                $ zipWith3 gangWorker [0 .. n-1] mvsRequest mvsDone

#ifdef ACCELERATE_INTERNAL_CHECKS
        -- The gang is currently idle.
        busy    <- newMVar False

        return  $! Gang n mvsRequest mvsDone busy
#else
        return  $! Gang n mvsRequest mvsDone
#endif


-- | The worker thread of a 'Gang'.
--
-- The threads blocks on the MVar waiting for a work request.
--
gangWorker :: Int -> MVar Req -> MVar () -> IO ()
gangWorker threadId varRequest varDone
  = do
        -- Wait for a request
        req     <- takeMVar varRequest

        case req of
          ReqDo action
            -> do
                  -- Run the action we were given.
                  action threadId

                  -- Signal that the action is complete.
                  putMVar varDone ()

                  -- Wait for more requests.
                  gangWorker threadId varRequest varDone

          ReqShutdown
            -> putMVar varDone ()


-- | The finaliser for worker threads.
--
--   We want to shutdown the corresponding thread when it's MVar becomes
--   unreachable.
--
--   Without this programs can complain about "Blocked indefinitely on an MVar"
--   because worker threads are still blocked on the request MVars when the
--   program ends. Whether the finalizer is called or not is very racey.
--
--   We're relying on the comment in System.Mem.Weak that says:
--
--     "If there are no other threads to run, the runtime system will check for
--      runnable finalizers before declaring the system to be deadlocked."
--
--   If we were creating and destroying the gang cleanly we wouldn't need this,
--   but 'theGang' is created with a top-level unsafePerformIO. Hacks beget
--   hacks beget hacks...
--
finaliseWorker :: MVar Req -> MVar () -> IO ()
finaliseWorker varReq varDone
  = do
        message "shutting down"
        putMVar varReq ReqShutdown
        takeMVar varDone
        return ()


-- | Issue work requests for the 'Gang' and wait until they complete.
--
{-# NOINLINE gangIO #-}
gangIO :: Gang -> (Int -> IO ()) -> IO ()
#if ACCELERATE_INTERNAL_CHECKS
gangIO gang@(Gang _ _ _ busy) action
  = do  b <- swapMVar busy True
        if b
          then seqIO gang action
          else do
                parIO gang action
                _ <- swapMVar busy False
                return ()
#else
gangIO gang action
  = do  parIO gang action
#endif

#ifdef ACCELERATE_INTERNAL_CHECKS
-- | Run an action on the gang sequentially.
--
seqIO :: Gang -> (Int -> IO ()) -> IO ()
seqIO _gang _action
  = error "seqIO: I was not expecting that..."
#endif

-- | Run an action on the gang in parallel.
--
parIO :: Gang -> (Int -> IO ()) -> IO ()
parIO Gang{..} action
  = Debug.timed Debug.dump_exec elapsed
  $ do
        event "parIO start"

        -- Send requests to all the threads.
        mapM_ (\v -> putMVar v (ReqDo action)) gangRequestVars

        -- Wait for all the requests to complete.
        mapM_ takeMVar gangResultVars

        event "parIO end"


-- | Same as 'gangIO' but in the 'ST' monad.
--
gangST :: Gang -> (Int -> ST s ()) -> ST s ()
gangST g p = unsafeIOToST . gangIO g $ unsafeSTToIO . p


-- Debugging
-- ---------

{-# INLINE message #-}
message :: String -> IO ()
message str = Debug.traceIO Debug.dump_sched ("gang: " ++ str)

{-# INLINE elapsed #-}
elapsed :: Double -> Double -> String
elapsed x y = "exec: " ++ Debug.elapsed x y

{-# INLINE event #-}
event :: String -> IO ()
event str = Debug.traceEventIO Debug.dump_exec ("exec: " ++ str)

