-- |
-- Module      : Control.Parallel.Meta
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Control.Parallel.Meta
  where

import Data.Monoid
import Control.Parallel.Meta.Worker
import Data.Concurrent.Deque.Class
import Data.Sequence                                            ( Seq )
import Data.Range.Range                                         as R
import qualified Data.Vector                                    as V
import qualified Data.Sequence                                  as Seq

import GHC.Base                                                 ( quotInt, remInt )


-- | The 'Startup' component of a 'Resource' is a callback that implements
-- initialisation behaviour. For example, it might contact remote hosts, spawn
-- threads, or initialise hardware such as GPUs.
--
data Startup = Startup {
  runStartup :: Gang -> IO () }

instance Monoid Startup where
  mempty                            = Startup $ \_ -> return ()
  Startup st1 `mappend` Startup st2 = Startup $ \g -> st1 g >> st2 g


-- | The 'WorkSearch' component of a 'Resource' is a callback that responds to
-- requests for work from meta-workers. The arguments to 'WorkSearch' are the
-- scheduler state for the current thread and a reference to all workers in the
-- program.
--
data WorkSearch = WorkSearch {
  runWorkSearch :: Worker -> IO (Maybe Range) }

instance Monoid WorkSearch where
  mempty                                  = WorkSearch $ \_ -> return Nothing
  WorkSearch ws1 `mappend` WorkSearch ws2 =
    WorkSearch $ \st -> do
        mwork <- ws1 st
        case mwork of
          Nothing -> ws2 st
          _       -> return mwork


-- | A 'Resource' provides an abstraction of heterogeneous execution resources
-- that may be combined. Composition of resources is left-biased. That is, if if
-- @resource1@ always returns work from its 'WorkSearch', then the composed
-- resourc @resource1 <> resource2@ will never request work from @resource2@.
--
data Resource = Resource {
    startup     :: Startup
  , workSearch  :: WorkSearch
  }

instance Monoid Resource where
  mempty                                        = Resource mempty mempty
  mappend (Resource st1 ws1) (Resource st2 ws2) = Resource (st1 <> st2) (ws1 <> ws2)


-- | An 'Executable' provides a callback that can be used to run a provided
-- function using an encapsulated work-stealing gang of threads.
--
data Executable = Executable {
    runExecutable
        :: Int          -- ^ Profitable parallelism threshold (PPT)
        -> Range        -- ^ The range to execute over
        -> Finalise     -- ^ Post-processing function given the ranges processed
                        -- by this thread.
        -> (Int -> Int -> Int -> IO ())
                -- ^ The function to execute. The first parameters are
                -- the start and end indices of the array this action
                -- should process, and the final is the ID of the thread
                -- doing the work.
        -> IO ()
  }

-- | The 'Finalise' component of an executable is an action the thread applies
-- after processing the work function, given the ranges that this thread
-- handled.
--
data Finalise = Finalise {
  runFinalise :: Seq Range -> IO () }

instance Monoid Finalise where
  mempty                            = Finalise $ \_ -> return ()
  Finalise f1 `mappend` Finalise f2 = Finalise $ \r -> f1 r >> f2 r


-- | Run a parallel work-stealing operation.
--
-- Each thread initialises its work queue with an equally sized chunk of the
-- work. Threads keep working until their work search returns Nothing, at which
-- point the thread exits. In our LBS implementation, a worker thread takes a
-- small chunk of its work range to process, and places the remainder back onto
-- its deque. Thus the work queues are only empty:
--
--   (a) Briefly during the scheduling process; or
--
--   (b) After the deque has been robbed. If the stolen chunk is large enough,
--       the stealee will split the remainder onto its deque to be stolen; or
--
--   (c) There is no more work.
--
-- As long as the thread makes a small number of retries, this should correctly
-- balance the work without too much scheduler overhead.
--
-- An alternative to every thread initialising with an even chunk size is to put
-- the entire range onto the first worker and then have the scheduler handle the
-- decomposition. However, since we are playing fast-and-loose with the exit
-- condition, this is a little racy.
--
-- TLM NOTE:
--
-- Should threads check whether the work queues of all threads are empty before
-- deciding to exit? If the PPT is too large then threads might not react
-- quickly enough to splitting once their deque is emptied. Maybe the first
-- thread to return Nothing can probe the queues to see if they are all empty.
-- If True, write into a shared MVar to signal to the others that it all is time
-- to exit. But, that still assumes that the PPT is not so large that the queues
-- are always empty.
--
runParIO
    :: Resource
    -> Gang
    -> Range
    -> (Int -> Int -> Int -> IO ())
    -> (Seq Range -> IO ())
    -> IO ()
runParIO resource gang range action finish
  | gangSize gang == 1  = seqIO resource gang range action finish
  | otherwise           = parIO resource gang range action finish

seqIO
    :: Resource
    -> Gang
    -> Range
    -> (Int -> Int -> Int -> IO ())
    -> (Seq Range -> IO ())
    -> IO ()
seqIO _ _    Empty    _      finish = finish Seq.empty
seqIO _ gang (IE u v) action finish = do
  gangIO gang $ action u v
  finish $ Seq.singleton (IE u v)

parIO
    :: Resource
    -> Gang
    -> Range
    -> (Int -> Int -> Int -> IO ())
    -> (Seq Range -> IO ())
    -> IO ()
parIO _        _    Empty        _      finish = finish Seq.empty
parIO resource gang (IE inf sup) action finish =
  gangIO gang $ \thread -> do
      let start = splitIx thread
          end   = splitIx (thread + 1)
          me    = V.unsafeIndex gang thread

          loop rs = do
            work <- runWorkSearch (workSearch resource) me
            case work of
              Just r@(IE u v)   -> action u v thread >> loop (rs `R.append` r)
              _                 -> return rs

      ranges <- if start < end
                  then pushL (workpool me) (IE start end) >> loop Seq.empty
                  else return $ Seq.empty
      finish (compress ranges)
  where
    len                 = sup - inf
    workers             = gangSize gang
    chunkLen            = len `quotInt` workers
    chunkLeftover       = len `remInt`  workers

    splitIx thread
      | thread < chunkLeftover
      = inf + thread * (chunkLen + 1)

      | otherwise
      = inf + thread * chunkLen + chunkLeftover

