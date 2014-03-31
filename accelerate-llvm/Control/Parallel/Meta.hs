-- |
-- Module      : Control.Parallel.Meta
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Control.Parallel.Meta
  where

import Control.Monad
import Data.Monoid
import Data.Range.Range
import Data.Concurrent.Deque.Class
import Control.Parallel.Meta.Worker
import qualified Data.Vector                                    as V

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
    -> Int
    -> (Int -> Int -> Int -> IO ())
    -> IO ()
runParIO resource gang len action =
  gangIO gang $ \thread -> do
      let start = splitIx thread
          end   = splitIx (thread + 1)
          me    = V.unsafeIndex gang thread

          loop  = do
            work <- runWorkSearch (workSearch resource) me
            case work of
              Just (Inclusive u v) -> action u (v+1) thread >> loop
              _                    -> return ()

      when (start < end) $ do
        pushL (workpool me) (start ... end-1)
        loop
  where
    workers             = gangSize gang
    chunkLen            = len `quotInt` workers
    chunkLeftover       = len `remInt`  workers

    splitIx thread
      | thread < chunkLeftover
      = thread * (chunkLen + 1)

      | otherwise
      = thread * chunkLen + chunkLeftover

