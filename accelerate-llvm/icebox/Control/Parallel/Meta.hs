{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Control.Parallel.Meta
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Control.Parallel.Meta (

  WorkSearch(..),
  Resource(..),
  Executable(..),
  Action,
  runSeqIO, runParIO,

) where

import Control.Monad
import Control.Parallel.Meta.Worker
import Data.ByteString.Short                                        ( ShortByteString )
import Data.Concurrent.Deque.Class
import Data.Monoid                                                  ( Monoid(..) )
#if __GLASGOW_HASKELL__ >= 800
import Data.Semigroup                                               ( Semigroup(..) )
#endif
import Data.Range                                                   as R
import Prelude                                                      as P
import qualified Data.Vector                                        as V

import GHC.Base                                                     ( quotInt, remInt )


-- | The 'WorkSearch' component of a 'Resource' is a callback that responds to
-- requests for work from meta-workers. The arguments to 'WorkSearch' are the
-- scheduler state for the current thread and a reference to all workers in the
-- program.
--
data WorkSearch = WorkSearch {
    runWorkSearch :: Int -> Workers -> IO (Maybe Range)
  }

#if __GLASGOW_HASKELL__ >= 800
instance Semigroup WorkSearch where
  {-# INLINE (<>) #-}
  WorkSearch ws1 <> WorkSearch ws2 =
    WorkSearch $ \tid st -> do
        mwork <- ws1 tid st
        case mwork of
          Nothing -> ws2 tid st
          _       -> return mwork
#endif

instance Monoid WorkSearch where
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}
  mempty                                  = WorkSearch $ \_ _ -> return Nothing
  WorkSearch ws1 `mappend` WorkSearch ws2 =
    WorkSearch $ \tid st -> do
        mwork <- ws1 tid st
        case mwork of
          Nothing -> ws2 tid st
          _       -> return mwork


-- | A 'Resource' provides an abstraction of heterogeneous execution resources
-- that may be combined. Composition of resources is left-biased. That is, if if
-- @resource1@ always returns work from its 'WorkSearch', then the composed
-- resource @resource1 <> resource2@ will never request work from @resource2@.
--
data Resource = Resource {
    -- startup     :: Startup
    workSearch  :: WorkSearch
  }

#if __GLASGOW_HASKELL__ >= 800
instance Semigroup Resource where
  {-# INLINE (<>) #-}
  Resource ws1 <> Resource ws2 = Resource (ws1 <> ws2)
#endif

instance Monoid Resource where
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}
  mempty                                = Resource mempty
  mappend (Resource ws1) (Resource ws2) = Resource (ws1 `mappend` ws2)


-- | An action to execute. The first parameters are the start and end indices of
-- the range this action should process, and the final is the ID of the thread
-- doing the work.
--
type Action = Int -> Int -> Int -> IO ()
-- data Action = Action {
--     runAction :: Int -> Int -> Int -> IO }
--   }

-- instance Monoid Action where
--   mempty                        = Action $ \_ _ _ -> return ()
--   Action f1 `mappend` Action f2 = Action $ \m n i -> f1 m n i >> f1 m n i


-- | An 'Executable' provides a callback that can be used to run a provided
-- function using an encapsulated work-stealing gang of threads.
--
data Executable = Executable {
    runExecutable
        :: ShortByteString    -- Function name
        -> Int                -- Profitable parallelism threshold (PPT)
        -> Range              -- The range to execute over
        -> Action             -- The main function to execute
        -> IO ()
  }


-- | Run a sequential operation
--
-- We just have the first thread of the gang execute the operation, but we could
-- also make the threads compete, which might be useful on a loaded system.
--
{-# INLINEABLE runSeqIO #-}
runSeqIO
    :: Gang
    -> Range
    -> Action
    -> IO ()
runSeqIO _    Empty    _      = return ()
runSeqIO gang (IE u v) action =
  gangIO   gang    $ \workers ->
  workerIO workers $ \thread  ->
    when (thread == 0) $ action u v thread
    -- let
    --     target  = V.unsafeIndex workers 0
    --     loop 0  = return ()
    --     loop n  = do
    --       mwork <- tryPopR (workpool target)
    --       case mwork of
    --         Nothing       -> loop (n-1)
    --         Just Empty    -> return ()
    --         Just (IE u v) -> action u v thread
    -- --
    -- when (thread == 0) $ pushL (workpool target) range
    -- loop 3


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
-- decomposition. However, this method should be better for locality,
-- particularly when the workloads are balanced and little stealing occurs.
--
-- TLM: Should threads check whether the work queues of all threads are empty
--      before deciding to exit? If the PPT is too large then threads might not
--      react quickly enough to splitting once their deque is emptied. Maybe the
--      first thread to return Nothing can probe the queues to see if they are
--      all empty. If True, write into a shared MVar to signal to the others
--      that it is time to exit. But, that still assumes that the PPT is not so
--      large that the queues are always empty.
--
-- TLM: The initial work distribution should probably be aligned to cache
--      boundaries, rather than attempting to split exactly evenly.
--
{-# INLINEABLE runParIO #-}
runParIO
    :: Resource
    -> Gang
    -> Range
    -> Action
    -> IO ()
runParIO _        _    Empty        _      = return ()
runParIO resource gang (IE inf sup) action =
  gangIO   gang    $ \workers ->
  workerIO workers $ \tid     -> do
    let
        len       = sup - inf
        threads   = V.length workers
        chunk     = len `quotInt` threads
        leftover  = len `remInt`  threads

        start     = splitIx tid
        end       = splitIx (tid + 1)
        me        = V.unsafeIndex workers tid

        splitIx n | n < leftover = inf + n * (chunk + 1)
        splitIx n                = inf + n * chunk + leftover

        loop  = do
          work <- runWorkSearch (workSearch resource) tid workers
          case work of
            -- Got a work unit. Execute it then search for more.
            Just (IE u v) -> action u v tid >> loop

            -- If the work search failed (which is random), to be extra safe
            -- make sure all the work queues are exhausted before exiting.
            _             -> do
              done <- exhausted workers
              unless done loop

    when (end > start) $ pushL (workpool me) (IE start end)
    loop



-- Icebox
-- ------

{--
-- | The 'Startup' component of a 'Resource' is a callback that implements
-- initialisation behaviour. For example, it might contact remote hosts, spawn
-- threads, or initialise hardware such as GPUs.
--
data Startup = Startup {
  _runStartup :: Gang -> IO () }

instance Monoid Startup where
  mempty                            = Startup $ \_ -> return ()
  Startup st1 `mappend` Startup st2 = Startup $ \g -> st1 g >> st2 g

-- | The 'Finalise' component of an executable is an action the thread applies
-- after processing the work function, given its thread id the ranges that this
-- thread actually handled.
--
data Finalise = Finalise {
    _runFinalise :: Seq Range -> IO ()
  }

instance Monoid Finalise where
  mempty                            = Finalise $ \_ -> return ()
  Finalise f1 `mappend` Finalise f2 = Finalise $ \r -> f1 r >> f2 r
--}

