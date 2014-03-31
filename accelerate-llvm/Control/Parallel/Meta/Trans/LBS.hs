{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Control.Parallel.Meta.Resource.LBS
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements a lazy binary splitting resource transformer. It is
-- suitable to add an adaptive runtime work-stealing component to resource.
--

module Control.Parallel.Meta.Trans.LBS (

  mkResource,
  mkWorkSearch,

) where

import Data.Range.Range                                         as R
import Data.Concurrent.Deque.Class
import Text.Printf
import qualified Data.Array.Accelerate.LLVM.Debug               as Debug

import Control.Parallel.Meta
import Control.Parallel.Meta.Worker


-- | Transform the 'WorkSearch' function of the given 'Resource' to include a
-- lazy binary splitting work stealing scheduler.
--
mkResource
    :: Int              -- ^ profitable parallelism threshold
    -> Resource         -- ^ the resource to modify
    -> Resource
mkResource ppt (Resource st ws)
  = Resource st (mkWorkSearch ppt ws)


-- | This transforms the 'WorkSearch' function to add a lazy binary splitting
-- operation on top of an existing (work-stealing) scheduler. On receiving a
-- unit of work, the scheduler proceeds as follows:
--
--   1. If the number of iterations is less than the profitable parallelism
--      threshold (ppt), execute the remaining iterations and exit, else (2).
--
--   2. Check this worker's remaining work queue. If it is not empty, then
--      execute ppt iterations, then go to (1) with the remainder.
--
--   3. If the remaining work queue is empty, split this work chunk in half.
--      Place the second half onto the remaining work queue and go to (1).
--
mkWorkSearch
  :: Int                -- ^ profitable parallelism threshold
  -> WorkSearch         -- ^ the basic work search method to modify
  -> WorkSearch
mkWorkSearch ppt steal =
  let search !me@Worker{..} = do

        -- Look for some work to do. If there is work on the local queue, take
        -- that first before trying to steal from the neighbours.
        self <- tryPopL workpool
        work <- case self of
                  Nothing -> runWorkSearch steal me
                  Just _  -> return self

        -- Once we have some work, take the first ppt elements (which we will
        -- return so that they are processed next) and decide what do do with
        -- the remainder:
        --
        --   1. If our deque is empty, split it in half and push both pieces
        --      back onto the deque.
        --
        --   2. If it is not empty, push the remainder back without splitting.
        --
        -- This strategy avoids excessive splitting, especially in the case
        -- where this worker steals back the remainder.
        case work of
          Nothing       -> return Nothing
          Just r        -> do
            let (this, rest)    = R.splitAt ppt r
                handleRemainder =
                  case rest of
                    Nothing -> message workerId "work search failed"
                    Just x  -> do
                      empty <- nullQ workpool
                      if not empty
                         then do
                           message workerId (printf "not splitting remainder %s" (showRange x))
                           pushL workpool x

                         else case R.halve x of
                           (l, Nothing) -> do message workerId (printf "singleton remainder %s" (showRange l))
                                              pushL workpool l

                           (l, Just u)  -> do message workerId (printf "splitting remainder %s -> %s, %s" (showRange x) (showRange l) (showRange u))
                                              pushL workpool u >> pushL workpool l
            --
            message workerId (printf "got work range %s" (showRange this))
            handleRemainder
            return (Just this)
  in
  WorkSearch search


-- Debugging
-- ---------

{-# INLINE showRange #-}
showRange :: Range -> String
showRange (SingletonRange x)   = printf "[%d]" x
showRange (InclusiveRange u v) = printf "[%d,%d]" u v

{-# INLINE message #-}
message :: Int -> String -> IO ()
message tid msg = Debug.message Debug.dump_sched (printf "sched/lbs: [%d] %s" tid msg)

