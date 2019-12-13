{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Control.Parallel.Meta.Resource.LBS
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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

import Control.Parallel.Meta
import Control.Parallel.Meta.Worker
import Data.Concurrent.Deque.Class
import Data.Range                                               ( Range )
import qualified Data.Range                                     as R
import qualified Data.Array.Accelerate.Debug                    as Debug

import Control.Monad
import Text.Printf
import qualified Data.Vector                                    as V


-- | Transform the 'WorkSearch' function of the given 'Resource' to include a
-- lazy binary splitting work stealing scheduler.
--
{-# INLINE mkResource #-}
mkResource
    :: Int              -- ^ profitable parallelism threshold
    -> Resource         -- ^ the resource to modify
    -> Resource
mkResource ppt (Resource ws)
  = Resource (mkWorkSearch ppt ws)


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
{-# INLINE mkWorkSearch #-}
mkWorkSearch
    :: Int              -- ^ profitable parallelism threshold
    -> WorkSearch       -- ^ the basic work search method to modify
    -> WorkSearch
mkWorkSearch ppt steal = WorkSearch search
  where
    search :: Int -> Workers -> IO (Maybe Range)
    search tid workers = do
        let Worker{..} = V.unsafeIndex workers tid

        -- Look for some work to do. If there is work on the local queue, take
        -- that first before trying to steal from the neighbours.
        --
        self <- tryPopL workpool
        work <- case self of
                  Nothing -> runWorkSearch steal tid workers
                  Just _  -> return self

        -- Once we have some work, take the first ppt elements (which we will
        -- return so that they are processed next) and decide what do do with
        -- the remainder:
        --
        --   1. If the deque is not empty OR the remainder is less than the PPT,
        --      push it back without splitting.
        --
        --   2. If our deque is empty, split it in half and push both pieces
        --      back onto the deque.
        --
        -- This strategy avoids excessive splitting, especially in the case
        -- where this worker steals back the remainder from itself.
        --
        case work of
          Just r | not (R.null r) -> do
            let (this, rest)    = R.splitAt ppt r
                handleRemainder
                  | R.null rest         = return ()

                  | R.size rest < ppt   = do
                      message workerId (printf "not splitting remainder (size %d < ppt %d)" (R.size rest) ppt)
                      pushL workpool rest

                  | otherwise           = do
                      empty <- nullQ workpool
                      if not empty
                         then do
                           message workerId (printf "not splitting remainder %s (deque not empty)" (show rest))
                           pushL workpool rest

                         else do
                           let (l,u) = R.bisect rest
                           message workerId (printf "splitting remainder %s -> %s, %s" (show rest) (show l) (show u))
                           unless (R.null u) $ pushL workpool u
                           pushL workpool l
            --
            message workerId (printf "got work range %s" (show this))
            handleRemainder
            return (Just this)

          _ -> message workerId "work search failed" >> return Nothing


-- Debugging
-- ---------

{-# INLINE message #-}
message :: Int -> String -> IO ()
message tid msg
  = Debug.when Debug.verbose
  $ Debug.traceIO Debug.dump_sched (printf "sched/lbs: [%d] %s" tid msg)

