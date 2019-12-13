{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Control.Parallel.Meta.Resource.SMP
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements a resource for SMP parallelism. It is suitable as a
-- base for combining a bunch of resources that can steal cheaply from each
-- other.
--
-- Inspired by the meta-par package. This package has a BSD license.
-- <http://hackage.haskell.org/package/meta-par>
--

module Control.Parallel.Meta.Resource.SMP (

  mkResource,
  mkWorkSearch,

) where

-- accelerate
import Control.Parallel.Meta
import Control.Parallel.Meta.Worker
import Data.Primitive.MutVar
import Data.Range
import qualified Data.Array.Accelerate.Debug            as Debug

-- standard library
import Data.Concurrent.Deque.Class
import System.Random.MWC
import Text.Printf
import qualified Data.Vector                            as V


-- | Create an SMP (symmetric multiprocessor) resource where all underlying
-- workers have uniform access to shared resources such as memory. Thus workers
-- at this level can steal cheaply from each other.
--
{-# INLINE mkResource #-}
mkResource
    :: Int              -- ^ number of steal attempts per 'WorkSearch'
    -> Resource
mkResource retries =
  Resource (mkWorkSearch retries)


-- | Given a set of workers and a number of steal attempts per worker, return a
-- work search function. Steals from workers in this gang are considered cheap
-- and uniform.
--
-- Note: [Number of retries in SMP resource]
--
-- A large number of retries in the work search will prevent the search function
-- from traversing the resource stack. This can result in spamming of stealing
-- actions. In particular, the exponential backoff resource usually placed at
-- the bottom of the stack may never be reached. Thus a balance is required
-- between number of times to traverse each level and number of times to
-- traverse the entire resource stack.
--
{-# INLINE mkWorkSearch #-}
mkWorkSearch :: Int -> WorkSearch
mkWorkSearch retries = WorkSearch search
  where
    search :: Int -> Workers -> IO (Maybe Range)
    search !tid !workers =
      let
          me          = V.unsafeIndex workers tid
          myId        = workerId me
          random      = uniformR (0, V.length workers - 1) (rngState me)

          loop 0      = do
            message myId "work search failed"
            modifyMutVar' (consecutiveFailures me) (+1)
            return Nothing

          loop n      = do
            target <- V.unsafeIndex workers `fmap` random
            if workerId target == myId
               then loop (n-1)
               else do
                 mwork <- tryPopR (workpool target)
                 case mwork of
                   Nothing    -> loop (n-1)
                   _          -> do event myId (printf "steal from %d" (workerId target))
                                    writeMutVar (consecutiveFailures me) 0
                                    return mwork
      in
      loop retries
--          self <- tryPopL (workpool me)
--          case self of
--            Nothing -> loop retries
--            _       -> do message myId "steal from self"
--                          writeMutVar (consecutiveFailures me) 0
--                          return self


-- Debugging
-- ---------

{-# INLINE message #-}
message :: Int -> String -> IO ()
message tid msg
  = Debug.when Debug.verbose
  $ Debug.traceIO Debug.dump_sched (printf "sched/smp: [%d] %s" tid msg)

{-# INLINE event #-}
event :: Int -> String -> IO ()
event tid msg = do
  let msg' = printf "sched/smp: [%d] %s" tid msg
  Debug.traceIO      Debug.dump_sched msg'
  Debug.traceEventIO Debug.dump_sched msg'

