{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Control.Parallel.Meta.Resource.SMP
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

import qualified Data.Array.Accelerate.LLVM.Debug       as Debug

-- standard library
import Data.Concurrent.Deque.Class
import Data.IORef
import Data.Monoid
import System.Random.MWC
import Text.Printf
import qualified Data.Vector                            as V


-- | Create an SMP (symmetric multiprocessor) resource where all underlying
-- workers have uniform access to shared resources such as memory. Thus workers
-- at this level can steal cheaply from each other.
--
mkResource
    :: Int              -- ^ number of steal attempts per 'WorkSearch'
    -> Gang             -- ^ the workers that will steal amongst each other
    -> Resource
mkResource retries gang =
  Resource mempty (mkWorkSearch retries gang)


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
mkWorkSearch :: Int -> Gang -> WorkSearch
mkWorkSearch retries gang =
  let search !me =
        let myId        = workerId me
            random      = uniformR (0, V.length gang - 1) (rngState me)

            loop 0      = do
              message myId "work search failed"
              modifyIORef' (consecutiveFailures me) (+1)
              return Nothing

            loop n      = do
              target <- V.unsafeIndex gang `fmap` random
              if workerId target == myId
                 then loop (n-1)
                 else do
                   mwork <- tryPopR (workpool target)
                   case mwork of
                     Nothing    -> loop (n-1)
                     _          -> do message myId (printf "steal from %d" (workerId target))
                                      writeIORef (consecutiveFailures me) 0
                                      return mwork
        in
        loop retries
--          self <- tryPopL (workpool me)
--          case self of
--            Nothing -> loop retries
--            _       -> do message myId "steal from self"
--                          writeIORef (consecutiveFailures me) 0
--                          return self
  in
  WorkSearch search


-- Debugging
-- ---------

{-# INLINE message #-}
message :: Int -> String -> IO ()
message tid msg = Debug.message Debug.dump_sched (printf "sched/smp: [%d] %s" tid msg)

