-- |
-- Module      : Control.Parallel.Meta.Resource.Single
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements a single-threaded resource.
--

module Control.Parallel.Meta.Resource.Single
  where

-- accelerate
import Control.Parallel.Meta
import Control.Parallel.Meta.Worker

-- library
import Control.Exception
import Data.Monoid
import Data.Concurrent.Deque.Class
import qualified Data.Vector                                    as V


-- | Create a single-threaded resource. This resource is not aware of any other
-- sources of work (at this level). The workpool might still be stolen from by
-- another processor.
--
-- The gang must contain a single worker.
--
mkResource :: Gang -> Resource
mkResource single
  = assert (gangSize single == 1)
  $ Resource mempty (mkWorkSearch (V.unsafeIndex single 0))


-- | The 'WorkSearch' for a single worker
--
mkWorkSearch :: Worker -> WorkSearch
mkWorkSearch single =
  WorkSearch $ \me ->
    if workerId me == workerId single
       then return Nothing
       else tryPopR (workpool single)

