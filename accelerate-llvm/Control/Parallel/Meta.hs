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

import Data.Monoid
import Data.Range.Range
import Control.Parallel.Meta.Worker


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

