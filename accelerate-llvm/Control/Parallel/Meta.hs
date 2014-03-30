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
import Data.IORef

import Data.Range.Range
import Control.Parallel.Meta.Worker


-- | The 'Startup' component of a 'Resource' is a callback that implements
-- initialisation behaviour. For example, in a multicore system, 'Startup' may
-- spawn a thread onto each CPU.
--
data Startup = Startup {
  runStartup :: WorkSearch -> IORef Gang -> IO () }

instance Monoid Startup where
  mempty                            = Startup $ \_ _  -> return ()
  Startup st1 `mappend` Startup st2 = Startup $ \ws g -> st1 ws g >> st2 ws g


-- | The 'WorkSearch' component of a 'Resource' is a callback that responds to
-- requests for work from meta-workers. The arguments to 'WorkSearch' are the
-- scheduler state for the current thread and a reference to all workers in the
-- program.
--
data WorkSearch = WorkSearch {
  runWorkSearch :: Worker -> IORef Gang -> IO (Maybe Range) }

instance Monoid WorkSearch where
  mempty                                  = WorkSearch $ \_ _ -> return Nothing
  WorkSearch ws1 `mappend` WorkSearch ws2 =
    WorkSearch $ \st gang -> do
        mwork <- ws1 st gang
        case mwork of
          Nothing -> ws2 st gang
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

