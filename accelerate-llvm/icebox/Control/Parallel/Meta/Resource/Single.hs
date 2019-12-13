{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Control.Parallel.Meta.Resource.Single
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Control.Parallel.Meta.Resource.Single
  where

-- accelerate
import Control.Parallel.Meta
import Control.Parallel.Meta.Worker

-- library
import Data.Concurrent.Deque.Class
import qualified Data.Vector                                    as V


-- | Create a resource where each thread works in isolation. The resource is not
-- aware of any other sources of work (at this level) and only ever tries to pop
-- from its own local queue.
--
{-# INLINE mkResource #-}
mkResource :: Resource
mkResource
  = Resource
  $ WorkSearch $ \tid workers -> tryPopL (workpool (V.unsafeIndex workers tid))

