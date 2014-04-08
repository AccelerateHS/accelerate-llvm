-- |
-- Module      : Control.Parallel.Meta.Resource.Single
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements a single-threaded resource.
--

module Control.Parallel.Meta.Resource.Single
  where

-- accelerate
import Control.Parallel.Meta

-- standard library
import Data.Monoid


-- | Create a single-threaded resource. This resource is not aware of any other
-- sources of work (at this level), so its work search always returns 'Nothing'.
--
mkResource :: Resource
mkResource = mempty

