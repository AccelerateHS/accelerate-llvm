-- |
-- Module      : Control.Parallel.Meta.Resource.NUMA
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

module Control.Parallel.Meta.Resource.NUMA
  where


import Control.Parallel.Meta
import Control.Parallel.Meta.Worker

import Data.IntMap.Strict                               ( IntMap )
import Data.IntMap.Strict                               as IM

import Data.Vector                                      ( Vector )
import qualified Data.Vector                            as V


-- type Typology = Vector Resource

-- We want to stack the resource such that we search locally, and then search
-- the other members of the topology.
--
-- 1. get all permutations of the gang
--
-- 2. for each permutations (x:xs)
--      -> x <> smp xs
--
-- 3. now we have a list of resources ... smp them together?
--

-- data Topology = Topology {
--
--   }

-- mkTopo :: [(Resource, Gang)] -> Topology
--
--
-- mkWorkSearch :: Int -> Topology -> WorkSearch
-- mkWorkSearch retries topo =

--mkWorkSearch :: Int -> Vector (Resource, Gang) -> WorkSearch
--mkWorkSearch retries topo =
localMap topo  = IM.fromList
                $ V.toList
                $ V.concatMap (\(i, (_,g)) -> V.map (\w -> (workerId w, i)) g)
                $ V.indexed topo

