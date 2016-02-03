{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Array.Data
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Array.Data (

  module Data.Array.Accelerate.LLVM.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array(..) )
import qualified Data.Array.Accelerate.Array.Sugar              as Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Async             ()
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim      as Prim

-- standard library
import Control.Monad.State


-- Instance of remote array memory management for the PTX target
--
instance Remote PTX where

  {-# INLINEABLE allocateRemote #-}
  allocateRemote sh = do
    arr <- liftIO $ Sugar.allocateArray sh
    runArray (Prim.mallocArray (Sugar.size sh)) arr
    return arr

  {-# INLINEABLE useRemoteR #-}
  useRemoteR mst arr@(Array sh _) = do
    let !n = R.size sh
    case mst of
      Nothing -> runArray (Prim.useArray         n) arr
      Just st -> runArray (Prim.useArrayAsync st n) arr

  {-# INLINEABLE copyToRemoteR #-}
  copyToRemoteR from to mst arr = do
    case mst of
      Nothing -> runArray (Prim.pokeArrayR         from to) arr
      Just st -> runArray (Prim.pokeArrayAsyncR st from to) arr

  {-# INLINEABLE copyToHostR #-}
  copyToHostR from to mst arr = do
    case mst of
      Nothing -> runArray (Prim.peekArrayR         from to) arr
      Just st -> runArray (Prim.peekArrayAsyncR st from to) arr

  {-# INLINEABLE copyToPeerR #-}
  copyToPeerR = error "copyToPeerR"
  -- copyToPeerR from to dst mst arr = do
  --   src <- gets llvmTarget
  --   liftIO . unless (ptxContext src == ptxContext dst) $
  --     liftIO $ case mst of
  --       Nothing -> runArray (Prim.copyArrayPeerR      (ptxContext src) (ptxMemoryTable src) (ptxStreamReservoir src) (ptxContext dst) (ptxMemoryTable dst)    from to) arr
  --       Just st -> runArray (Prim.copyArrayPeerAsyncR (ptxContext src) (ptxMemoryTable src)                          (ptxContext dst) (ptxMemoryTable dst) st from to) arr

  {-# INLINEABLE indexRemote #-}
  indexRemote arr i =
    runIndexArray Prim.indexArray arr i

