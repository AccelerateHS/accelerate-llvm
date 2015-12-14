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
import Data.Array.Accelerate.Array.Sugar                        ( Array(..), size )
import qualified Data.Array.Accelerate.Array.Sugar              as Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.State
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
    PTX{..} <- gets llvmTarget
    liftIO   $ do
      arr <- Sugar.allocateArray sh
      runArray (void . Prim.mallocArray ptxContext ptxMemoryTable (size sh)) arr
      return arr

  {-# INLINEABLE copyToRemoteR #-}
  copyToRemoteR from to ms arr@(Array sh _) = do
    PTX{..} <- gets llvmTarget
    liftIO   $ if from == 0 && to == R.size sh
                  then runArray (Prim.useArrayAsync   ptxContext ptxMemoryTable ms to)      arr
                  else runArray (Prim.pokeArrayAsyncR ptxContext ptxMemoryTable ms from to) arr

  {-# INLINEABLE copyToHostR #-}
  copyToHostR from to ms arr = do
    PTX{..} <- gets llvmTarget
    liftIO $ runArray (Prim.peekArrayAsyncR ptxContext ptxMemoryTable ms from to) arr

  {-# INLINEABLE copyToPeerR #-}
  copyToPeerR from to dst ms arr = do
    src <- gets llvmTarget
    liftIO . unless (ptxContext src == ptxContext dst) $ do
      runArray (Prim.copyArrayPeerAsyncR (ptxContext src) (ptxMemoryTable src)
                                         (ptxContext dst) (ptxMemoryTable dst) ms from to)
               arr

  {-# INLINEABLE indexRemote #-}
  indexRemote arr i = do
    PTX{..} <- gets llvmTarget
    liftIO   $ runIndexArray (Prim.indexArray ptxContext ptxMemoryTable) arr i

