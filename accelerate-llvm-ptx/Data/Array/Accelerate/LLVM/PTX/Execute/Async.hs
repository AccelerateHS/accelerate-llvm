{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Async
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Async (

  Async(..), Stream,
  A.wait, A.after, A.streaming, A.async,

) where

-- accelerate
import qualified Data.Array.Accelerate.LLVM.Execute.Async       as A

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Event             ( Event )
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream            ( Stream )
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Event   as Event
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Stream  as Stream

-- standard library
import Control.Monad.State


-- Asynchronous arrays in the CUDA backend are tagged with an Event that will be
-- filled once the kernel implementing that array has completed.
--
data Async a = Async {-# UNPACK #-} !Event !a

instance A.Async PTX where
  type AsyncR PTX a = Async a
  type StreamR PTX  = Stream

  {-# INLINEABLE wait #-}
  wait (Async event arr) =
    liftIO $ Event.block event >> return arr

  {-# INLINEABLE after #-}
  after stream (Async event arr) =
    liftIO $ Event.after event stream >> return arr

  {-# INLINEABLE streaming #-}
  streaming first second = do
    PTX{..} <- gets llvmTarget
    Stream.streaming ptxContext ptxStreamReservoir first (\event arr -> second (Async event arr))

  {-# INLINEABLE async #-}
  async stream action = do
    r <- action
    e <- liftIO $ Event.waypoint stream
    return $! Async e r

