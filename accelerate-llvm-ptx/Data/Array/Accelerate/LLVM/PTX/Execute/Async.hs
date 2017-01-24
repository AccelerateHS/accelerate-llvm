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

  Async, Stream, Event,
  module Data.Array.Accelerate.LLVM.Execute.Async,

) where

-- accelerate
import Data.Array.Accelerate.LLVM.Execute.Async                 hiding ( Async )
import qualified Data.Array.Accelerate.LLVM.Execute.Async       as A

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Event             ( Event )
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream            ( Stream )
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Event   as Event
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Stream  as Stream

-- standard library
import Control.Monad.State
import GHC.Float                                                ( float2Double )


-- Asynchronous arrays in the CUDA backend are tagged with an Event that will be
-- filled once the kernel implementing that array has completed.
--
type Async a = AsyncR PTX a

instance A.Async PTX where
  type StreamR PTX = Stream
  type EventR  PTX = Event

  {-# INLINEABLE fork #-}
  fork =
    Stream.create

  {-# INLINEABLE join #-}
  join stream =
    liftIO $! Stream.destroy stream

  {-# INLINEABLE checkpoint #-}
  checkpoint stream =
    Event.waypoint False stream

  {-# INLINEABLE after #-}
  after stream event =
    liftIO $! Event.after event stream

  {-# INLINEABLE block #-}
  block event =
    liftIO $! Event.block event

  {-# INLINEABLE timed #-}
  timed f = do
    s     <- fork
    start <- Event.waypoint True s
    r     <- f s
    end   <- Event.waypoint True s
    block start
    block end
    time  <- liftIO $! float2Double <$> Event.elapsedTime start end
    return (time,r)
