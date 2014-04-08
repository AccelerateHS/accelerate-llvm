{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Debug
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Debug (

  module Data.Array.Accelerate.LLVM.Debug,
  module Data.Array.Accelerate.LLVM.PTX.Debug,

) where

import Data.Array.Accelerate.LLVM.Debug

import Foreign.CUDA.Driver.Stream                       ( Stream )
import qualified Foreign.CUDA.Driver.Event              as Event

import Control.Concurrent
import Control.Monad.Trans
import Data.Label
import Data.Time.Clock
import Text.Printf

import GHC.Float


{-# INLINE timed #-}
timed
    :: MonadIO m
    => (Flags :-> Bool)
    -> (Double -> Double -> String)
    -> Maybe Stream
    -> m ()
    -> m ()
timed _f _str _stream action
#ifdef ACCELERATE_DEBUG
  | mode _f
  = do
      gpuBegin  <- liftIO $ Event.create []
      gpuEnd    <- liftIO $ Event.create []
      wallBegin <- liftIO getCurrentTime
      liftIO $ Event.record gpuBegin _stream
      action
      liftIO $ Event.record gpuEnd _stream
      wallEnd   <- liftIO getCurrentTime

      -- Wait for the GPU to finish executing then display the timing execution
      -- message. Do this in a separate thread so that the remaining kernels can
      -- be queued asynchronously.
      --
      _         <- liftIO . forkIO $ do
        Event.block gpuEnd
        diff    <- Event.elapsedTime gpuBegin gpuEnd
        let gpuTime  = float2Double $ diff * 1E-3       -- milliseconds
            wallTime = realToFrac (diffUTCTime wallEnd wallBegin)

        Event.destroy gpuBegin
        Event.destroy gpuEnd
        --
        message _f (_str gpuTime wallTime)
      --
      return ()

  | otherwise
#endif
  = action

{-# INLINE elapsed #-}
elapsed :: Double -> Double -> String
elapsed gpuTime wallTime =
  printf "gpu: %s, wall: %s"
    (showFFloatSIBase (Just 3) 1000 gpuTime "s")
    (showFFloatSIBase (Just 3) 1000 wallTime "s")

