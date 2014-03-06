{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Debug
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Debug (

  module Data.Array.Accelerate.LLVM.Debug,
  module Data.Array.Accelerate.LLVM.NVVM.Debug,

) where

import Data.Array.Accelerate.LLVM.Debug

import Foreign.CUDA.Driver.Stream                       ( Stream )
import qualified Foreign.CUDA.Driver.Event              as Event

import Control.Concurrent
import Control.Monad.Trans
import Data.Label
import System.CPUTime
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
      cpuBegin  <- liftIO getCPUTime
      liftIO $ Event.record gpuBegin _stream
      action
      liftIO $ Event.record gpuEnd _stream
      cpuEnd    <- liftIO getCPUTime

      -- Wait for the GPU to finish executing then display the timing execution
      -- message. Do this in a separate thread so that the remaining kernels can
      -- be queued asynchronously.
      --
      _         <- liftIO . forkIO $ do
        Event.block gpuEnd
        diff    <- Event.elapsedTime gpuBegin gpuEnd
        let gpuTime = float2Double $ diff * 1E-3                         -- milliseconds
            cpuTime = fromIntegral (cpuEnd - cpuBegin) * 1E-12 :: Double -- picoseconds

        Event.destroy gpuBegin
        Event.destroy gpuEnd
        --
        message _f (_str gpuTime cpuTime)
      --
      return ()

  | otherwise
#endif
  = action

{-# INLINE elapsed #-}
elapsed :: Double -> Double -> String
elapsed gpuTime cpuTime =
  printf "gpu: %s, cpu: %s"
    (showFFloatSIBase (Just 3) 1000 gpuTime "s")
    (showFFloatSIBase (Just 3) 1000 cpuTime "s")

