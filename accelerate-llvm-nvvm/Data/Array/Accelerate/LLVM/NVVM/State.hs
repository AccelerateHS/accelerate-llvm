{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.State
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.State (

  evalNVVM,
  createNVVM, defaultNVVM

) where

-- accelerate
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.NVVM.Analysis.Device
import Data.Array.Accelerate.LLVM.NVVM.Target
import qualified Data.Array.Accelerate.LLVM.NVVM.Context        as CT
import qualified Data.Array.Accelerate.LLVM.NVVM.Array.Table    as MT
import qualified Data.Array.Accelerate.LLVM.NVVM.Execute.Stream as RT
import qualified Data.Array.Accelerate.LLVM.NVVM.Debug          as Debug

-- standard library
import Control.Exception                                        ( bracket_, catch )
import Control.Concurrent                                       ( runInBoundThread )
import System.IO.Unsafe                                         ( unsafePerformIO )
import System.Mem                                               ( performGC )
import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver                            as CUDA

#include "accelerate.h"


-- | Execute an NVVM computation
--
evalNVVM :: NVVM -> LLVM NVVM a -> IO a
evalNVVM nvvm acc =
  runInBoundThread (bracket_ setup teardown action)
  `catch`
  \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))
  where
    setup       = CT.push (nvvmContext nvvm)
    teardown    = performGC >> CT.pop
    action      = evalLLVM nvvm acc


-- | Create a new NVVM execution target for the given device
--
createNVVM
    :: CUDA.Device
    -> CUDA.DeviceProperties
    -> [CUDA.ContextFlag]
    -> IO NVVM
createNVVM dev prp flags = do
  ctx   <- CT.new dev prp flags
  mt    <- MT.new ctx
  st    <- RT.new ctx
  return $! NVVM ctx mt st


-- Top-level mutable state
-- -----------------------
--
-- It is important to keep some information alive for the entire run of the
-- program, not just a single execution. These tokens use 'unsafePerformIO' to
-- ensure they are executed only once, and reused for subsequent invocations.
--

-- | Select and initialise the default CUDA device, and create a new target
-- context. The device is selected based on compute capability and estimated
-- maximum throughput.
--
{-# NOINLINE defaultNVVM #-}
defaultNVVM :: NVVM
defaultNVVM = unsafePerformIO $ do
  Debug.message Debug.dump_gc "gc: initialise default instance"
  CUDA.initialise []
  (dev,prp)     <- selectBestDevice
  createNVVM dev prp [CUDA.SchedAuto]

