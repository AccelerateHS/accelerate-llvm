{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.State
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.State (

  evalPTX,
  createTarget, defaultTarget

) where

-- accelerate
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.Analysis.Device
import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Context         as CT
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Table     as MT
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Stream  as RT
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

-- standard library
import Control.Exception                                        ( bracket_, catch )
import Control.Concurrent                                       ( runInBoundThread )
import System.IO.Unsafe                                         ( unsafePerformIO )
import System.Mem                                               ( performGC )
import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver                            as CUDA

#include "accelerate.h"


-- | Execute a PTX computation
--
evalPTX :: PTX -> LLVM PTX a -> IO a
evalPTX ptx acc =
  runInBoundThread (bracket_ setup teardown action)
  `catch`
  \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))
  where
    setup       = CT.push (ptxContext ptx)
    teardown    = performGC >> CT.pop
    action      = evalLLVM ptx acc


-- | Create a new PTX execution target for the given device
--
createTarget
    :: CUDA.Device
    -> CUDA.DeviceProperties
    -> [CUDA.ContextFlag]
    -> IO PTX
createTarget dev prp flags = do
  ctx   <- CT.new dev prp flags
  mt    <- MT.new ctx
  st    <- RT.new ctx
  return $! PTX ctx mt st


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
{-# NOINLINE defaultTarget #-}
defaultTarget :: PTX
defaultTarget = unsafePerformIO $ do
  Debug.message Debug.dump_gc "gc: initialise default instance"
  CUDA.initialise []
  (dev,prp)     <- selectBestDevice
  createTarget dev prp [CUDA.SchedAuto]

