{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.State
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.State (

  evalPTX,
  createTargetForDevice, createTargetFromContext, defaultTarget,
  unsafeInterleavePTX

) where

-- accelerate
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.PTX.Analysis.Device
import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Context         as CT
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Table     as MT
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Stream  as ST
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

import Data.Range.Range                                         ( Range(..) )
import Control.Parallel.Meta                                    ( Executable(..) )

-- standard library
import Control.Concurrent                                       ( runInBoundThread )
import Control.Exception                                        ( catch )
import Control.Monad.State                                      ( gets, liftIO )
import System.IO.Unsafe                                         ( unsafePerformIO, unsafeInterleaveIO )
import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver                            as CUDA
import qualified Foreign.CUDA.Driver.Context                    as Context


-- | Execute a PTX computation
--
evalPTX :: PTX -> LLVM PTX a -> IO a
evalPTX ptx acc =
  runInBoundThread (CT.withContext (ptxContext ptx) (evalLLVM ptx acc))
  `catch`
  \e -> $internalError "unhandled" (show (e :: CUDAException))


-- | Interleave a computation in the native backend.
--
unsafeInterleavePTX :: LLVM PTX a -> LLVM PTX a
unsafeInterleavePTX a = do
  target <- gets llvmTarget
  liftIO (unsafeInterleaveIO (evalPTX target a))


-- | Create a new PTX execution target for the given device
--
createTargetForDevice
    :: CUDA.Device
    -> CUDA.DeviceProperties
    -> [CUDA.ContextFlag]
    -> IO PTX
createTargetForDevice dev prp flags = do
  ctx    <- CT.new dev prp flags
  mt     <- MT.new ctx
  st     <- ST.new ctx
  return $! PTX ctx mt st simpleIO


-- | Create a PTX execute target for the given device context
--
createTargetFromContext
    :: CUDA.Context
    -> IO PTX
createTargetFromContext ctx' = do
  dev    <- Context.device
  prp    <- CUDA.props dev
  ctx    <- CT.raw dev prp ctx'
  mt     <- MT.new ctx
  st     <- ST.new ctx
  return $! PTX ctx mt st simpleIO


{-# INLINE simpleIO #-}
simpleIO :: Executable
simpleIO = Executable $ \_name _ppt range action ->
  case range of
    Empty       -> return ()
    IE u v      -> action u v 0


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
  Debug.traceIO Debug.dump_gc "gc: initialise default PTX target"
  CUDA.initialise []
  (dev,prp)     <- selectBestDevice
  createTargetForDevice dev prp [CUDA.SchedAuto]

