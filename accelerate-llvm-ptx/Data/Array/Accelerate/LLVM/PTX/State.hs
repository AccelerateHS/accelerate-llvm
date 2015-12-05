{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.State
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.State (

  evalPTX,
  createTarget, defaultTarget

) where

-- accelerate
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.PTX.Analysis.Device
import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Context         as CT
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Table     as MT
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Stream  as RT
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

import Data.Range.Range                                         ( Range(..) )
import Control.Parallel.Meta                                    ( Executable(..) )

-- standard library
import Control.Exception                                        ( bracket_, catch )
import Control.Concurrent                                       ( runInBoundThread )
import System.IO.Unsafe                                         ( unsafePerformIO )
import System.Mem                                               ( performGC )
import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver                            as CUDA


-- | Execute a PTX computation
--
evalPTX :: PTX -> LLVM PTX a -> IO a
evalPTX ptx acc =
  runInBoundThread (bracket_ setup teardown action)
  `catch`
  \e -> $internalError "unhandled" (show (e :: CUDAException))
  where
    setup       = CT.push (ptxContext ptx)
    teardown    = performGC >> CT.pop
    action      = evalLLVM ptx acc


-- | Create a new PTX execution target for the given device
--
-- TLM: This state structure does not generalise to multiple devices. We require
--      the context and tables per device, whereas the scheduler requires
--      access to all devices.
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
  return $! PTX ctx mt st simpleIO


{-# INLINE simpleIO #-}
simpleIO :: Executable
simpleIO = Executable $ \_ppt range _after _init action ->
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
  createTarget dev prp [CUDA.SchedAuto]

