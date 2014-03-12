{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.State
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.State (

  evalNVVM,
  create, defaultNVVM

) where

-- accelerate
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.NVVM.Analysis.Device
import Data.Array.Accelerate.LLVM.NVVM.Target
import qualified Data.Array.Accelerate.LLVM.NVVM.Array.Table    as MT
import qualified Data.Array.Accelerate.LLVM.NVVM.Execute.Stream as RT
import qualified Data.Array.Accelerate.LLVM.NVVM.Debug          as Debug

-- standard library
import Control.Exception                                        ( bracket_, catch )
import Control.Concurrent                                       ( forkIO, threadDelay, runInBoundThread )
import Control.Monad                                            ( when )
import Text.PrettyPrint
import System.IO.Unsafe                                         ( unsafePerformIO )
import System.Mem                                               ( performGC )
import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver                            as CUDA
import qualified Foreign.CUDA.Driver.Context                    as CUDA

import GHC.Exts                                                 ( Ptr(..), mkWeak# )
import GHC.Base                                                 ( IO(..) )
import GHC.Weak                                                 ( Weak(..) )

#include "accelerate.h"


-- | Execute an NVVM computation
--
{-# NOINLINE evalNVVM #-}
evalNVVM :: NVVM -> LLVM NVVM a -> IO a
evalNVVM nvvm acc =
  runInBoundThread (bracket_ setup teardown action)
  `catch`
  \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))
  where
    setup       = CUDA.push (nvvmContext nvvm)
    teardown    = CUDA.pop >> performGC
    action      = evalLLVM nvvm acc


-- | Create a new NVVM context for the given device
--
create
    :: CUDA.Device
    -> CUDA.DeviceProperties
    -> [CUDA.ContextFlag]
    -> IO NVVM
create dev prp flags = do
  -- Create and set up the device context
  ctx   <- CUDA.create dev flags >> CUDA.pop
  _     <- mkWeakContext ctx $ do
    Debug.message Debug.dump_gc ("gc: finalise context #" ++ show (CUDA.useContext ctx))
    CUDA.destroy ctx

  -- The kernels don't use much shared memory, so for devices that support it
  -- prefer using those memory banks as an L1 cache.
  when (CUDA.computeCapability prp >= CUDA.Compute 2 0) $
    bracket_ (CUDA.push ctx) CUDA.pop (CUDA.setCacheConfig CUDA.PreferL1)

  -- Set up the memory tables and execution state
  mt    <- MT.new ctx
  st    <- RT.new ctx

  Debug.message Debug.verbose (deviceInfo dev prp)
  return $! NVVM ctx prp mt st

-- | Make a weak pointer to a CUDA context. We need to be careful to put the
-- finaliser on the underlying pointer, rather than the box around it as
-- 'mkWeak' will do, because unpacking the context will cause the finaliser to
-- fire prematurely.
--
mkWeakContext :: CUDA.Context -> IO () -> IO (Weak CUDA.Context)
mkWeakContext c@(CUDA.Context (Ptr c#)) f = IO $ \s ->
  case mkWeak# c# c f s of (# s', w #) -> (# s', Weak w #)


-- Top-level mutable state
-- -----------------------
--
-- It is important to keep some information alive for the entire run of the
-- program, not just a single execution. These tokens use 'unsafePerformIO' to
-- ensure they are executed only once, and reused for subsequent invocations.
--

-- | Make sure the GC knows that we want to keep this thing alive forever.
--
-- We may want to introduce some way to actually shut this down if, for example,
-- the object has not been accessed in a while (whatever that means).
--
-- Broken in ghci-7.6.1 Mac OS X due to bug #7299.
--
keepAlive :: a -> IO a
keepAlive x = forkIO (caffeine x) >> return x
  where
    caffeine hit = do threadDelay (5 * 1000 * 1000) -- microseconds = 5 seconds
                      caffeine hit


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
  create dev prp [CUDA.SchedAuto]


-- Debugging
-- ---------

-- Nicely format a summary of the selected CUDA device, example:
--
-- Device 0: GeForce 9600M GT (compute capability 1.1)
--           4 multiprocessors @ 1.25GHz (32 cores), 512MB global memory
--
deviceInfo :: CUDA.Device -> CUDA.DeviceProperties -> String
deviceInfo dev prp = render $ reset <>
  devID <> colon <+> vcat [ name <+> parens compute
                          , processors <+> at <+> text clock <+> parens cores <> comma <+> memory
                          ]
  where
    name        = text (CUDA.deviceName prp)
    compute     = text "compute capatability" <+> text (show $ CUDA.computeCapability prp)
    devID       = text "Device" <+> int (fromIntegral $ CUDA.useDevice dev)     -- hax
    processors  = int (CUDA.multiProcessorCount prp)                              <+> text "multiprocessors"
    cores       = int (CUDA.multiProcessorCount prp * coresPerMultiProcessor prp) <+> text "cores"
    memory      = text mem <+> text "global memory"
    --
    clock       = Debug.showFFloatSIBase (Just 2) 1000 (fromIntegral $ CUDA.clockRate prp * 1000 :: Double) "Hz"
    mem         = Debug.showFFloatSIBase (Just 0) 1024 (fromIntegral $ CUDA.totalGlobalMem prp   :: Double) "B"
    at          = char '@'
    reset       = zeroWidthText "\r"

