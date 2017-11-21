{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.State
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.State (

  evalPTX,
  createTargetForDevice, createTargetFromContext, defaultTarget, defaultTargets,

) where

-- accelerate
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.PTX.Pool                          ( Pool )
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Table         as MT
import qualified Data.Array.Accelerate.LLVM.PTX.Context             as CT
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Stream      as ST
import qualified Data.Array.Accelerate.LLVM.PTX.Link.Cache          as LC
import qualified Data.Array.Accelerate.LLVM.PTX.Pool                as Pool

import Data.Range.Range                                             ( Range(..) )
import Control.Parallel.Meta                                        ( Executable(..) )

-- standard library
import Control.Concurrent                                           ( runInBoundThread )
import Control.Exception                                            ( try, catch )
import Data.Maybe                                                   ( fromMaybe, catMaybes )
import System.Environment                                           ( lookupEnv )
import System.IO.Unsafe                                             ( unsafePerformIO, unsafeInterleaveIO )
import Text.Printf                                                  ( printf )
import Text.Read                                                    ( readMaybe )
import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver                                as CUDA
import qualified Foreign.CUDA.Driver.Context                        as Context


-- | Execute a PTX computation
--
evalPTX :: PTX -> LLVM PTX a -> IO a
evalPTX ptx acc =
  runInBoundThread (CT.withContext (ptxContext ptx) (evalLLVM ptx acc))
  `catch`
  \e -> $internalError "unhandled" (show (e :: CUDAException))


-- | Create a new PTX execution target for the given device
--
createTargetForDevice
    :: CUDA.Device
    -> CUDA.DeviceProperties
    -> [CUDA.ContextFlag]
    -> IO PTX
createTargetForDevice dev prp flags = do
  raw <- CUDA.create dev flags
  ptx <- createTarget dev prp raw
  _   <- CUDA.pop
  return ptx


-- | Create a PTX execute target for the given device context
--
createTargetFromContext
    :: CUDA.Context
    -> IO PTX
createTargetFromContext raw = do
  dev <- Context.device
  prp <- CUDA.props dev
  createTarget dev prp raw


-- | Create a PTX execution target
--
createTarget
    :: CUDA.Device
    -> CUDA.DeviceProperties
    -> CUDA.Context
    -> IO PTX
createTarget dev prp raw = do
  ctx <- CT.raw dev prp raw
  mt  <- MT.new ctx
  lc  <- LC.new
  st  <- ST.new ctx
  return $! PTX ctx mt lc st simpleIO


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
defaultTarget = unsafePerformIO $ do Pool.with defaultTargets return
{--
defaultTarget = unsafePerformIO $ do
  Debug.traceIO Debug.dump_gc "gc: initialise default PTX target"
  CUDA.initialise []
  (dev,prp,ctx) <- selectBestDevice
  ptx           <- createTarget dev prp ctx
  _             <- CUDA.pop
  return ptx
--}


-- | Create a shared resource pool of the available CUDA devices.
--
-- This globally shared resource pool is auto-initialised on startup. It will
-- consist of every currently available device, or those specified by the value
-- of the environment variable @ACCELERATE_LLVM_PTX_DEVICES@ (as a list of
-- device ordinals).
--
{-# NOINLINE defaultTargets #-}
defaultTargets :: Pool PTX
defaultTargets = unsafePerformIO $ do
  Debug.traceIO Debug.dump_gc "gc: initialise default PTX pool"
  CUDA.initialise []

  -- Figure out which GPUs we should put into the execution pool
  --
  ngpu  <- CUDA.count
  menv  <- (readMaybe =<<) <$> lookupEnv "ACCELERATE_LLVM_PTX_DEVICES"

  let using   = fromMaybe [0..ngpu-1] menv

      -- Spin up the GPU at the given ordinal. We do this lazily so that it
      -- happens the first time we try to execute something on the device.
      --
      -- On a system with multiple GPUs this prevents creating a context on
      -- every GPU because we may never actually use it (which might be
      -- problematic if the devices are in exclusive mode). On the down side,
      -- any initialisation overhead will occur the first time the context is
      -- used, rather than entirely upfront.
      --
      boot :: Int -> IO (Maybe PTX)
      boot i =
        unsafeInterleaveIO $ do
          dev <- CUDA.device 0
          prp <- CUDA.props dev
          r   <- try $ createTargetForDevice dev prp [CUDA.SchedAuto]
          case r of
            Right ptx               -> return (Just ptx)
            Left (e::CUDAException) -> do
              Debug.traceIO Debug.dump_gc (printf "gc: failed to initialise device %d: %s" i (show e))
              return Nothing

  gpus <- mapM boot using
  Pool.create (catMaybes gpus)

