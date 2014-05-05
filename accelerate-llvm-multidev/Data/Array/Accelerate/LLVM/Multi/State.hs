{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.State
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Multi.State (

  evalMulti,
  createTarget, defaultTarget

) where

-- accelerate
import Data.Array.Accelerate.Error

import Control.Parallel.Meta
import Control.Parallel.Meta.Worker
import qualified Control.Parallel.Meta.Trans.LBS                as LBS
import qualified Control.Parallel.Meta.Resource.SMP             as SMP
import qualified Control.Parallel.Meta.Resource.Single          as Single

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Multi.Target

import Data.Array.Accelerate.LLVM.PTX.Target                    ( PTX(..) )
import qualified Data.Array.Accelerate.LLVM.PTX.State           as PTX
import qualified Data.Array.Accelerate.LLVM.PTX.Target          as PTX
import qualified Data.Array.Accelerate.LLVM.PTX.Context         as Context

import Data.Array.Accelerate.LLVM.Native.Target                 ( Native(..) )
import qualified Data.Array.Accelerate.LLVM.Native.State        as CPU
import qualified Data.Array.Accelerate.LLVM.Native.Target       as CPU

import qualified Data.Array.Accelerate.LLVM.Debug               as Debug

-- cuda
import Foreign.CUDA.Driver.Error

-- standard library
import Data.Monoid
import Control.Exception                                        ( bracket_, catch )
import Control.Concurrent                                       ( runInBoundThread )
import System.Mem                                               ( performGC )
import System.IO.Unsafe                                         ( unsafePerformIO )


-- | Execute a computation in the Multi backend. Requires initialising the CUDA
-- environment; copied from that backend.
--
evalMulti :: Multi -> LLVM Multi a -> IO a
evalMulti multi acc =
  runInBoundThread (bracket_ setup teardown action)
  `catch`
  \e -> $internalError "unhandled" (show (e :: CUDAException))
  where
    setup       = Context.push (PTX.ptxContext (ptxTarget multi))
    teardown    = performGC >> Context.pop
    action      = evalLLVM multi acc


-- | Create a multi-device execution target by combining the given CPU and GPU
-- targets.
--
-- This spawns a thread to control each execution unit. A lazy binary splitting
-- work-stealing scheduler is used to balance the load amongst the available
-- processors. A suitable PPT should be chosen when invoking the continuation in
-- order to balance scheduler overhead with fine-grained function calls.
--
createTarget :: Native -> PTX -> IO Multi
createTarget native ptx = do

  -- Although we only support one GPU (at the moment), we still need to spawn a
  -- worker thread so that it can take part in work stealing.
  gpuGang <- forkGang 1
  cpuGang <- return (theGang native)

  let
      -- The basic resources for the CPU and GPU. As we don't currently support
      -- multiple GPUs, the lone GPU knows of no other sources of work.
      --
      gpuR      = Single.mkResource gpuGang
      cpuR      = SMP.mkResource (2 * gangSize cpuGang) cpuGang

      -- Construct the new Executable contexts for each backend, where the CPU
      -- can steal from the GPU, and vice-versa.
      --
      -- Note that each backend does not need to know about the PPT for the
      -- other: individual backends only need to know the best way to split the
      -- data for themselves (to balance the frequency of deque checks to doing
      -- useful work, or rather, how expensive it is to jump out of the
      -- execution function and back to the scheduler). When work is stolen from
      -- another processor, they steal the whole chunk, and then subdivide it
      -- based on their own PPT.
      --
      native'   = native { CPU.fillP = Executable $ \ppt range sync fill -> do
                              parIO (LBS.mkResource ppt cpuR <> gpuR) cpuGang range fill (runFinalise sync) }

      ptx'      = ptx    { PTX.fillP = Executable $ \ppt range sync fill ->
                              parIO (LBS.mkResource ppt gpuR <> cpuR) gpuGang range fill (runFinalise sync) }
  --
  return $! Multi ptx' native'


-- Top-level mutable state
-- -----------------------
--
-- It is important to keep some information alive for the entire run of the
-- program, not just a single execution. These tokens use 'unsafePerformIO' to
-- ensure they are executed only once, and reused for subsequent invocations.
--

-- | Initialise the CPU threads and GPUs that will be used to execute
-- computations. This spawns one worker on each capability, which can be set via
-- +RTS -Nn.
--
-- This globally shared target is auto-initialised on startup and used by
-- default for all computations.
--
{-# NOINLINE defaultTarget #-}
defaultTarget :: Multi
defaultTarget = unsafePerformIO $ do
  Debug.message Debug.dump_gc "gc: initialise default multi target"
  createTarget CPU.defaultTarget PTX.defaultTarget

