{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements a backend for the /Accelerate/ language targeting
-- NVPTX for execution on NVIDIA GPUs. Expressions are on-line translated into
-- LLVM code, which is just-in-time executed in parallel on the GPU.
--

module Data.Array.Accelerate.LLVM.PTX (

  Acc, Arrays,

  -- * Synchronous execution
  run, runWith,
  run1, run1With,
  stream, streamWith,

  -- * Asynchronous execution
  Async,
  wait, poll, cancel,

  runAsync, runAsyncWith,
  run1Async, run1AsyncWith,

  -- * Execution targets
  PTX, createTargetForDevice, createTargetFromContext,

) where

-- accelerate
import Data.Array.Accelerate.Async
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Smart                      ( Acc )
import Data.Array.Accelerate.Array.Sugar                ( Arrays )
import Data.Array.Accelerate.Debug                      as Debug

import Data.Array.Accelerate.LLVM.PTX.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Compile
import Data.Array.Accelerate.LLVM.PTX.Execute
import Data.Array.Accelerate.LLVM.PTX.State
import Data.Array.Accelerate.LLVM.PTX.Target

-- standard library
import Control.Monad.Trans
import System.IO.Unsafe


-- Accelerate: LLVM backend for NVIDIA GPUs
-- ----------------------------------------

-- | Compile and run a complete embedded array program.
--
-- Note that it is recommended that you use 'run1' whenever possible.
--
run :: Arrays a => Acc a -> a
run = runWith defaultTarget


-- | As 'run', but execute using the specified target rather than using the
-- default, automatically selected device.
--
-- Contexts passed to this function may all target to the same device, or to
-- separate devices of differing compute capabilities.
--
runWith :: Arrays a => PTX -> Acc a -> a
runWith target a
  = unsafePerformIO
  $ wait =<< runAsyncWith target a


-- | As 'run', but run the computation asynchronously and return immediately
-- without waiting for the result. The status of the computation can be queried
-- using 'wait', 'poll', and 'cancel'.
--
-- Note that a CUDA context can be active on only one host thread at a time. If
-- you want to execute multiple computations in parallel, on the same or
-- different devices, use 'runAsyncWith'.
--
runAsync :: Arrays a => Acc a -> IO (Async a)
runAsync = runAsyncWith defaultTarget


-- | As 'runWith', but execute asynchronously. Be sure not to destroy the context,
-- or attempt to attach it to a different host thread, before all outstanding
-- operations have completed.
--
runAsyncWith :: Arrays a => PTX -> Acc a -> IO (Async a)
runAsyncWith target a = asyncBound execute
  where
    !acc        = convertAccWith config a
    execute     = dumpGraph acc >> evalPTX target (compileAcc acc >>= dumpStats >>= executeAcc >>= copyToHost)


-- | Prepare and execute an embedded array program of one argument.
--
-- This function can be used to improve performance in cases where the array
-- program is constant between invocations, because it enables us to bypass
-- front-end conversion stages and move directly to the execution phase. If you
-- have a computation applied repeatedly to different input data, use this,
-- specifying any changing aspects of the computation via the input parameter.
-- If the function is only evaluated once, this is equivalent to 'run'.
--
-- To use 'run1' effectively you must express your program as a function of one
-- argument. If your program takes more than one argument, you can use
-- 'Data.Array.Accelerate.lift' and 'Data.Array.Accelerate.unlift' to tuple up
-- the arguments.
--
-- At an example, once your program is expressed as a function of one argument,
-- instead of the usual:
--
-- > step :: Acc (Vector a) -> Acc (Vector b)
-- > step = ...
-- >
-- > simulate :: Vector a -> Vector b
-- > simulate xs = run $ step (use xs)
--
-- Instead write:
--
-- > simulate xs = run1 step xs
--
-- You can use the debugging options to check whether this is working
-- successfully by, for example, observing no output from the @-ddump-cc@ flag
-- at the second and subsequent invocations.
--
-- See the programs in the 'accelerate-examples' package for examples.
--
run1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b
run1 = run1With defaultTarget


-- | As 'run1', but execute using the specified target rather than using the
-- default, automatically selected device.
--
run1With :: (Arrays a, Arrays b) => PTX -> (Acc a -> Acc b) -> a -> b
run1With target f =
  let go = run1AsyncWith target f
  in \a -> unsafePerformIO $ wait =<< go a


-- | As 'run1', but the computation is executed asynchronously.
--
run1Async :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> IO (Async b)
run1Async = run1AsyncWith defaultTarget


-- | As 'run1With', but execute asynchronously.
--
run1AsyncWith :: (Arrays a, Arrays b) => PTX -> (Acc a -> Acc b) -> a -> IO (Async b)
run1AsyncWith target f = \a -> asyncBound (execute a)
  where
    !acc        = convertAfunWith config f
    !afun       = unsafePerformIO $ dumpGraph acc >> evalPTX target (compileAfun acc) >>= dumpStats
    execute a   = evalPTX target (executeAfun1 afun a >>= copyToHost)


-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go.
--
stream :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> [a] -> [b]
stream = streamWith defaultTarget


-- | As 'stream', but execute using the specified target.
--
streamWith :: (Arrays a, Arrays b) => PTX -> (Acc a -> Acc b) -> [a] -> [b]
streamWith target f arrs = map go arrs
  where
    !go = run1With target f


-- How the Accelerate program should be evaluated.
--
-- TODO: make sharing/fusion runtime configurable via debug flags or otherwise.
--
config :: Phase
config =  phases
  { convertOffsetOfSegment = True
  }


-- Debugging
-- =========

-- Compiler phase statistics
-- -------------------------

dumpStats :: MonadIO m => a -> m a
dumpStats next = dumpSimplStats >> return next

