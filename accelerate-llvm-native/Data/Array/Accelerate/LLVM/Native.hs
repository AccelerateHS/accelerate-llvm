{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements a backend for the /Accelerate/ language targeting
-- multicore CPUs. Expressions are on-line translated into LLVM code, which is
-- just-in-time executed in parallel over the available CPUs. Functions are
-- automatically parallel, provided you specify '+RTS -Nwhatever' on the command
-- line when running the program.
--

module Data.Array.Accelerate.LLVM.Native (

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
  Native, Strategy,
  createTarget, balancedParIO, unbalancedParIO,

) where

-- accelerate
import Data.Array.Accelerate.Async
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Array.Sugar                            ( Arrays )
import Data.Array.Accelerate.Smart                                  ( Acc )
import Data.Array.Accelerate.LLVM.Native.Debug                      as Debug

import Data.Array.Accelerate.LLVM.Native.Compile                    ( compileAcc, compileAfun )
import Data.Array.Accelerate.LLVM.Native.Execute                    ( executeAcc, executeAfun1 )
import Data.Array.Accelerate.LLVM.Native.State
import Data.Array.Accelerate.LLVM.Native.Target

-- standard library
import Control.Monad.Trans
import System.IO.Unsafe
import Text.Printf


-- Accelerate: LLVM backend for multicore CPUs
-- -------------------------------------------

-- | Compile and run a complete embedded array program.
--
-- NOTE: it is recommended to use 'run1' whenever possible.
--
run :: Arrays a => Acc a -> a
run = runWith defaultTarget

-- | As 'run', but execute using the specified target (thread gang).
--
runWith :: Arrays a => Native -> Acc a -> a
runWith target a = unsafePerformIO (run' target a)

-- | As 'run', but allow the computation to run asynchronously and return
-- immediately without waiting for the result. The status of the computation can
-- be queried using 'wait', 'poll', and 'cancel'.
--
runAsync :: Arrays a => Acc a -> IO (Async a)
runAsync = runAsyncWith defaultTarget

-- | As 'runAsync', but execute using the specified target (thread gang).
--
runAsyncWith :: Arrays a => Native -> Acc a -> IO (Async a)
runAsyncWith target a = async (run' target a)

run' :: Arrays a => Native -> Acc a -> IO a
run' target a = execute
  where
    !acc        = convertAccWith (config target) a
    execute     = do
      dumpGraph acc
      evalNative target $ do
        exec <- phase "compile" elapsedS (compileAcc acc) >>= dumpStats
        res  <- phase "execute" elapsedP (executeAcc exec)
        return res


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

-- | As 'run1', but execute using the specified target (thread gang).
--
run1With :: (Arrays a, Arrays b) => Native -> (Acc a -> Acc b) -> a -> b
run1With = run1' unsafePerformIO

-- | As 'run1', but execute asynchronously.
--
run1Async :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> IO (Async b)
run1Async = run1AsyncWith defaultTarget

-- | As 'run1Async', but execute using the specified target (thread gang).
--
run1AsyncWith :: (Arrays a, Arrays b) => Native -> (Acc a -> Acc b) -> a -> IO (Async b)
run1AsyncWith = run1' async

run1' :: (Arrays a, Arrays b) => (IO b -> c) -> Native -> (Acc a -> Acc b) -> a -> c
run1' using target f = \a -> using (execute a)
  where
    !acc        = convertAfunWith (config target) f
    !afun       = unsafePerformIO $ do
                    dumpGraph acc
                    phase "compile" elapsedS (evalNative target (compileAfun acc)) >>= dumpStats
    execute a   =   phase "execute" elapsedP (evalNative target (executeAfun1 afun a))


-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go.
--
stream :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> [a] -> [b]
stream = streamWith defaultTarget

-- | As 'stream', but execute using the specified target (thread gang).
--
streamWith :: (Arrays a, Arrays b) => Native -> (Acc a -> Acc b) -> [a] -> [b]
streamWith target f arrs = map go arrs
  where
    !go = run1With target f


-- How the Accelerate program should be evaluated.
--
-- TODO: make sharing/fusion runtime configurable via debug flags or otherwise.
--
config :: Native -> Phase
config target = phases
  { convertOffsetOfSegment = gangSize target > 1
  }


-- Debugging
-- =========

dumpStats :: MonadIO m => a -> m a
dumpStats x = dumpSimplStats >> return x

phase :: MonadIO m => String -> (Double -> Double -> String) -> m a -> m a
phase n fmt go = timed dump_phases (\wall cpu -> printf "phase %s: %s" n (fmt wall cpu)) go

