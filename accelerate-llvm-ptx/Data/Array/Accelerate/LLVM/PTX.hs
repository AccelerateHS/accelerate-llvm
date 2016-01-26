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

  Arrays,

  -- ** Parallel execution
  run, run1, stream,

) where

-- accelerate
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Smart                      ( Acc )
import Data.Array.Accelerate.Array.Sugar                ( Arrays )
import Data.Array.Accelerate.Pretty                     ( Graph, graphDelayedAcc, graphDelayedAfun )

import Data.Array.Accelerate.LLVM.PTX.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Compile
import Data.Array.Accelerate.LLVM.PTX.Execute
import Data.Array.Accelerate.LLVM.PTX.State

#if ACCELERATE_DEBUG
import Data.Array.Accelerate.Debug                      as Debug

import Control.Exception                                ( bracket )
import System.IO                                        ( Handle, openTempFile, hPutStrLn, hPrint, hClose, stderr )
import System.FilePath                                  ( (</>) )
import System.Directory                                 ( getTemporaryDirectory, createDirectoryIfMissing )

#if   defined(UNIX)
import System.Posix.Process                             ( getProcessID )
#elif defined(WIN32)
import System.Win32.Process                             ( ProcessId )
#else
#error "I don't know what operating system I am"
#endif
#endif

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
run a = unsafePerformIO execute
  where
    !acc        = convertAccWith config a
    execute     = evalPTX defaultTarget (compileAcc acc >>= dumpStats >>= executeAcc >>= copyToHost)


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
run1 f = \a -> unsafePerformIO (execute a)
  where
    !acc        = convertAfunWith config f
    !afun       = unsafePerformIO $ evalPTX defaultTarget (compileAfun acc) >>= dumpStats
    execute a   = evalPTX defaultTarget (executeAfun1 afun a >>= copyToHost)


-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go.
--
stream :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> [a] -> [b]
stream f arrs = map go arrs
  where
    !go = run1 f


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
#if ACCELERATE_DEBUG
dumpStats next = do
  stats <- liftIO simplCount
  liftIO $ traceIO dump_simpl_stats (show stats)
  liftIO $ resetSimplCount
  return next
#else
dumpStats next = return next
#endif


-- Program execution/dependency graph
-- ----------------------------------

class Graphable g where
  toGraph :: Bool -> g -> Graph

instance Graphable (DelayedAcc a) where
  toGraph = graphDelayedAcc

instance Graphable (DelayedAfun a) where
  toGraph = graphDelayedAfun

dumpGraph :: (MonadIO m, Graphable g) => g -> m g
#if ACCELERATE_DEBUG
dumpGraph g = do
  liftIO $ do
    Debug.when dump_dot       $ writeGraph False g
    Debug.when dump_simpl_dot $ writeGraph True  g
  return g
#else
dumpGraph g = return g
#endif

#if ACCELERATE_DEBUG
writeGraph :: Graphable g => Bool -> g -> IO ()
writeGraph simple g = do
  withTemporaryFile "acc.dot" $ \path hdl -> do
    hPutStrLn stderr ("program graph: " ++ path)
    hPrint hdl (toGraph simple g)

withTemporaryFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTemporaryFile template go = do
  pid <- getProcessID
  tmp <- getTemporaryDirectory
  let dir = tmp </> "accelerate-llvm-native-" ++ show pid
  createDirectoryIfMissing True dir
  bracket (openTempFile dir template) (hClose . snd) (uncurry go)
#endif

#ifdef WIN32
getProcessID :: IO ProcessId
getProcessID = return 0xaaaa
#endif

