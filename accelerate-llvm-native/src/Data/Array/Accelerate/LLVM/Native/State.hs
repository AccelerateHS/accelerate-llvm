{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.State
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.State (

  evalNative,
  createTarget, defaultTarget,

) where

import Data.Array.Accelerate.Debug.Internal

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Native.Execute.Scheduler
import qualified Data.Array.Accelerate.LLVM.Native.Link.Cache       as LC
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

import Data.Char
import Data.Maybe
import Formatting
import Language.Haskell.TH
import System.Environment
import System.IO.Unsafe
import Text.Read

import GHC.Conc
import GHC.Ptr


-- | Execute a computation in the Native backend
--
evalNative :: Native -> LLVM Native a -> IO a
-- evalNative = evalLLVM

-- XXX: This is correct for run, but for runN we'll use this operation to
-- do the compilation separately from execution, thus there will be an
-- empty "frame" with no (execution) trace
--
evalNative target acc = do
  let label = Ptr $(litE (stringPrimL (map (fromIntegral . ord) "Native.run\0")))
  emit_frame_mark_start label
  !result <- evalLLVM target acc
  emit_frame_mark_end label
  return result


-- | Create a Native execution target by spawning a worker thread on each of the
-- given capabilities.
--
createTarget
    :: [Int]              -- ^ CPUs to launch worker threads on
    -> IO Native
createTarget cpus = do
  gang    <- hireWorkersOn cpus
  linker  <- LC.new
  return  $! Native linker gang

{--
-- | The strategy for balancing work amongst the available worker threads.
--
type Strategy = Gang -> Executable


-- | Execute an operation sequentially on a single thread
--
sequentialIO :: Strategy
sequentialIO gang =
  Executable $ \name _ppt range fill ->
    timed name $ runSeqIO gang range fill


-- | Execute a computation without load balancing. Each thread computes an
-- equally sized chunk of the input. No work stealing occurs.
--
unbalancedParIO :: Strategy
unbalancedParIO gang =
  Executable $ \name _ppt range fill ->
    timed name $ runParIO Single.mkResource gang range fill


-- | Execute a computation where threads use work stealing (based on lazy
-- splitting of work stealing queues and exponential backoff) in order to
-- automatically balance the workload amongst themselves.
--
balancedParIO
    :: Int                -- ^ number of steal attempts before backing off
    -> Strategy
balancedParIO retries gang =
  Executable $ \name ppt range fill ->
    -- TLM: A suitable PPT should be chosen when invoking the continuation in
    --      order to balance scheduler overhead with fine-grained function calls
    --
    let resource = LBS.mkResource ppt (SMP.mkResource retries <> Backoff.mkResource)
    in  timed name $ runParIO resource gang range fill
--}


-- Top-level mutable state
-- -----------------------
--
-- It is important to keep some information alive for the entire run of the
-- program, not just a single execution. These tokens use 'unsafePerformIO' to
-- ensure they are executed only once, and reused for subsequent invocations.
--

-- | Initialise the gang of threads that will be used to execute computations.
-- This spawns one worker for each available processor, or as specified by the
-- value of the environment variable @ACCELERATE_LLVM_NATIVE_THREADS@.
--
-- This globally shared thread gang is auto-initialised on startup and shared by
-- all computations (unless the user chooses to 'run' with a different gang).
--
-- It does not help to have multiple gangs running at the same time, as then the
-- system as a whole may run slower as the threads contend for cache. The
-- scheduler is able to execute operations from multiple sources concurrently,
-- so multiple gangs should not be necessary.
--
{-# NOINLINE defaultTarget #-}
defaultTarget :: Native
defaultTarget = unsafePerformIO $ do
  nproc <- getNumProcessors
  ncaps <- getNumCapabilities
  menv  <- (readMaybe =<<) <$> lookupEnv "ACCELERATE_LLVM_NATIVE_THREADS"

  let nthreads = fromMaybe nproc menv

  -- Update the number of capabilities, but never set it lower than it already
  -- is. This target will spawn a worker on each processor (as returned by
  -- 'getNumProcessors', which includes SMT (hyperthreading) cores), but the
  -- user may have requested more capabilities than this to handle, for example,
  -- concurrent output.
  --
  setNumCapabilities (max ncaps nthreads)

  Debug.traceM Debug.dump_gc ("gc: initialise native target with " % int % " worker threads") nthreads
  createTarget [0 .. nthreads-1]


{--
-- Debugging
-- ---------

{-# INLINE timed #-}
timed :: ShortByteString -> IO a -> IO a
timed name f = Debug.timed Debug.dump_exec (elapsed name) f

{-# INLINE elapsed #-}
elapsed :: ShortByteString -> Double -> Double -> String
elapsed name x y = printf "exec: %s %s" (unpack name) (Debug.elapsedP x y)
--}

