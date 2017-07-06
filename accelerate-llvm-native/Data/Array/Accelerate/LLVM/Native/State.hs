{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.State
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.State (

  evalNative,
  createTarget, defaultTarget,

  Strategy,
  balancedParIO, unbalancedParIO,

) where

-- accelerate
import Control.Parallel.Meta
import Control.Parallel.Meta.Worker
import qualified Control.Parallel.Meta.Trans.LBS                as LBS
import qualified Control.Parallel.Meta.Resource.SMP             as SMP
import qualified Control.Parallel.Meta.Resource.Single          as Single
import qualified Control.Parallel.Meta.Resource.Backoff         as Backoff

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Native.Target
import qualified Data.Array.Accelerate.LLVM.Native.Link.Cache   as LC
import qualified Data.Array.Accelerate.LLVM.Native.Debug        as Debug

-- library
import Data.Monoid
import System.IO.Unsafe
import Text.Printf
import Data.ByteString.Short.Char8                              ( ShortByteString, unpack )

import GHC.Conc


-- | Execute a computation in the Native backend
--
evalNative :: Native -> LLVM Native a -> IO a
evalNative = evalLLVM


-- | Create a Native execution target by spawning a worker thread on each of the
-- given capabilities, and using the given strategy to load balance the workers
-- when executing parallel operations.
--
createTarget
    :: [Int]              -- ^ CPU IDs to launch worker threads on
    -> Strategy           -- ^ Strategy to balance parallel workloads
    -> IO Native
createTarget caps parallelIO = do
  gang   <- forkGangOn caps
  linker <- LC.new
  return $! Native (length caps) linker (sequentialIO gang) (parallelIO gang)


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


-- Top-level mutable state
-- -----------------------
--
-- It is important to keep some information alive for the entire run of the
-- program, not just a single execution. These tokens use 'unsafePerformIO' to
-- ensure they are executed only once, and reused for subsequent invocations.
--

-- | Initialise the gang of threads that will be used to execute computations.
-- This spawns one worker on each capability, which can be set via +RTS -Nn.
--
-- This globally shared thread gang is auto-initialised on startup and shared by
-- all computations (unless the user chooses to 'run' with a different gang).
--
-- In a data parallel setting, it does not help to have multiple gangs running
-- at the same time. This is because a single data parallel computation should
-- already be able to keep all threads busy. If we had multiple gangs running at
-- the same time, then the system as a whole would run slower as the gangs
-- contend for cache and thrash the scheduler.
--
{-# NOINLINE defaultTarget #-}
defaultTarget :: Native
defaultTarget = unsafePerformIO $ do
  Debug.traceIO Debug.dump_gc (printf "gc: initialise native target with %d CPUs" numCapabilities)
  case numCapabilities of
    1 -> createTarget [0]        sequentialIO
    n -> createTarget [0 .. n-1] (balancedParIO n)


-- Debugging
-- ---------

{-# INLINE timed #-}
timed :: ShortByteString -> IO a -> IO a
timed name f = Debug.timed Debug.dump_exec (elapsed name) f

{-# INLINE elapsed #-}
elapsed :: ShortByteString -> Double -> Double -> String
elapsed name x y = printf "exec: %s %s" (unpack name) (Debug.elapsedP x y)

