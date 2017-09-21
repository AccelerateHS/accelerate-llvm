{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX
-- Copyright   : [2014..2017] Trevor L. McDonell
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
  runN, runNWith,
  stream, streamWith,

  -- * Asynchronous execution
  Async,
  wait, poll, cancel,

  runAsync, runAsyncWith,
  run1Async, run1AsyncWith,
  runNAsync, runNAsyncWith,

  -- * Ahead-of-time compilation
  runQ, runQWith,
  runQAsync, runQAsyncWith,

  -- * Execution targets
  PTX, createTargetForDevice, createTargetFromContext,

  -- * Controlling host-side allocation
  registerPinnedAllocator, registerPinnedAllocatorWith,

) where

-- accelerate
import Data.Array.Accelerate.AST                                    ( PreOpenAfun(..) )
import Data.Array.Accelerate.Array.Sugar                            ( Arrays )
import Data.Array.Accelerate.Async
import Data.Array.Accelerate.Debug                                  as Debug
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Smart                                  ( Acc )
import Data.Array.Accelerate.Trafo

import Data.Array.Accelerate.LLVM.Execute.Async                     ( AsyncR(..) )
import Data.Array.Accelerate.LLVM.Execute.Environment               ( AvalR(..) )
import Data.Array.Accelerate.LLVM.PTX.Compile
import Data.Array.Accelerate.LLVM.PTX.Embed                         ( embedOpenAcc )
import Data.Array.Accelerate.LLVM.PTX.Execute
import Data.Array.Accelerate.LLVM.PTX.Execute.Environment           ( Aval )
import Data.Array.Accelerate.LLVM.PTX.Link
import Data.Array.Accelerate.LLVM.PTX.State
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.State
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Data          as AD
import qualified Data.Array.Accelerate.LLVM.PTX.Context             as CT
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Async       as E

import Foreign.CUDA.Driver                                          as CUDA ( CUDAException, mallocHostForeignPtr )

-- standard library
import Data.Typeable
import Control.Exception
import Control.Monad.Trans
import System.IO.Unsafe
import Text.Printf
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH


-- Accelerate: LLVM backend for NVIDIA GPUs
-- ----------------------------------------

-- | Compile and run a complete embedded array program.
--
-- The result is copied back to the host only once the arrays are demanded (or
-- the result is forced to normal form). For results consisting of multiple
-- components (a tuple of arrays or array of tuples) this applies per primitive
-- array. Evaluating the result of 'run' to WHNF will initiate the computation,
-- but does not copy the results back from the device.
--
-- /NOTE:/ it is recommended to use 'runN' or 'runQ' whenever possible.
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
    execute     = do
      dumpGraph acc
      evalPTX target $ do
        acc `seq` dumpSimplStats
        build <- phase "compile" (compileAcc acc)
        exec  <- phase "link"    (linkAcc build)
        res   <- phase "execute" (executeAcc exec >>= AD.copyToHostLazy)
        return res


-- | This is 'runN', specialised to an array program of one argument.
--
run1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b
run1 = run1With defaultTarget

-- | As 'run1', but execute using the specified target rather than using the
-- default, automatically selected device.
--
run1With :: (Arrays a, Arrays b) => PTX -> (Acc a -> Acc b) -> a -> b
run1With = runNWith


-- | Prepare and execute an embedded array program.
--
-- This function can be used to improve performance in cases where the array
-- program is constant between invocations, because it enables us to bypass
-- front-end conversion stages and move directly to the execution phase. If you
-- have a computation applied repeatedly to different input data, use this,
-- specifying any changing aspects of the computation via the input parameters.
-- If the function is only evaluated once, this is equivalent to 'run'.
--
-- In order to use 'runN' you must express your Accelerate program as a function
-- of array terms:
--
-- > f :: (Arrays a, Arrays b, ... Arrays c) => Acc a -> Acc b -> ... -> Acc c
--
-- This function then returns the compiled version of 'f':
--
-- > runN f :: (Arrays a, Arrays b, ... Arrays c) => a -> b -> ... -> c
--
-- At an example, rather than:
--
-- > step :: Acc (Vector a) -> Acc (Vector b)
-- > step = ...
-- >
-- > simulate :: Vector a -> Vector b
-- > simulate xs = run $ step (use xs)
--
-- Instead write:
--
-- > simulate = runN step
--
-- You can use the debugging options to check whether this is working
-- successfully. For example, running with the @-ddump-phases@ flag should show
-- that the compilation steps only happen once, not on the second and subsequent
-- invocations of 'simulate'. Note that this typically relies on GHC knowing
-- that it can lift out the function returned by 'runN' and reuse it.
--
-- As with 'run', the resulting array(s) are only copied back to the host once
-- they are actually demanded (forced to normal form). Thus, splitting a program
-- into multiple 'runN' steps does not imply transferring intermediate
-- computations back and forth between host and device. However note that
-- Accelerate is not able to optimise (fuse) across separate 'runN' invocations.
--
-- See the programs in the 'accelerate-examples' package for examples.
--
-- See also 'runQ', which compiles the Accelerate program at _Haskell_ compile
-- time, thus eliminating the runtime overhead altogether.
--
runN :: Afunction f => f -> AfunctionR f
runN = runNWith defaultTarget

-- | As 'runN', but execute using the specified target device.
--
runNWith :: Afunction f => PTX -> f -> AfunctionR f
runNWith target f = exec
  where
    !acc  = convertAfunWith config f
    !afun = unsafePerformIO $ do
              dumpGraph acc
              evalPTX target $ do
                build <- phase "compile" (compileAfun acc) >>= dumpStats
                link  <- phase "link"    (linkAfun build)
                return link
    !exec = go afun (return Aempty)

    go :: ExecOpenAfun PTX aenv t -> LLVM PTX (Aval aenv) -> t
    go (Alam l) k = \arrs ->
      let k' = do aenv       <- k
                  AsyncR _ a <- E.async (AD.useRemoteAsync arrs)
                  return (aenv `Apush` a)
      in go l k'
    go (Abody b) k = unsafePerformIO . phase "execute" . evalPTX target $ do
      aenv <- k
      r    <- E.async (executeOpenAcc b aenv)
      AD.copyToHostLazy =<< E.get r


-- | As 'run1', but the computation is executed asynchronously.
--
run1Async :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> IO (Async b)
run1Async = run1AsyncWith defaultTarget

-- | As 'run1With', but execute asynchronously.
--
run1AsyncWith :: (Arrays a, Arrays b) => PTX -> (Acc a -> Acc b) -> a -> IO (Async b)
run1AsyncWith = runNAsyncWith


-- | As 'runN', but execute asynchronously.
--
runNAsync :: (Afunction f, RunAsync r, AfunctionR f ~ RunAsyncR r) => f -> r
runNAsync = runNAsyncWith defaultTarget

-- | As 'runNWith', but execute asynchronously.
--
runNAsyncWith :: (Afunction f, RunAsync r, AfunctionR f ~ RunAsyncR r) => PTX -> f -> r
runNAsyncWith target f = runAsync' target afun (return Aempty)
  where
    !acc  = convertAfunWith config f
    !afun = unsafePerformIO $ do
              dumpGraph acc
              evalPTX target $ do
                build <- phase "compile" (compileAfun acc) >>= dumpStats
                exec  <- phase "link"    (linkAfun build)
                return exec

class RunAsync f where
  type RunAsyncR f
  runAsync' :: PTX -> ExecOpenAfun PTX aenv (RunAsyncR f) -> LLVM PTX (Aval aenv) -> f

instance RunAsync b => RunAsync (a -> b) where
  type RunAsyncR (a -> b) = a -> RunAsyncR b
  runAsync' _      Abody{}  _ _    = error "runAsync: function oversaturated"
  runAsync' target (Alam l) k arrs =
    let k' = do aenv       <- k
                AsyncR _ a <- E.async (AD.useRemoteAsync arrs)
                return (aenv `Apush` a)
    in runAsync' target l k'

instance RunAsync (IO (Async b)) where
  type RunAsyncR  (IO (Async b)) = b
  runAsync' _      Alam{}    _ = error "runAsync: function not fully applied"
  runAsync' target (Abody b) k = asyncBound . phase "execute" . evalPTX target $ do
    aenv <- k
    r    <- E.async (executeOpenAcc b aenv)
    AD.copyToHostLazy =<< E.get r


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


-- | Ahead-of-time compilation for an embedded array program.
--
-- This function will generate, compile, and link into the final executable,
-- code to execute the given Accelerate computation /at Haskell compile time/.
-- This eliminates any runtime overhead associated with the other @run*@
-- operations. The generated code will be compiled for the current (default) GPU
-- architecture.
--
-- Since the Accelerate program will be generated at Haskell compile time,
-- construction of the Accelerate program, in particular via meta-programming,
-- will be limited to operations available to that phase. Also note that any
-- arrays which are embedded into the program via 'Data.Array.Accelerate.use'
-- will be stored as part of the final executable.
--
-- Usage of this function in your program is similar to that of 'runN'. First,
-- express your Accelerate program as a function of array terms:
--
-- > f :: (Arrays a, Arrays b, ... Arrays c) => Acc a -> Acc b -> ... -> Acc c
--
-- This function then returns a compiled version of @f@ as a Template Haskell
-- splice, to be added into your program at Haskell compile time:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > f' :: a -> b -> ... -> c
-- > f' = $( runQ f )
--
-- Note that at the splice point the usage of @f@ must monomorphic; i.e. the
-- types @a@, @b@ and @c@ must be at some known concrete type.
--
-- See the <https://github.com/tmcdonell/lulesh-accelerate lulesh-accelerate>
-- project for an example.
--
-- [/Note:/]
--
-- Due to <https://ghc.haskell.org/trac/ghc/ticket/13587 GHC#13587>, this
-- currently must be as an /untyped/ splice.
--
-- The correct type of this function is similar to that of 'runN':
--
-- > runQ :: Afunction f => f -> Q (TExp (AfunctionR f))
--
-- @since 1.1.0.0
--
runQ :: Afunction f => f -> TH.ExpQ
runQ = runQ' [| unsafePerformIO |] [| defaultTarget |]

-- | Ahead-of-time analogue of 'runNWith'. See 'runQ' for more information.
--
-- /NOTE:/ The supplied (at runtime) target must be compatible with the
-- architecture that this function was compiled for (the 'defaultTarget' of the
-- compiling machine). Running on a device with the same compute capability is
-- best, but this should also be forward compatible to newer architectures.
--
-- The correct type of this function is:
--
-- > runQWith :: Afunction f => f -> Q (TExp (PTX -> AfunctionR f))
--
-- @since 1.1.0.0
--
runQWith :: Afunction f => f -> TH.ExpQ
runQWith f = do
  target <- TH.newName "target"
  TH.lamE [TH.varP target] (runQ' [| unsafePerformIO |] (TH.varE target) f)


-- | Ahead-of-time analogue of 'runNAsync'. See 'runQ' for more information.
--
-- The correct type of this function is:
--
-- > runQAsync :: (Afunction f, RunAsync r, AfunctionR f ~ RunAsyncR r) => f -> Q (TExp r)
--
-- @since 1.1.0.0
--
runQAsync :: Afunction f => f -> TH.ExpQ
runQAsync = runQ' [| async |] [| defaultTarget |]

-- | Ahead-of-time analogue of 'runNAsyncWith'. See 'runQWith' for more information.
--
-- The correct type of this function is:
--
-- > runQAsyncWith :: (Afunction f, RunAsync r, AfunctionR f ~ RunAsyncR r) => f -> Q (TExp (PTX -> r))
--
-- @since 1.1.0.0
--
runQAsyncWith :: Afunction f => f -> TH.ExpQ
runQAsyncWith f = do
  target <- TH.newName "target"
  TH.lamE [TH.varP target] (runQ' [| async |] (TH.varE target) f)


runQ' :: Afunction f => TH.ExpQ -> TH.ExpQ -> f -> TH.ExpQ
runQ' using target f = do
  afun  <- let acc = convertAfunWith config f
           in  TH.runIO $ do
                 dumpGraph acc
                 evalPTX defaultTarget $
                   phase "compile" (compileAfun acc) >>= dumpStats
  let
      go :: Typeable aenv => CompiledOpenAfun PTX aenv t -> [TH.PatQ] -> [TH.ExpQ] -> [TH.StmtQ] -> TH.ExpQ
      go (Alam lam) xs as stmts = do
        x <- TH.newName "x" -- lambda bound variable
        a <- TH.newName "a" -- local array name
        s <- TH.bindS (TH.conP 'AsyncR [TH.wildP, TH.varP a]) [| E.async (AD.useRemoteAsync $(TH.varE x)) |]
        go lam (TH.varP x : xs) (TH.varE a : as) (return s : stmts)

      go (Abody body) xs as stmts =
        let aenv = foldr (\a gamma -> [| $gamma `Apush` $a |] ) [| Aempty |] as
            eval = TH.noBindS [| AD.copyToHostLazy =<< E.get =<< E.async (executeOpenAcc $(TH.unTypeQ (embedOpenAcc defaultTarget body)) $aenv) |]
        in
        TH.lamE (reverse xs) [| $using . phase "execute" . evalPTX $target $
                                  $(TH.doE (reverse (eval : stmts))) |]
  --
  go afun [] [] []


-- How the Accelerate program should be evaluated.
--
-- TODO: make sharing/fusion runtime configurable via debug flags or otherwise.
--
config :: Phase
config =  phases
  { convertOffsetOfSegment = True
  }


-- Controlling host-side allocation
-- --------------------------------

-- | Configure the default execution target to allocate all future host-side
-- arrays using (CUDA) pinned memory. Any newly allocated arrays will be
-- page-locked and directly accessible from the device, enabling high-speed
-- (asynchronous) DMA.
--
-- Note that since the amount of available pageable memory will be reduced,
-- overall system performance can suffer.
--
registerPinnedAllocator :: IO ()
registerPinnedAllocator = registerPinnedAllocatorWith defaultTarget


-- | As with 'registerPinnedAllocator', but configure the given execution
-- context.
--
registerPinnedAllocatorWith :: PTX -> IO ()
registerPinnedAllocatorWith target =
  AD.registerForeignPtrAllocator $ \bytes ->
    CT.withContext (ptxContext target) (CUDA.mallocHostForeignPtr [] bytes)
    `catch`
    \e -> $internalError "registerPinnedAlocator" (show (e :: CUDAException))


-- Debugging
-- =========

dumpStats :: MonadIO m => a -> m a
dumpStats x = dumpSimplStats >> return x

phase :: MonadIO m => String -> m a -> m a
phase n go = timed dump_phases (\wall cpu -> printf "phase %s: %s" n (elapsed wall cpu)) go

