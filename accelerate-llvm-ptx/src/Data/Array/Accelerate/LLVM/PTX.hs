{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements a backend for the /Accelerate/ language targeting
-- NVPTX for execution on NVIDIA GPUs. Expressions are on-line translated into
-- LLVM code, which is just-in-time executed in parallel on the GPU.
--

module Data.Array.Accelerate.LLVM.PTX (

  Acc, Arrays,
  Afunction, AfunctionR,

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
  registerPinnedAllocatorWith,

) where

-- accelerate
import Data.Array.Accelerate.AST                                    ( PreOpenAfun(..), arraysRepr, liftLHS, liftArraysR, lhsToArraysR )
import Data.Array.Accelerate.Array.Sugar                            ( Arrays, ArrRepr, arrays, toArr, fromArr )
import Data.Array.Accelerate.Async                                  ( Async, asyncBound, wait, poll, cancel )
import Data.Array.Accelerate.Error                                  ( internalError )
import Data.Array.Accelerate.Smart                                  ( Acc )
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Debug                                  as Debug

import Data.Array.Accelerate.LLVM.PTX.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Compile
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Embed
import Data.Array.Accelerate.LLVM.PTX.Execute
import Data.Array.Accelerate.LLVM.PTX.Execute.Async                 ( Par, evalPar, getArrays )
import Data.Array.Accelerate.LLVM.PTX.Execute.Environment
import Data.Array.Accelerate.LLVM.PTX.Link
import Data.Array.Accelerate.LLVM.PTX.State
import Data.Array.Accelerate.LLVM.PTX.Target

import Foreign.CUDA.Driver                                          as CUDA ( CUDAException, mallocHostForeignPtr )

-- standard library
import Control.Exception
import Control.Monad.Trans
import Data.Maybe
import Data.Typeable
import System.IO.Unsafe
import Text.Printf
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH


-- Accelerate: LLVM backend for NVIDIA GPUs
-- ----------------------------------------

-- | Compile and run a complete embedded array program.
--
-- This will execute using the first available CUDA device. If you wish to run
-- on a specific device, use 'runWith'.
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
run a = unsafePerformIO (runIO a)

-- | As 'run', but execute using the specified target rather than using the
-- default, automatically selected device.
--
runWith :: Arrays a => PTX -> Acc a -> a
runWith target a = unsafePerformIO (runWithIO target a)

-- | As 'run', but run the computation asynchronously and return immediately
-- without waiting for the result. The status of the computation can be queried
-- using 'wait', 'poll', and 'cancel'.
--
-- This will run on the first available CUDA device. If you wish to run on
-- a specific device, use 'runAsyncWith'.
--
runAsync :: Arrays a => Acc a -> IO (Async a)
runAsync a = asyncBound (runIO a)

-- | As 'runWith', but execute asynchronously. Be sure not to destroy the context,
-- or attempt to attach it to a different host thread, before all outstanding
-- operations have completed.
--
runAsyncWith :: Arrays a => PTX -> Acc a -> IO (Async a)
runAsyncWith target a = asyncBound (runWithIO target a)


runIO :: Arrays a => Acc a -> IO a
runIO a = withPool defaultTargetPool (\target -> runWithIO target a)

runWithIO :: forall a. Arrays a => PTX -> Acc a -> IO a
runWithIO target a = execute
  where
    !acc    = convertAcc a
    execute = do
      dumpGraph acc
      evalPTX target $ do
        build <- phase "compile" (compileAcc acc) >>= dumpStats
        exec  <- phase "link"    (linkAcc build)
        res   <- phase "execute" (evalPar (executeAcc exec >>= copyToHostLazy (arrays @a)))
        return $ toArr res


-- | This is 'runN', specialised to an array program of one argument.
--
run1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b
run1 = runN

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
runN :: forall f. Afunction f => f -> AfunctionR f
runN f = exec
  where
    !acc  = convertAfun f
    !exec = unsafeWithPool defaultTargetPool
          $ \target -> fromJust (lookup (ptxContext target) afun)

    -- Lazily cache the compiled function linked for each execution context.
    -- This includes specialisation for different compute capabilities and
    -- device-side memory management.
    --
    -- Perhaps this implicit version of 'runN' is not a good idea then, because
    -- we might need to migrate data between devices between iterations
    -- depending on which GPU gets scheduled.
    --
    !afun = flip map (unmanaged defaultTargetPool)
          $ \target -> (ptxContext target, runNWith' @f target acc)


-- | As 'runN', but execute using the specified target device.
--
runNWith :: forall f. Afunction f => PTX -> f -> AfunctionR f
runNWith target f = exec
  where
    !acc  = convertAfun f
    !exec = runNWith' @f target acc

runNWith' :: forall f. Afunction f => PTX -> DelayedAfun (AreprFunctionR f) -> AfunctionR f
runNWith' target acc = go (afunctionRepr @f) afun (return Empty)
  where
    !afun = unsafePerformIO $ do
              dumpGraph acc
              evalPTX target $ do
                build <- phase "compile" (compileAfun acc) >>= dumpStats
                link  <- phase "link"    (linkAfun build)
                return link

    go :: forall aenv t tf trepr. AfunctionRepr t tf trepr -> ExecOpenAfun PTX aenv trepr -> Par PTX (Val aenv) -> tf
    go (AfunctionReprLam repr) (Alam lhs l) k = \ !arrs ->
      let k' = do aenv <- k
                  a    <- useRemoteAsync (lhsToArraysR lhs) $ fromArr arrs
                  return (aenv `push` (lhs, a))
      in go repr l k'
    go AfunctionReprBody (Abody b) k = unsafePerformIO . phase "execute" . evalPTX target . evalPar $ do
      aenv <- k
      fut  <- executeOpenAcc b aenv
      toArr <$> copyToHostLazy (arrays @tf) fut
    go _ _ _ = error "But that's not right, oh, no, what's the story?"


-- | As 'run1', but the computation is executed asynchronously.
--
run1Async :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> IO (Async b)
run1Async = runNAsync

-- | As 'run1With', but execute asynchronously.
--
run1AsyncWith :: (Arrays a, Arrays b) => PTX -> (Acc a -> Acc b) -> a -> IO (Async b)
run1AsyncWith = runNAsyncWith


-- | As 'runN', but execute asynchronously.
--
runNAsync :: (Afunction f, RunAsync r, AreprFunctionR f ~ RunAsyncR r) => f -> r
runNAsync f = exec
  where
    !acc  = convertAfun f
    !exec = unsafeWithPool defaultTargetPool
          $ \target -> fromJust (lookup (ptxContext target) afun)

    !afun = flip map (unmanaged defaultTargetPool)
          $ \target -> (ptxContext target, runNAsyncWith' target acc)


-- | As 'runNWith', but execute asynchronously.
--
runNAsyncWith :: (Afunction f, RunAsync r, AreprFunctionR f ~ RunAsyncR r) => PTX -> f -> r
runNAsyncWith target f = exec
  where
    !acc  = convertAfun f
    !exec = runNAsyncWith' target acc

runNAsyncWith' :: RunAsync f => PTX -> DelayedAfun (RunAsyncR f) -> f
runNAsyncWith' target acc = exec
  where
    !afun = unsafePerformIO $ do
              dumpGraph acc
              evalPTX target $ do
                build <- phase "compile" (compileAfun acc) >>= dumpStats
                link  <- phase "link"    (linkAfun build)
                return link
    !exec = runAsync' target afun (return Empty)

class RunAsync f where
  type RunAsyncR f
  runAsync' :: PTX -> ExecOpenAfun PTX aenv (RunAsyncR f) -> Par PTX (Val aenv) -> f

instance (Arrays a, RunAsync b) => RunAsync (a -> b) where
  type RunAsyncR (a -> b) = ArrRepr a -> RunAsyncR b
  runAsync' _      Abody{}  _ _     = error "runAsync: function oversaturated"
  runAsync' target (Alam lhs l) k !arrs =
    let k' = do aenv  <- k
                a     <- useRemoteAsync (arrays @a) $ fromArr arrs
                return (aenv `push` (lhs, a))
    in runAsync' target l k'

instance Arrays b => RunAsync (IO (Async b)) where
  type RunAsyncR (IO (Async b)) = ArrRepr b
  runAsync' _      Alam{}    _ = error "runAsync: function not fully applied"
  runAsync' target (Abody b) k = asyncBound . phase "execute" . evalPTX target . evalPar $ do
    aenv <- k
    ans  <- executeOpenAcc b aenv
    arrs  <- getArrays (arraysRepr b) ans
    return $ toArr arrs


-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go.
--
stream :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> [a] -> [b]
stream f arrs = map go arrs
  where
    !go = run1 f

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
runQ = runQ' [| unsafePerformIO |]

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
  TH.lamE [TH.varP target] (runQWith' [| unsafePerformIO |] (TH.varE target) f)


-- | Ahead-of-time analogue of 'runNAsync'. See 'runQ' for more information.
--
-- The correct type of this function is:
--
-- > runQAsync :: (Afunction f, RunAsync r, AfunctionR f ~ RunAsyncR r) => f -> Q (TExp r)
--
-- @since 1.1.0.0
--
runQAsync :: Afunction f => f -> TH.ExpQ
runQAsync = runQ' [| asyncBound |]

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
  TH.lamE [TH.varP target] (runQWith' [| asyncBound |] (TH.varE target) f)


runQ' :: Afunction f => TH.ExpQ -> f -> TH.ExpQ
runQ' using = runQ'_ using (\go -> [| withPool defaultTargetPool (\target -> evalPTX target (evalPar $go)) |])

runQWith' :: Afunction f => TH.ExpQ -> TH.ExpQ -> f -> TH.ExpQ
runQWith' using target = runQ'_ using (\go -> [| evalPTX $target (evalPar $go) |])

-- Generate a template haskell expression for the given function to be embedded
-- into the current program. The supplied continuation specifies how to execute
-- the given body expression (e.g. using 'evalPTX')
--
-- NOTE:
--
--  * Can we do this without requiring an active GPU context? This should be
--    possible with only the DeviceProperties, but we would have to be a little
--    careful if we pass invalid values for the other state components. If we
--    attempt this, at minimum we need to parse the generated .sass to extract
--    resource usage information, rather than loading the module and probing
--    directly.
--
--  * What happens if we execute this code on a different architecture revision?
--    With runN this will automatically be recompiled for each new architecture
--    (at runtime).
--
runQ'_ :: Afunction f => TH.ExpQ -> (TH.ExpQ -> TH.ExpQ) -> f -> TH.ExpQ
runQ'_ using k f = do
  afun  <- let acc = convertAfun f
           in  TH.runIO $ do
                 dumpGraph acc
                 evalPTX defaultTarget $
                   phase "compile" (compileAfun acc) >>= dumpStats
  let
      go :: Typeable aenv => CompiledOpenAfun PTX aenv t -> [TH.PatQ] -> [TH.ExpQ] -> [TH.StmtQ] -> TH.ExpQ
      go (Alam lhs l) xs as stmts | HasTypeable <- lhsTypeable lhs = do
        x <- TH.newName "x" -- lambda bound variable
        a <- TH.newName "a" -- local array name
        s <- TH.bindS (TH.varP a) [| useRemoteAsync $(TH.unTypeQ $ liftArraysR (lhsToArraysR lhs)) (fromArr $(TH.varE x)) |]
        go l (TH.bangP (TH.varP x) : xs) ([| ($(TH.unTypeQ $ liftLHS lhs), $(TH.varE a)) |] : as) (return s : stmts)

      go (Abody b) xs as stmts = do
        r <- TH.newName "r" -- result
        s <- TH.newName "s"
        let
            aenv  = foldr (\a gamma -> [| $gamma `push` $a |] ) [| Empty |] as
            body  = embedOpenAcc defaultTarget b
        --
        TH.lamE (reverse xs)
                [| $using (phase "execute" $(k (
                     TH.doE ( reverse stmts ++
                            [ TH.bindS (TH.varP r) [| executeOpenAcc $(TH.unTypeQ body) $aenv |]
                            , TH.bindS (TH.varP s) [| copyToHostLazy $(TH.unTypeQ (liftArraysR (arraysRepr b))) $(TH.varE r) |]
                            , TH.noBindS [| return $ toArr $(TH.varE s) |]
                            ]))))
                 |]
  --
  go afun [] [] []


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
-- registerPinnedAllocator :: IO ()
-- registerPinnedAllocator = registerPinnedAllocatorWith defaultTarget


-- | All future array allocations will use pinned memory associated with the
-- given execution context. These arrays will be directly accessible from the
-- device, enabling high-speed asynchronous DMA.
--
-- Note that since the amount of available pageable memory will be reduced,
-- overall system performance can suffer.
--
registerPinnedAllocatorWith :: PTX -> IO ()
registerPinnedAllocatorWith target =
  registerForeignPtrAllocator $ \bytes ->
    withContext (ptxContext target) (CUDA.mallocHostForeignPtr [] bytes)
    `catch`
    \e -> $internalError "registerPinnedAlocator" (show (e :: CUDAException))


-- Debugging
-- =========

dumpStats :: MonadIO m => a -> m a
dumpStats x = liftIO dumpSimplStats >> return x

phase :: MonadIO m => String -> m a -> m a
phase n go = timed dump_phases (\wall cpu -> printf "phase %s: %s" n (elapsed wall cpu)) go

