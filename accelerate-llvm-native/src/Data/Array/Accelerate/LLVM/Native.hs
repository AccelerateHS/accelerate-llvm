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
-- Module      : Data.Array.Accelerate.LLVM.Native
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements a backend for the /Accelerate/ language targeting
-- multicore CPUs. Expressions are on-line translated into LLVM code, which is
-- just-in-time executed in parallel over the available CPUs. Functions are
-- automatically parallelised over all available cores, unless you set the
-- environment variable 'ACCELERATE_LLVM_NATIVE_THREADS=N', in which case 'N'
-- threads will be used.
--
-- Programs must be compiled with '-threaded', otherwise you will get a "Blocked
-- indefinitely on MVar" error.
--

module Data.Array.Accelerate.LLVM.Native (

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
  Native,
  createTarget,

) where

-- accelerate
import Data.Array.Accelerate.AST                                    ( PreOpenAfun(..), arraysRepr, liftLHS, liftArraysR, lhsToArraysR )
import Data.Array.Accelerate.Array.Sugar                            ( Arrays, toArr, fromArr, arrays, ArrRepr )
import Data.Array.Accelerate.Async                                  ( Async, async, wait, poll, cancel )
import Data.Array.Accelerate.Smart                                  ( Acc )
import Data.Array.Accelerate.Trafo

import Data.Array.Accelerate.LLVM.Embed                             ( HasTypeable(..), lhsTypeable )
import Data.Array.Accelerate.LLVM.Native.Array.Data                 ( useRemoteAsync )
import Data.Array.Accelerate.LLVM.Native.Compile                    ( CompiledOpenAfun, compileAcc, compileAfun )
import Data.Array.Accelerate.LLVM.Native.Embed                      ( embedOpenAcc )
import Data.Array.Accelerate.LLVM.Native.Execute                    ( executeAcc, executeOpenAcc )
import Data.Array.Accelerate.LLVM.Native.Execute.Async              ( Par, evalPar, getArrays )
import Data.Array.Accelerate.LLVM.Native.Execute.Environment        ( Val, ValR(..), push )
import Data.Array.Accelerate.LLVM.Native.Link                       ( ExecOpenAfun, linkAcc, linkAfun )
import Data.Array.Accelerate.LLVM.Native.State
import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Native.Debug                      as Debug

-- standard library
import Control.Monad.Trans
import Data.Typeable
import System.IO.Unsafe
import Text.Printf
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH


-- Accelerate: LLVM backend for multicore CPUs
-- -------------------------------------------

-- | Compile and run a complete embedded array program.
--
-- /NOTE:/ it is recommended to use 'runN' or 'runQ' whenever possible.
--
run :: Arrays a => Acc a -> a
run = runWith defaultTarget

-- | As 'run', but execute using the specified target (thread gang).
--
runWith :: Arrays a => Native -> Acc a -> a
runWith target a = unsafePerformIO (runWithIO target a)

-- | As 'run', but allow the computation to run asynchronously and return
-- immediately without waiting for the result. The status of the computation can
-- be queried using 'wait', 'poll', and 'cancel'.
--
runAsync :: Arrays a => Acc a -> IO (Async a)
runAsync = runAsyncWith defaultTarget

-- | As 'runAsync', but execute using the specified target (thread gang).
--
runAsyncWith :: Arrays a => Native -> Acc a -> IO (Async a)
runAsyncWith target a = async (runWithIO target a)

runWithIO :: Arrays a => Native -> Acc a -> IO a
runWithIO target a = execute
  where
    !acc    = convertAcc a
    execute = do
      dumpGraph acc
      evalNative target $ do
        build <- phase "compile" elapsedS (compileAcc acc) >>= dumpStats
        exec  <- phase "link"    elapsedS (linkAcc build)
        res   <- phase "execute" elapsedP (evalPar (executeAcc exec >>= getArrays (arraysRepr exec)))
        return $ toArr res


-- | This is 'runN', specialised to an array program of one argument.
--
run1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b
run1 = run1With defaultTarget

-- | As 'run1', but execute using the specified target (thread gang).
--
run1With :: (Arrays a, Arrays b) => Native -> (Acc a -> Acc b) -> a -> b
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
-- See the programs in the 'accelerate-examples' package for examples.
--
-- See also 'runQ', which compiles the Accelerate program at _Haskell_ compile
-- time, thus eliminating the runtime overhead altogether.
--
runN :: Afunction f => f -> AfunctionR f
runN = runNWith defaultTarget

-- | As 'runN', but execute using the specified target (thread gang).
--
runNWith :: forall f. Afunction f => Native -> f -> AfunctionR f
runNWith target f = go (afunctionRepr @f) afun (return Empty)
  where
    !acc  = convertAfun f
    !afun = unsafePerformIO $ do
              dumpGraph acc
              evalNative target $ do
                build <- phase "compile" elapsedS (compileAfun acc) >>= dumpStats
                link  <- phase "link"    elapsedS (linkAfun build)
                return link

    go :: AfunctionRepr t (AfunctionR t) (AreprFunctionR t)
       -> ExecOpenAfun Native aenv (AreprFunctionR t)
       -> Par Native (Val aenv)
       -> AfunctionR t
    go (AfunctionReprLam repr) (Alam lhs l) k = \(arrs :: a) ->
      let k' = do aenv  <- k
                  a     <- useRemoteAsync (arrays @a) $ fromArr arrs
                  return (aenv `push` (lhs, a))
      in go repr l k'
    go AfunctionReprBody (Abody b) k = unsafePerformIO . phase "execute" elapsedP . evalNative target . evalPar $ do
      aenv <- k
      res  <- executeOpenAcc b aenv
      arrs <- getArrays (arraysRepr b) res
      return $ toArr arrs
    go _ _ _ = error "The moon is hanging upside down"


-- | As 'run1', but execute asynchronously.
--
run1Async :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> IO (Async b)
run1Async = run1AsyncWith defaultTarget

-- | As 'run1Async', but execute using the specified target (thread gang).
--
run1AsyncWith :: (Arrays a, Arrays b) => Native -> (Acc a -> Acc b) -> a -> IO (Async b)
run1AsyncWith = runNAsyncWith


-- | As 'runN', but execute asynchronously.
--
runNAsync :: (Afunction f, RunAsync r, AreprFunctionR f ~ RunAsyncR r) => f -> r
runNAsync = runNAsyncWith defaultTarget

-- | As 'runNWith', but execute asynchronously.
--
runNAsyncWith :: (Afunction f, RunAsync r, AreprFunctionR f ~ RunAsyncR r) => Native -> f -> r
runNAsyncWith target f = exec
  where
    !acc  = convertAfun f
    !afun = unsafePerformIO $ do
              dumpGraph acc
              evalNative target $ do
                build <- phase "compile" elapsedS (compileAfun acc) >>= dumpStats
                link  <- phase "link"    elapsedS (linkAfun build)
                return link
    !exec = runAsync' target afun (return Empty)

class RunAsync f where
  type RunAsyncR f
  runAsync' :: Native -> ExecOpenAfun Native aenv (RunAsyncR f) -> Par Native (Val aenv) -> f

instance (Arrays a, RunAsync b) => RunAsync (a -> b) where
  type RunAsyncR (a -> b) = ArrRepr a -> RunAsyncR b
  runAsync' _      Abody{}  _ _    = error "runAsync: function oversaturated"
  runAsync' target (Alam lhs l) k arrs =
    let k' = do aenv  <- k
                a     <- useRemoteAsync (arrays @a) $ fromArr arrs
                return (aenv `push` (lhs, a))
    in runAsync' target l k'

instance Arrays b => RunAsync (IO (Async b)) where
  type RunAsyncR  (IO (Async b)) = ArrRepr b
  runAsync' _      Alam{}    _ = error "runAsync: function not fully applied"
  runAsync' target (Abody b) k = async . phase "execute" elapsedP . evalNative target . evalPar $ do
    aenv  <- k
    ans   <- executeOpenAcc b aenv
    arrs  <- getArrays (arraysRepr b) ans
    return $ toArr arrs


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


-- | Ahead-of-time compilation for an embedded array program.
--
-- This function will generate, compile, and link into the final executable,
-- code to execute the given Accelerate computation /at Haskell compile time/.
-- This eliminates any runtime overhead associated with the other @run*@
-- operations. The generated code will be optimised for the compiling
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
-- It is recommended to use GHC-8.6 or later. Earlier GHC versions can
-- successfully build executables utilising 'runQ', but fail to correctly link
-- libraries containing this function.
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
-- The correct type of this function is:
--
-- > runQWith :: Afunction f => f -> Q (TExp (Native -> AfunctionR f))
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

-- | Ahead-of-time analogue of 'runNAsyncWith'. See 'runQ' for more information.
--
-- The correct type of this function is:
--
-- > runQAsyncWith :: (Afunction f, RunAsync r, AfunctionR f ~ RunAsyncR r) => f -> Q (TExp (Native -> r))
--
-- @since 1.1.0.0
--
runQAsyncWith :: Afunction f => f -> TH.ExpQ
runQAsyncWith f = do
  target <- TH.newName "target"
  TH.lamE [TH.varP target] (runQ' [| async |] (TH.varE target) f)


runQ' :: forall f. Afunction f => TH.ExpQ -> TH.ExpQ -> f -> TH.ExpQ
runQ' using target f = do
#if MIN_VERSION_template_haskell(2,13,0)
  -- The plugin ensures that objects are loaded correctly into GHCi
  TH.addCorePlugin "Data.Array.Accelerate.LLVM.Native.Plugin"
#endif

  afun  <- let acc = convertAfun f
            in TH.runIO $ do
                 dumpGraph acc
                 evalNative defaultTarget $
                  phase "compile" elapsedS (compileAfun acc) >>= dumpStats

  -- generate a lambda function with the correct number of arguments and
  -- apply directly to the body expression.
  --
  -- XXX: remove use of 'getArrays', 'toArr', and 'fromArr' in the embedded
  -- code; we should be able to generate all bindings directly and assemble
  -- the pieces directly.
  --
  let
      go :: Typeable aenv => CompiledOpenAfun Native aenv t -> [TH.PatQ] -> [TH.ExpQ] -> [TH.StmtQ] -> TH.ExpQ
      go (Alam lhs l) xs as stmts | HasTypeable <- lhsTypeable lhs = do
        x <- TH.newName "x" -- lambda bound variable
        a <- TH.newName "a" -- local array name
        s <- TH.bindS (TH.varP a) [| useRemoteAsync $(TH.unTypeQ $ liftArraysR (lhsToArraysR lhs)) (fromArr $(TH.varE x)) |]
        go l (TH.varP x : xs) ([| ($(TH.unTypeQ $ liftLHS lhs), $(TH.varE a)) |] : as) (return s : stmts)

      go (Abody b) xs as stmts = do
        r <- TH.newName "r" -- result
        s <- TH.newName "s"
        let
            aenv  = foldr (\a gamma -> [| $gamma `push` $a |]) [| Empty |] as
            body  = embedOpenAcc defaultTarget b
        --
        TH.lamE (reverse xs)
                [| $using . phase "execute" elapsedP . evalNative $target . evalPar $
                      $(TH.doE ( reverse stmts ++
                               [ TH.bindS (TH.varP r) [| executeOpenAcc $(TH.unTypeQ body) $aenv |]
                               , TH.bindS (TH.varP s) [| getArrays $(TH.unTypeQ (liftArraysR (arraysRepr b))) $(TH.varE r) |]
                               , TH.noBindS [| return $ toArr $(TH.varE s) |]
                               ]))
                 |]
  --
  go afun [] [] []


-- Debugging
-- =========

dumpStats :: MonadIO m => a -> m a
dumpStats x = liftIO dumpSimplStats >> return x

phase :: MonadIO m => String -> (Double -> Double -> String) -> m a -> m a
phase n fmt go = timed dump_phases (\wall cpu -> printf "phase %s: %s" n (fmt wall cpu)) go

