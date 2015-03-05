{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile.Function
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile.Function (

  Function,
  startFunction,
  executeFunction, executeNamedFunction,

) where

-- accelerate
import Data.Array.Accelerate.Error
import qualified Data.Array.Accelerate.LLVM.Native.Debug        as Debug

-- standard library
import Control.Concurrent
import Data.List
import Foreign.Ptr


-- | The 'Req' type encapsulates a request to execute the JIT compiled function
--
data Req
  -- | Run the given action
  = ReqDo (IO ())

  -- | Tell the function that it is time to exit, and clean up all resources
  -- related to JIT compiling the function(s).
  | ReqShutdown


-- | A 'Function' is a worker that executes captured JIT compiled functions.
--
-- Note: [Executing JIT-compiled functions]
--
-- We have the problem that all the llvm-general functions dealing with the FFI
-- are expressed as bracketed 'with*' operations. This is a good design, because
-- it ensures functions clean up their resources when they exit. However, it
-- makes it impossible to return the compiled code from inside the bracketed
-- function, because it will no longer be valid once we try to execute it. For
-- good performance, we really do not want to have to lower all the way from the
-- pure AST into machine code every time we want to execute a function.
--
-- This function wrapper provides a way around it. The initial lowering step
-- that compiles into machine code is spawned into a separate thread. This
-- lowering includes all the bracketed functions as discussed above. However
-- instead of returning straight away, the thread then loops waiting for
-- requests to execute the compiled functions. Only once we signal to the thread
-- that it can exit, does the thread exit the bracketed functions, allowing
-- resources to be cleaned up.
--
data Function = Function {

    -- | The encapsulated JIT functions that can be executed
    functionTable       :: [(String, FunPtr ())]

    -- | The worker listens against this MVar for an action to execute
  , functionReq         :: {-# UNPACK #-} !(MVar Req)

    -- | Indicates that the requested action has completed
  , functionResult      :: {-# UNPACK #-} !(MVar ())
  }

instance Show Function where
  showsPrec _ f
    = showString "<<"
    . showString (intercalate "," [ n | (n,_) <- functionTable f ])
    . showString ">>"


-- | Execute the main function (the first function defined in a module).
--
executeFunction :: Function -> (FunPtr () -> IO ()) -> IO ()
executeFunction Function{..} run =
  case functionTable of
    []      -> $internalError "executeFunction" "no functions defined"
    (_,f):_ -> do
      putMVar  functionReq (ReqDo $ run f)
      takeMVar functionResult

-- | Execute a named function that was defined in the module. If the function
-- does not exist: error.
--
executeNamedFunction :: Function -> String -> (FunPtr () -> IO ()) -> IO ()
executeNamedFunction Function{..} name run =
  case lookup name functionTable of
    Nothing -> $internalError "executeNamedFunction" "function not found"
    Just f  -> do
      putMVar  functionReq (ReqDo $ run f)
      takeMVar functionResult


-- | Start a worker thread that compiles the given module into executable code,
-- then waits for requests to execute the compiled functions.
--
startFunction
    :: (([(String, FunPtr ())] -> IO ()) -> IO ())
    -> IO Function
startFunction withFunctions = do

  varFun  <- newEmptyMVar
  varReq  <- newEmptyMVar
  varDone <- newEmptyMVar

  -- The finaliser requires access to both varReq and varDone, but which is GC'd
  -- first is very racey. If we don't put the finaliser on both, and other MVar
  -- is GC'd first, the function will never get the shutdown request.
  _       <- mkWeakMVar varReq  (finaliseFunction varReq varDone)
  _       <- mkWeakMVar varDone (finaliseFunction varReq varDone)

  _       <- forkIO $ withFunctions $ \f -> do
    putMVar varFun f
    functionLoop varReq varDone

  -- The below use of 'unsafePerformIO' might be possible to allow the main
  -- thread to continue while the lowering to LLVM happens in the background.
  -- However I had some problems interacting with the multi device backend.
  -- let fun    = unsafePerformIO $ takeMVar varFun

  fun <- takeMVar varFun
  let worker = Function fun varReq varDone

  message $ "created worker function: " ++ show worker
  return worker


-- | The worker thread blocks on the MVar waiting for a work request. On
-- completion it writes into the result MVar.
--
functionLoop :: MVar Req -> MVar () -> IO ()
functionLoop varReq varDone
  = do
        -- Wait for the request
        req   <- takeMVar varReq

        case req of
          ReqDo action
            -> do action
                  putMVar varDone ()
                  functionLoop varReq varDone

          ReqShutdown
            -> putMVar varDone ()


-- | The finaliser tells the worker to shutdown once its MVar becomes
-- unreachable.
--
-- Without this the program can complain about "Blocked indefinitely on an MVar"
-- because the workers are still blocked on the requests when the program ends.
-- Whether the finaliser is called or not is very racey.
--
finaliseFunction :: MVar Req -> MVar () -> IO ()
finaliseFunction varReq varDone
  = do
        message "worker function shutting down"
        putMVar varReq ReqShutdown
        takeMVar varDone


-- Debug
-- -----

{-# INLINE message #-}
message :: String -> IO ()
message str = Debug.traceIO Debug.dump_exec ("exec: " ++ str)

