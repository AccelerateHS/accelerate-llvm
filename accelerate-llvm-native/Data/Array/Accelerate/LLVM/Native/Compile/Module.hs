{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile.Module
-- Copyright   : [2014..2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile.Module (

  Module,
  compileModule,
  execute, executeMain,

) where

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import qualified Data.Array.Accelerate.LLVM.Native.Debug        as Debug

-- library
import Control.Concurrent
import Data.List
import Foreign.LibFFI
import Foreign.Ptr
import Text.Printf


-- | An encapsulation of the callable functions resulting from compiling
-- a module.
--
data Module         = Module {-# UNPACK #-} !(Lifetime FunctionTable)

data FunctionTable  = FunctionTable { functionTable :: [Function] }
type Function       = (String, FunPtr ())

instance Show Module where
  showsPrec p (Module m)
    = showsPrec p (unsafeGetValue m)

instance Show FunctionTable where
  showsPrec _ f
    = showString "<<"
    . showString (intercalate "," [ n | (n,_) <- functionTable f ])
    . showString ">>"


-- | Execute a named function that was defined in the module. An error is thrown
-- if the requested function is not define in the module.
--
-- Note: [Execute and Lifetime]
--
-- We should really use 'withLifetime' here, but we want to be able to return
-- closure of type ([Arg] -> IO ()) that we can call many times skipping the
-- function table lookup overhead. So, if using this technique, it should be
-- enough to ensure that defining 'Module' stays alive for as long as the
-- closure is used.
--
execute :: Module -> String -> [Arg] -> IO ()
execute mdl@(Module ft) name =
  let FunctionTable{..} = unsafeGetValue ft in
  case lookup name functionTable of
    Just f  -> \argv -> callFFI f retVoid argv
    Nothing -> $internalError "execute" (printf "function '%s' not found in module: %s\n" name (show mdl))


-- | Execute the 'main' function of a module, which is just the first function
-- defined in the module.
--
-- See: [Execute and Lifetime]
--
executeMain :: Module -> [Arg] -> IO ()
executeMain (Module ft) =
  let FunctionTable{..} = unsafeGetValue ft in
  case functionTable of
      []      -> $internalError "executeMain" "no functions defined in module"
      (_,f):_ -> \argv -> callFFI f retVoid argv


-- Compile a given module into executable code.
--
-- Note: [Executing JIT-compiled functions]
--
-- We have the problem that the llvm-general functions dealing with the FFI are
-- exposed as bracketed 'with*' operations, rather than as separate
-- 'create*'/'destroy*' pairs. This is a good design that guarantees that
-- functions clean up their resources on exit, but also means that we can't
-- return a function pointer to the compiled code from within the bracketed
-- expression, because it will no longer be valid once we get around to
-- executing it, as it has already been deallocated!
--
-- This function provides a wrapper that does the compilation step (first
-- argument) in a separate thread, returns the compiled functions, then waits
-- until they are no longer needed before allowing the finalisation routines to
-- proceed.
--
compileModule :: (([Function] -> IO ()) -> IO ()) -> IO Module
compileModule compile = do
  mfuns <- newEmptyMVar
  mdone <- newEmptyMVar
  _     <- forkIO . compile $ \funs -> do
    putMVar mfuns funs
    takeMVar mdone                              -- thread blocks, keeping 'funs' alive
    message "worker thread shutting down"       -- we better have a matching message from 'finalise'
  --
  funs  <- takeMVar mfuns
  ftab  <- newLifetime (FunctionTable funs)
  addFinalizer ftab (finalise mdone)
  return (Module ftab)


finalise :: MVar () -> IO ()
finalise done = do
  message "finalising function table"
  putMVar done ()


-- Debug
-- -----

{-# INLINE message #-}
message :: String -> IO ()
message msg = Debug.traceIO Debug.dump_exec ("exec: " ++ msg)

