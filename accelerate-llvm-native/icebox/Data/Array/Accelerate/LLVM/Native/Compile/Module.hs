{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile.Module
-- Copyright   : [2014..2017] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile.Module (

  Module,
  compileModule,
  execute, executeMain,
  nm,

) where

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

-- library
import Control.Exception
import Control.Concurrent
import Data.List
import Foreign.LibFFI
import Foreign.Ptr
import Text.Printf
import Data.ByteString.Short                                        ( ShortByteString )


-- | An encapsulation of the callable functions resulting from compiling
-- a module.
--
data Module         = Module {-# UNPACK #-} !(Lifetime FunctionTable)

data FunctionTable  = FunctionTable { functionTable :: [Function] }
type Function       = (ShortByteString, FunPtr ())

instance Show Module where
  showsPrec p (Module m)
    = showsPrec p (unsafeGetValue m)

instance Show FunctionTable where
  showsPrec _ f
    = showString "<<"
    . showString (intercalate "," [ show n | (n,_) <- functionTable f ])
    . showString ">>"


-- | Execute a named function that was defined in the module. An error is thrown
-- if the requested function is not define in the module.
--
-- The final argument is a continuation to which we pass a function you can call
-- to actually execute the foreign function.
--
{-# INLINEABLE execute #-}
execute
    :: Module
    -> ShortByteString
    -> ((ShortByteString, [Arg] -> IO ()) -> IO a)
    -> IO a
execute mdl@(Module ft) name k =
  withLifetime ft $ \FunctionTable{..} ->
    case lookup name functionTable of
      Just f  -> k (name, \argv -> callFFI f retVoid argv)
      Nothing -> $internalError "execute" (printf "function '%s' not found in module: %s\n" (show name) (show mdl))


-- | Execute the 'main' function of a module, which is just the first function
-- defined in the module.
--
{-# INLINEABLE executeMain #-}
executeMain
    :: Module
    -> ((ShortByteString, [Arg] -> IO ()) -> IO a)
    -> IO a
executeMain (Module ft) k =
  withLifetime ft $ \FunctionTable{..} ->
    case functionTable of
      []         -> $internalError "executeMain" "no functions defined in module"
      (name,f):_ -> k (name, \argv -> callFFI f retVoid argv)


-- | Display the global (external) symbol table for this module.
--
nm :: Module -> IO [ShortByteString]
nm (Module ft) =
  withLifetime ft $ \FunctionTable{..} ->
    return $ map fst functionTable


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
compileModule compile = mask $ \restore -> do
  main  <- myThreadId
  mfuns <- newEmptyMVar
  mdone <- newEmptyMVar
  _     <- forkIO . reflectExceptionsTo main . restore . compile $ \funs -> do
    putMVar mfuns funs
    takeMVar mdone                              -- thread blocks, keeping 'funs' alive
    message "worker thread shutting down"       -- we better have a matching message from 'finalise'
  --
  funs  <- takeMVar mfuns
  ftab  <- newLifetime (FunctionTable funs)
  addFinalizer ftab (finalise mdone)
  return (Module ftab)

reflectExceptionsTo :: ThreadId -> IO () -> IO ()
reflectExceptionsTo tid action =
  catchNonThreadKilled action (throwTo tid)

catchNonThreadKilled :: IO a -> (SomeException -> IO a) -> IO a
catchNonThreadKilled action handler =
  action `catch` \e ->
    case fromException e of
      Just ThreadKilled -> throwIO e
      _                 -> handler e

finalise :: MVar () -> IO ()
finalise done = do
  message "finalising function table"
  putMVar done ()


-- Debug
-- -----

{-# INLINE message #-}
message :: String -> IO ()
message msg = Debug.traceIO Debug.dump_exec ("exec: " ++ msg)

