{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.Posix
-- Copyright   : [2021] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Utilities for linking object code to shared objects and loading those
-- generated shared objects on Unix-like systems.
--
module Data.Array.Accelerate.LLVM.Native.Link.Posix (
  LibraryHandle,
  sharedObjectExt,
  rawObjectExt,
  linkSharedObject,
  linkKernel,
  embedKernel,
) where

import Data.Array.Accelerate.Lifetime

import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug
import Data.Array.Accelerate.LLVM.Native.Link.Object

import Control.Exception
import Control.Monad                                                ( forM, when )
import Data.ByteString                                              ( ByteString )
import qualified Data.ByteString                                    as B
import Data.ByteString.Short                                        ( ShortByteString )
import qualified Data.ByteString.Short.Char8                        as B8
import Data.Maybe                                                   ( fromMaybe )
import Formatting
import System.FilePath                                              ( takeBaseName )
import System.Environment
import System.Exit                                                  ( ExitCode(..) )
import System.IO                                                    ( hFlush )
import System.IO.Temp
import System.Posix.DynamicLinker
import System.Process


-- | The platform specific shared library handle.
type LibraryHandle = DL

-- | The file extension used for shared libraries.
sharedObjectExt :: String
#ifdef darwin_HOST_OS
sharedObjectExt = "dylib"
#else
sharedObjectExt = "so"
#endif

-- | The file extension used for raw relocatable objects.
rawObjectExt :: String
rawObjectExt = "o"

-- TODO: 'linkSharedObject' and 'linkKernel' sound similar even though they do
--       the exact opposite thing. Any suggestions for better names?

-- | Link a relocatable object to a shared library using the operating system's
-- native linker and save the result to the specified path, returning the
-- linker's exit code.
linkSharedObject :: FilePath -> ByteString -> IO ()
linkSharedObject outputPath obj =
  withSystemTempFile (takeBaseName outputPath <> ".o") $ \objectPath h -> do
    B.hPut h obj
    hFlush h

    -- We'll respect the common @LD@ environment variable, falling back to
    -- searching the search path for @ld@ if that isn't set
    linker <- fromMaybe "ld" <$> lookupEnv "LD"
    process <- spawnProcess linker ["--shared", "-o", outputPath, objectPath]
    exitCode <- waitForProcess process
    when (exitCode /= ExitSuccess) $
      throwIO . userError $ "Linking " <> objectPath <> " failed"

-- | Link to the specified shared object file and create a function table for
-- the entry points specified in @nms@. Will throw an IO exception if the
-- library could not be loaded or any of the symbols could not be resolved. The
-- library will be automatically unloaded once the garbage collector drops the
-- returned lifetime.
linkKernel :: [ShortByteString] -> FilePath -> IO (Lifetime (LibraryHandle, FunctionTable))
linkKernel nms libPath = do
  lib <- dlopen libPath [RTLD_LAZY, RTLD_LOCAL]
  funs <- forM nms $ \nm -> do
    Debug.traceM Debug.dump_ld ("ld: looking up symbol " % shown) nm
    sym <- dlsym lib $ B8.unpack nm
    return (nm, sym)

  libLft <- newLifetime (lib, FunctionTable funs)
  addFinalizer libLft $ do
    Debug.traceM Debug.dump_gc ("gc: unloading shared object: " % string) libPath
    dlclose lib

  return libLft

-- | Build the same representation as the one created in 'linkKernel' for an
-- existing table of function pointers. This is used when statically linking
-- kernels directly into a Haskell binary.
embedKernel :: FunctionTable -> IO (Lifetime (LibraryHandle, FunctionTable))
embedKernel funs = newLifetime (Default, funs)
