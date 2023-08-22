{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.Runtime
-- Copyright   : [2022] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Utilities for linking object code to shared objects and loading those
-- generated shared objects on Unix-like systems.
--

module Data.Array.Accelerate.LLVM.Native.Link.Runtime (

  loadSharedObject

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.Native.Link.Object
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

import Control.Monad
import Data.ByteString.Short.Char8                                  ( ShortByteString )
import Formatting
import qualified Data.ByteString.Short.Char8                        as B8

#if defined(mingw32_HOST_OS)
import System.Win32.DLL
#else
import System.Posix.DynamicLinker
#endif


-- Dynamic object loading
-- ----------------------

-- Load the shared object file and return pointers to the executable
-- functions defined within
--
loadSharedObject :: HasCallStack => [ShortByteString] -> FilePath -> IO (FunctionTable, ObjectCode)
loadSharedObject nms path = do
#if defined(mingw32_HOST_OS)
  -- shims for win32 api compatibility
  let dlopen path _ = loadLibrary path
      dlsym dll sym = castPtrToFunPtr <$> getProcAddress dll sym
      dlclose dll   = freeLibrary dll
#endif
  --
  so      <- dlopen path [RTLD_LAZY, RTLD_LOCAL]
  fun_tab <- fmap FunctionTable $ forM nms $ \nm -> do
    let s = B8.unpack nm
    Debug.traceM Debug.dump_ld ("ld: looking up symbol " % string) s
    sym <- dlsym so s
    return (nm, sym)

  object_code <- newLifetime so
  addFinalizer object_code $ do
    -- XXX: Should we disable unloading objects in debug mode? Tracy might
    -- still need access to e.g. embedded string data
    Debug.traceM Debug.dump_gc ("gc: unload module: " % formatFunctionTable) fun_tab
    dlclose so

  return (fun_tab, object_code)

