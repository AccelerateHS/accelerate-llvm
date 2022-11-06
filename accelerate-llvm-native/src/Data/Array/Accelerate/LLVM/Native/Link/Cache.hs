{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.Cache
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link.Cache (

  LinkCache,
  new, LC.dlsym,

) where

import Data.Array.Accelerate.Debug.Internal                         ( debuggingIsEnabled )

import Data.Array.Accelerate.LLVM.Native.Link.Object
import qualified Data.Array.Accelerate.LLVM.Link.Cache              as LC

import Control.Monad

#if defined(mingw32_HOST_OS)
import System.Win32.DLL
#else
import System.Posix.DynamicLinker
#endif

type LinkCache = LC.LinkCache FunctionTable ObjectCode

new :: IO LinkCache
new = do
  -- For whatever reason ghci isn't adding library dependencies to the
  -- dynamic link state, which means that dynamic linking will fail in
  -- debugging mode because we depend on tracy symbols exported by the
  -- accelerate library. This brings those symbols into scope so that they
  -- can be found by later calls to dlsym().
  --
  -- Additionally, the Accelerate library has been compiled with -rdynamic
  -- in order to bring all exported symbols into the global symbol table.
  -- This seems to be required so that dlsym() can find symbols from the
  -- GHC RTS when we are in compiled (not interpreted) mode. In non-ghci
  -- mode, loading the RTS dynamic library explicitly (as we do with the
  -- Accelerate library) causes segfaults; possibly because the RTS was
  -- otherwise linked statically into the executable.
  --
  when debuggingIsEnabled $ void $
#if defined(mingw32_HOST_OS)
    loadLibrary ACCELERATE_DYLD_LIBRARY_PATH
#else
    dlopen ACCELERATE_DYLD_LIBRARY_PATH [RTLD_LAZY, RTLD_GLOBAL]
#endif
  LC.new
