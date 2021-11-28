{-# LANGUAGE CPP #-}

-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.Posix
-- Copyright   : [2021] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Re-exports the correct linking utilities for the current platform.
--
module Data.Array.Accelerate.LLVM.Native.Link.Util (
  LibraryHandle,
  sharedObjectExt,
  rawObjectExt,
  linkSharedObject,
  linkKernel,
  embedKernel,
) where

#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
import Data.Array.Accelerate.LLVM.Native.Link.Posix
#else
-- TODO: Implementing this same functionality on Windows should be fairly
--       simple, I just don't have a way to test it. The difficult part is
--       probably going to be linking the object code to a .dll file, although
--       GHC on Windows should come with a mingw-w64 toolchain. Once that is
--       done, the uses of @dlopen()@ and @dlsym()@ need to be replaced with
--       @LoadLibrary()@ and @GetProcAddress()@. In theory everything should
--       then work.
#error "Runtime linking not supported on this platform"
#endif
