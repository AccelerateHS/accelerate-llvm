{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link (

  module Data.Array.Accelerate.LLVM.Link,
  ExecutableR(..), FunctionTable(..), Function,

) where

import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Link
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Native.Compile

import Data.Array.Accelerate.LLVM.Native.Link.Object
#if   defined(darwin_HOST_OS)
import Data.Array.Accelerate.LLVM.Native.Link.MachO
#elif defined(linux_HOST_OS)
import Data.Array.Accelerate.LLVM.Native.Link.ELF
#elif defined(mingw32_HOST_OS)
import Data.Array.Accelerate.LLVM.Native.Link.COFF
#else
#error "Runtime linking not supported on this platform"
#endif

import Control.Monad.State


instance Link Native where
  data ExecutableR Native = NativeR { nativeExecutable :: {-# UNPACK #-} !FunctionTable
                                    , nativeObjectCode :: {-# UNPACK #-} !ObjectCode
                                    }
  linkForTarget = link


-- | Load the generated object file into the target address space
--
link :: ObjectR Native -> LLVM Native (ExecutableR Native)
link (ObjectR obj) = liftIO $ do
  (nm, vm)  <- loadObject obj
  return    $! NativeR nm vm

