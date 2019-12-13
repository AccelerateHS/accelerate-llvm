{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link (

  module Data.Array.Accelerate.LLVM.Link,
  module Data.Array.Accelerate.LLVM.Native.Link,
  ExecutableR(..), FunctionTable(..), Function, ObjectCode,

) where

import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Link
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Native.Compile

import Data.Array.Accelerate.LLVM.Native.Link.Object
import Data.Array.Accelerate.LLVM.Native.Link.Cache
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
import Prelude                                                      hiding ( lookup )


instance Link Native where
  data ExecutableR Native = NativeR { nativeExecutable :: {-# UNPACK #-} !(Lifetime FunctionTable)
                                    }
  linkForTarget = link


-- | Load the generated object file into the target address space
--
link :: ObjectR Native -> LLVM Native (ExecutableR Native)
link (ObjectR uid _ obj) = do
  cache  <- gets linkCache
  funs   <- liftIO $ dlsym uid cache (loadObject obj)
  return $! NativeR funs


-- | Execute some operation with the supplied executable functions
--
withExecutable :: MonadIO m => ExecutableR Native -> (FunctionTable -> m b) -> m b
withExecutable NativeR{..} f = do
  r <- f (unsafeGetValue nativeExecutable)
  liftIO $ touchLifetime nativeExecutable
  return r

