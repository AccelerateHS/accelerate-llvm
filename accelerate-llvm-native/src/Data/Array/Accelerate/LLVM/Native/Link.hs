{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link
-- Copyright   : [2017..2020] The Accelerate Team
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

import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Link
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.Native.Compile
import Data.Array.Accelerate.LLVM.Native.Target

import Data.Array.Accelerate.LLVM.Native.Link.Object
import Data.Array.Accelerate.LLVM.Native.Link.Util

import Control.Monad.State
import Formatting
import Prelude hiding (lookup)

instance Link Native where
  data ExecutableR Native = NativeR
    { -- | The platform-specific library handle along with mappings between
      -- Accelerate kernel names and function pointers for those kernels.
      nativeExecutable :: {-# UNPACK #-} !(Lifetime (LibraryHandle, FunctionTable))
    }
  linkForTarget = link


-- | Link to the generated shared object file, creating function pointers for
-- every kernel's entry point.
--
link :: ObjectR Native -> LLVM Native (ExecutableR Native)
link (ObjectR nms libM _) = do
  libPath <- libM

  liftIO $! Debug.traceM Debug.dump_ld ("ld: loading shared object " % string) libPath
  libLft <- liftIO $! linkKernel nms libPath
  liftIO $! Debug.traceM Debug.dump_ld ("ld: finished loading shared object " % string) libPath

  return $! NativeR libLft

-- | Execute some operation with the supplied executable functions
--
withExecutable :: MonadIO m => ExecutableR Native -> (FunctionTable -> m b) -> m b
withExecutable NativeR{..} f = do
  r <- f (snd $ unsafeGetValue nativeExecutable)
  liftIO $ touchLifetime nativeExecutable
  return r
