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

import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Link
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Native.Compile

import Data.Array.Accelerate.LLVM.Native.Link.Cache
import Data.Array.Accelerate.LLVM.Native.Link.Object
import Data.Array.Accelerate.LLVM.Native.Link.Runtime

import Control.Monad.State
import Prelude                                                      hiding ( lookup )


instance Link Native where
  data ExecutableR Native = NativeR { nativeExecutable :: {-# UNPACK #-} !(Lifetime FunctionTable)
                                    }
  linkForTarget = link


-- | Link to the generated shared object file, creating function pointers for
-- every kernel's entry point.
--
link :: ObjectR Native -> LLVM Native (ExecutableR Native)
link (ObjectR uid nms _ so) = do
  cache <- gets linkCache
  funs  <- liftIO $ dlsym uid cache (loadSharedObject nms so)
  return $! NativeR funs


-- | Execute some operation with the supplied executable functions
--
withExecutable :: MonadIO m => ExecutableR Native -> (FunctionTable -> m b) -> m b
withExecutable NativeR{..} f = do
  r <- f (unsafeGetValue nativeExecutable)
  liftIO $ touchLifetime nativeExecutable
  return r

