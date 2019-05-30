{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile.Cache
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile.Cache (

  module Data.Array.Accelerate.LLVM.Compile.Cache

) where

import Data.Array.Accelerate.LLVM.Compile.Cache
import Data.Array.Accelerate.LLVM.Native.Target

import Data.Version
import System.FilePath
import qualified Data.ByteString.Char8                              as B8
import qualified Data.ByteString.Short.Char8                        as S8

import Paths_accelerate_llvm_native


instance Persistent Native where
  targetCacheTemplate =
    return $ "accelerate-llvm-native-" ++ showVersion version
         </> "llvm-hs-" ++ VERSION_llvm_hs
         </> S8.unpack nativeTargetTriple
         </> B8.unpack nativeCPUName
         </> "meep.o"

