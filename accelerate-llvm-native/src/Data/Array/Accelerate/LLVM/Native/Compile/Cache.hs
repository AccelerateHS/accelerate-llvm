{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile.Cache
-- Copyright   : [2017..2020] The Accelerate Team
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
import Data.Array.Accelerate.LLVM.Target.ClangInfo                  ( hostLLVMVersion )

import Data.Foldable                                                ( toList )
import Data.List                                                    ( intercalate )
import Data.Version
import System.FilePath
import qualified Data.ByteString.Char8                              as B8
import qualified Data.ByteString.Short.Char8                        as S8

import Paths_accelerate_llvm_native


instance Persistent Native where
  targetCacheTemplate =
    -- The "llvmpr" is for "llvm-pretty". This is to ensure we still have a
    -- sensible cache path to switch to should we ever move away from
    -- llvm-pretty again.
    return $ "accelerate-llvm-native-" ++ showVersion version
         </> "llvmpr-" ++ intercalate "." (map show (toList hostLLVMVersion))
         </> S8.unpack nativeTargetTriple
         </> B8.unpack nativeCPUName
         </> "meep"

