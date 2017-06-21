{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile.Cache
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
import Data.ByteString.Internal                                     ( w2c )
import qualified Data.ByteString.Char8                              as B8
import qualified Data.ByteString.Short                              as BS

import Paths_accelerate_llvm_native


instance Persistent Native where
  targetCacheTemplate =
    return $ "accelerate-llvm-native-" ++ showVersion version
         </> map w2c (BS.unpack nativeTargetTriple)
         </> B8.unpack nativeCPUName
         </> "meep.o"

