{-# LANGUAGE CPP               #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards   #-}
-- {-# LANGUAGE TemplateHaskell   #-}
-- {-# LANGUAGE TupleSections     #-}

-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.Load
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.Load (

  -- nvvmReflect, libdevice,
  libdeviceBitcodePath,

) where

import qualified Text.LLVM                                          as LP

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.PTX.Execute.Event                 ( ) -- GHC#1012
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream                ( ) -- GHC#1012

import Foreign.CUDA.Analysis
import qualified Foreign.CUDA.Driver                                as CUDA

#if MIN_VERSION_nvvm(0,10,0)
import Foreign.NVVM.Path
#else
import Foreign.CUDA.Path
#endif

import Data.List                                                    ( isPrefixOf, sortBy )
import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short.Char8                                  ( ShortByteString )
import qualified Data.ByteString.Short.Char8                        as S8
import qualified Data.Array.Accelerate.TH.Compat                    as TH
import Formatting
import System.Directory
import System.FilePath
import System.IO.Unsafe
import Text.Printf


-- NVVM Reflect
-- ------------

-- class NVVMReflect a where
--   nvvmReflect :: a

-- instance NVVMReflect LP.Module where
--   nvvmReflect = nvvmReflectModule

-- instance NVVMReflect (ShortByteString, ByteString) where
--   nvvmReflect = $$( nvvmReflectBitcode nvvmReflectModule )


-- libdevice
-- ---------

-- Compatible version of libdevice for a given compute capability should be
-- listed here:
--
--   https://github.com/llvm/llvm-project/blob/master/lib/Target/NVPTX/NVPTX.td

-- | Find the libdevice bitcode file for the given compute architecture. The name
-- of the bitcode file follows the format @libdevice.XX.bc@, where XX
-- represents a version(?). We search the libdevice path for all files of the
-- appropriate compute capability and load the "most recent" (by sort order).
libdeviceBitcodePath :: HasCallStack => IO FilePath
libdeviceBitcodePath
  | CUDA.libraryVersion < 9000 =
      -- There is some support code for cuda < 9 in an earlier version of these
      -- files; in particular, look at commit
      --   2b5d69448557e89002c0179ea1aaf59bb757a6e3 (2023-08-22)
      -- for original llvm-hs code.
      internalError "Cuda < 9 is unsupported."
  | otherwise = do
#if MIN_VERSION_nvvm(0,10,0)
      let nvvm    = nvvmDeviceLibraryPath
#else
      let nvvm    = cudaInstallPath </> "nvvm" </> "libdevice"
#endif

      files <- getDirectoryContents nvvm

      let matches f = "libdevice" `isPrefixOf` f && takeExtension f == ".bc"
      return $ case sortBy (flip compare) (filter matches files) of
                 name : _ -> nvvm </> name
                 [] -> internalError "not found: libdevice.XX.bc"
