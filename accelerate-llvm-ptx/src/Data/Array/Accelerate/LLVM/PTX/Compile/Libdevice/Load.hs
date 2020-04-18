{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.Load
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.Load (

  nvvmReflect, libdevice,

) where

-- llvm-hs
import LLVM.Context
import LLVM.Module                                                  as LLVM
import LLVM.AST                                                     as AST ( Module(..) )

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.TH
import Data.Array.Accelerate.LLVM.PTX.Execute.Event                 ( ) -- GHC#1012
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream                ( ) -- GHC#1012

-- cuda
import Foreign.CUDA.Analysis
import qualified Foreign.CUDA.Driver                                as CUDA

-- standard library
import System.IO.Unsafe
import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short.Char8                                  ( ShortByteString )
import qualified Data.ByteString.Short.Char8                        as S8
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH


-- NVVM Reflect
-- ------------

class NVVMReflect a where
  nvvmReflect :: a

instance NVVMReflect AST.Module where
  nvvmReflect = nvvmReflectModule

instance NVVMReflect (ShortByteString, ByteString) where
  nvvmReflect = $$( nvvmReflectBitcode nvvmReflectModule )


-- libdevice
-- ---------

-- Compatible version of libdevice for a given compute capability should be
-- listed here:
--
--   https://github.com/llvm-mirror/llvm/blob/master/lib/Target/NVPTX/NVPTX.td#L72
--
class Libdevice a where
  libdevice :: Compute -> a

-- Load the libdevice bitcode files as an LLVM AST module. The top-level
-- unsafePerformIO ensures that the data is only read from disk once per
-- program execution.
--
-- As of CUDA-9.0, libdevice is no longer split into multiple files
-- depending on the target compute architecture.
--
$( let
      libdeviceModule :: TH.ExpQ
      libdeviceModule = [| \(name, bc) ->
        unsafePerformIO $
          withContext $ \ctx ->
            withModuleFromBitcode ctx (S8.unpack name, bc) moduleAST |]
   in
   if CUDA.libraryVersion < 9000
      then
        [d| {-# NOINLINE libdevice_20_mdl #-}
            {-# NOINLINE libdevice_30_mdl #-}
            {-# NOINLINE libdevice_35_mdl #-}
            {-# NOINLINE libdevice_50_mdl #-}
            libdevice_20_mdl, libdevice_30_mdl, libdevice_35_mdl, libdevice_50_mdl :: AST.Module
            libdevice_20_mdl = $libdeviceModule libdevice_20_bc
            libdevice_30_mdl = $libdeviceModule libdevice_30_bc
            libdevice_35_mdl = $libdeviceModule libdevice_35_bc
            libdevice_50_mdl = $libdeviceModule libdevice_50_bc

            libdevice_20_bc, libdevice_30_bc, libdevice_35_bc, libdevice_50_bc :: (ShortByteString,ByteString)
            libdevice_20_bc = $( TH.unTypeQ $ libdeviceBitcode (Compute 2 0) )
            libdevice_30_bc = $( TH.unTypeQ $ libdeviceBitcode (Compute 3 0) )
            libdevice_35_bc = $( TH.unTypeQ $ libdeviceBitcode (Compute 3 5) )
            libdevice_50_bc = $( TH.unTypeQ $ libdeviceBitcode (Compute 5 0) )

            instance Libdevice AST.Module where
              libdevice compute =
                case compute of
                  Compute 2 _   -> libdevice_20_mdl   -- 2.0, 2.1
                  Compute 3 x
                    | x < 5     -> libdevice_30_mdl   -- 3.0, 3.2
                    | otherwise -> libdevice_35_mdl   -- 3.5, 3.7
                  Compute 5 _   -> libdevice_50_mdl   -- 5.x
                  _             -> $internalError "libdevice"
                                       (unlines [ "This device (compute capability " ++ show compute ++ ") is not supported by this version of the CUDA toolkit (" ++ show CUDA.libraryVersion ++ ")"
                                                , "Please upgrade to the latest version of the CUDA toolkit and reinstall the 'cuda' package."
                                                ])
        |]
      else
        [d| {-# NOINLINE libdevice_mdl #-}
            libdevice_mdl :: AST.Module
            libdevice_mdl = $libdeviceModule libdevice_bc

            libdevice_bc :: (ShortByteString,ByteString)
            libdevice_bc = $( TH.unTypeQ $ libdeviceBitcode undefined )

            instance Libdevice AST.Module where
              libdevice _ = libdevice_mdl

            instance Libdevice (ShortByteString,ByteString) where
              libdevice _ = libdevice_bc
        |]
 )

