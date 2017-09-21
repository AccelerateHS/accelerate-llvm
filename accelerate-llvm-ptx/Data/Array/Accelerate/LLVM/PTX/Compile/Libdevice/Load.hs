{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.Load
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

-- standard library
import Data.ByteString                                              ( ByteString )
import System.IO.Unsafe


-- NVVM Reflect
-- ------------

class NVVMReflect a where
  nvvmReflect :: a

instance NVVMReflect AST.Module where
  nvvmReflect = nvvmReflectModule

instance NVVMReflect (String, ByteString) where
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

instance Libdevice AST.Module where
  libdevice (Compute n m) =
    case (n,m) of
      (2,_)             -> libdevice_20_mdl   -- 2.0, 2.1
      (3,x) | x < 5     -> libdevice_30_mdl   -- 3.0, 3.2
            | otherwise -> libdevice_35_mdl   -- 3.5, 3.7
      (5,_)             -> libdevice_50_mdl   -- 5.x
      (6,_)             -> libdevice_50_mdl   -- 6.x
      _                 -> $internalError "libdevice" "no binary for this architecture"

instance Libdevice (String, ByteString) where
  libdevice (Compute n m) =
    case (n,m) of
      (2,_)             -> libdevice_20_bc    -- 2.0, 2.1
      (3,x) | x < 5     -> libdevice_30_bc    -- 3.0, 3.2
            | otherwise -> libdevice_35_bc    -- 3.5, 3.7
      (5,_)             -> libdevice_50_bc    -- 5.x
      (6,_)             -> libdevice_50_bc    -- 6.x
      _                 -> $internalError "libdevice" "no binary for this architecture"


-- Load the libdevice bitcode files as an LLVM AST module. The top-level
-- unsafePerformIO ensures that the data is only read from disk once per program
-- execution.
--
{-# NOINLINE libdevice_20_mdl #-}
{-# NOINLINE libdevice_30_mdl #-}
{-# NOINLINE libdevice_35_mdl #-}
{-# NOINLINE libdevice_50_mdl #-}
libdevice_20_mdl, libdevice_30_mdl, libdevice_35_mdl, libdevice_50_mdl :: AST.Module
libdevice_20_mdl = unsafePerformIO $ libdeviceModule (Compute 2 0)
libdevice_30_mdl = unsafePerformIO $ libdeviceModule (Compute 3 0)
libdevice_35_mdl = unsafePerformIO $ libdeviceModule (Compute 3 5)
libdevice_50_mdl = unsafePerformIO $ libdeviceModule (Compute 5 0)

-- Load the libdevice bitcode files as raw binary data.
--
libdevice_20_bc, libdevice_30_bc, libdevice_35_bc, libdevice_50_bc :: (String,ByteString)
libdevice_20_bc = $$( libdeviceBitcode (Compute 2 0) )
libdevice_30_bc = $$( libdeviceBitcode (Compute 3 0) )
libdevice_35_bc = $$( libdeviceBitcode (Compute 3 5) )
libdevice_50_bc = $$( libdeviceBitcode (Compute 5 0) )


-- Load the libdevice bitcode file for the given compute architecture, and raise
-- it to a Haskell AST that can be kept for future use. The name of the bitcode
-- files follows:
--
--   libdevice.compute_XX.YY.bc
--
-- Where XX represents the compute capability, and YY represents a version(?) We
-- search the libdevice PATH for all files of the appropriate compute capability
-- and load the most recent.
--
libdeviceModule :: Compute -> IO AST.Module
libdeviceModule arch = do
  let bc :: (String, ByteString)
      bc = libdevice arch
  --
  withContext $ \ctx ->
    withModuleFromBitcode ctx bc moduleAST

