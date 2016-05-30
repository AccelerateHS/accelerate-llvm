{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice (

  nvvmReflect, libdevice,

) where

-- llvm-general
import LLVM.General.Context
import LLVM.General.Module                                      as LLVM

import LLVM.General.AST                                         as AST ( Module(..), Definition(..), defaultModule )
import LLVM.General.AST.Instruction                             as AST ( Named(..) )
import LLVM.General.AST.Attribute
import LLVM.General.AST.Global                                  as G
import qualified LLVM.General.AST.Name                          as AST

-- accelerate
import LLVM.General.AST.Type.Name                               ( Label(..) )
import LLVM.General.AST.Type.Terminator                         ( Terminator(..) )

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Downcast
import Data.Array.Accelerate.LLVM.CodeGen.Intrinsic

import Data.Array.Accelerate.LLVM.PTX.Target

-- cuda
import Foreign.CUDA.Analysis

-- standard library
import Control.Monad.Except
import Data.ByteString                                          ( ByteString )
import Data.HashMap.Strict                                      ( HashMap )
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO.Unsafe
import Text.Printf
import qualified Data.ByteString                                as B
import qualified Data.ByteString.Char8                          as B8
import qualified Data.HashMap.Strict                            as HashMap


-- NVVM Reflect
-- ------------

class NVVMReflect a where
  nvvmReflect :: a

instance NVVMReflect AST.Module where
  nvvmReflect = nvvmReflectPass_mdl

instance NVVMReflect (String, ByteString) where
  nvvmReflect = nvvmReflectPass_bc


-- This is a hacky module that can be linked against in order to provide the
-- same functionality as running the NVVMReflect pass.
--
-- Note: [NVVM Reflect Pass]
--
-- To accommodate various math-related compiler flags that can affect code
-- generation of libdevice code, the library code depends on a special LLVM IR
-- pass (NVVMReflect) to handle conditional compilation within LLVM IR. This
-- pass looks for calls to the @__nvvm_reflect function and replaces them with
-- constants based on the defined reflection parameters.
--
-- libdevice currently uses the following reflection parameters to control code
-- generation:
--
--   * __CUDA_FTZ={0,1}     fast math that flushes denormals to zero
--
-- Since this is currently the only reflection parameter supported, and that we
-- prefer correct results over pure speed, we do not flush denormals to zero. If
-- the list of supported parameters ever changes, we may need to re-evaluate
-- this implementation.
--
nvvmReflectPass_mdl :: AST.Module
nvvmReflectPass_mdl =
  AST.defaultModule {
    moduleDefinitions = [GlobalDefinition $ functionDefaults
      { name                 = AST.Name "__nvvm_reflect"
      , returnType           = downcast (integralType :: IntegralType Int32)
      , parameters           = ( [ptrParameter scalarType (UnName 0 :: Name Int8)], False )
#if MIN_VERSION_llvm_general(3,5,0)
      , G.functionAttributes = map Right [NoUnwind, ReadNone, AlwaysInline]
#else
      , G.functionAttributes = [NoUnwind, ReadNone, AlwaysInline]
#endif
      , basicBlocks          = [BasicBlock (AST.Name "") [] (AST.Do $ downcast (RetVal (num numType (0::Int32))))]
      }]
    }

{-# NOINLINE nvvmReflectPass_bc #-}
nvvmReflectPass_bc :: (String, ByteString)
nvvmReflectPass_bc = (name,) . unsafePerformIO $ do
  withContext $ \ctx -> do
    runError  $ withModuleFromAST ctx nvvmReflectPass_mdl (return . B8.pack <=< moduleLLVMAssembly)
  where
    name     = "__nvvm_reflect"
    runError = either ($internalError "nvvmReflectPass") return <=< runExceptT
#if !MIN_VERSION_llvm_general(3,3,0)
    moduleLLVMAssembly = moduleString
#endif


-- libdevice
-- ---------

class Libdevice a where
  libdevice :: Compute -> a

instance Libdevice AST.Module where
  libdevice (Compute n m) =
    case (n,m) of
      (2,_)             -> libdevice_20_mdl   -- 2.0, 2.1
      (3,n) | n < 5     -> libdevice_30_mdl   -- 3.0, 3.2
            | otherwise -> libdevice_35_mdl   -- 3.5, 3.7
      (5,_)             -> libdevice_50_mdl   -- 5.0
      _                 -> $internalError "libdevice" "no binary for this architecture"

instance Libdevice (String, ByteString) where
  libdevice (Compute n m) =
    case (n,m) of
      (2,_)             -> libdevice_20_bc    -- 2.0, 2.1
      (3,n) | n < 5     -> libdevice_30_bc    -- 3.0, 3.2
            | otherwise -> libdevice_35_bc    -- 3.5, 3.7
      (5,_)             -> libdevice_50_bc    -- 5.0
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

-- Load the libdevice bitcode files as raw binary data. The top-level
-- unsafePerformIO ensures that the data is read only once per program
-- execution.
--
{-# NOINLINE libdevice_20_bc #-}
{-# NOINLINE libdevice_30_bc #-}
{-# NOINLINE libdevice_35_bc #-}
{-# NOINLINE libdevice_50_bc #-}
libdevice_20_bc, libdevice_30_bc, libdevice_35_bc, libdevice_50_bc :: (String,ByteString)
libdevice_20_bc = unsafePerformIO $ libdeviceBitcode (Compute 2 0)
libdevice_30_bc = unsafePerformIO $ libdeviceBitcode (Compute 3 0)
libdevice_35_bc = unsafePerformIO $ libdeviceBitcode (Compute 3 5)
libdevice_50_bc = unsafePerformIO $ libdeviceBitcode (Compute 5 0)


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

  -- TLM: we have called 'withContext' again here, although the LLVM state
  --      already carries a version of the context. We do this so that we can
  --      fully apply this function that can be lifted out to a CAF and only
  --      executed once per program execution.
  --
  Module{..} <- withContext $ \ctx ->
    either ($internalError "libdeviceModule") id `fmap`
    runExceptT (withModuleFromBitcode ctx bc moduleAST)

  -- This is to avoid the warning message:
  --
  --   WARNING: Linking two modules of different target triples: map:
  --            'nvptx64-nvidia-cuda' and 'nvptx-nvidia-cl.1.0'
  --
  -- We can't use the second target, used by libdevice*.bc, because we get an
  -- unknown internal driver error code.
  --
  return $! Module { moduleTargetTriple=Nothing, .. }

#if !MIN_VERSION_llvm_general(3,3,0)
withModuleFromBitcode :: Context -> (String,ByteString) -> (LLVM.Module -> IO a) -> ErrorT String IO a
withModuleFromBitcode _ _ _ = error "withModuleFromBitcode: requires llvm-general >= 3.3"
#endif


-- Load the libdevice bitcode file for the given compute architecture. The name
-- of the bitcode files follows the format:
--
--   libdevice.compute_XX.YY.bc
--
-- Where XX represents the compute capability, and YY represents a version(?) We
-- search the libdevice PATH for all files of the appropriate compute capability
-- and load the "most recent" (by sort order).
--
libdeviceBitcode :: Compute -> IO (String, ByteString)
libdeviceBitcode (Compute m n) = do
  let arch       = printf "libdevice.compute_%d%d" m n
      err        = $internalError "libdevice" (printf "not found: %s.YY.bc" arch)
      best f     = arch `isPrefixOf` f && takeExtension f == ".bc"

  path  <- libdevicePath
  files <- getDirectoryContents path
  name  <- maybe err return . listToMaybe . sortBy (flip compare) $ filter best files
  bc    <- B.readFile (path </> name)

  return (name, bc)


-- Determine the location of the libdevice bitcode libraries. We search for the
-- location of the 'nvcc' executable in the PATH. From that, we assume the
-- location of the libdevice bitcode files.
--
libdevicePath :: IO FilePath
libdevicePath = do
  nvcc  <- fromMaybe (error "could not find 'nvcc' in PATH") `fmap` findExecutable "nvcc"

  let ccvn = reverse (splitPath nvcc)
      dir  = "libdevice" : "nvvm" : drop 2 ccvn

  return (joinPath (reverse dir))


instance Intrinsic PTX where
  intrinsicForTarget _ = libdeviceIndex

-- The list of functions implemented by libdevice. These are all more-or-less
-- named consistently based on the standard mathematical functions they
-- implement, with the "__nv_" prefix stripped.
--
libdeviceIndex :: HashMap String Label
libdeviceIndex =
  let nv base   = (base, Label $ "__nv_" ++ base)
  in
  HashMap.fromList $ map nv
    [ "abs"
    , "acos"
    , "acosf"
    , "acosh"
    , "acoshf"
    , "asin"
    , "asinf"
    , "asinh"
    , "asinhf"
    , "atan"
    , "atan2"
    , "atan2f"
    , "atanf"
    , "atanh"
    , "atanhf"
    , "brev"
    , "brevll"
    , "byte_perm"
    , "cbrt"
    , "cbrtf"
    , "ceil"
    , "ceilf"
    , "clz"
    , "clzll"
    , "copysign"
    , "copysignf"
    , "cos"
    , "cosf"
    , "cosh"
    , "coshf"
    , "cospi"
    , "cospif"
    , "dadd_rd"
    , "dadd_rn"
    , "dadd_ru"
    , "dadd_rz"
    , "ddiv_rd"
    , "ddiv_rn"
    , "ddiv_ru"
    , "ddiv_rz"
    , "dmul_rd"
    , "dmul_rn"
    , "dmul_ru"
    , "dmul_rz"
    , "double2float_rd"
    , "double2float_rn"
    , "double2float_ru"
    , "double2float_rz"
    , "double2hiint"
    , "double2int_rd"
    , "double2int_rn"
    , "double2int_ru"
    , "double2int_rz"
    , "double2ll_rd"
    , "double2ll_rn"
    , "double2ll_ru"
    , "double2ll_rz"
    , "double2loint"
    , "double2uint_rd"
    , "double2uint_rn"
    , "double2uint_ru"
    , "double2uint_rz"
    , "double2ull_rd"
    , "double2ull_rn"
    , "double2ull_ru"
    , "double2ull_rz"
    , "double_as_longlong"
    , "drcp_rd"
    , "drcp_rn"
    , "drcp_ru"
    , "drcp_rz"
    , "dsqrt_rd"
    , "dsqrt_rn"
    , "dsqrt_ru"
    , "dsqrt_rz"
    , "erf"
    , "erfc"
    , "erfcf"
    , "erfcinv"
    , "erfcinvf"
    , "erfcx"
    , "erfcxf"
    , "erff"
    , "erfinv"
    , "erfinvf"
    , "exp"
    , "exp10"
    , "exp10f"
    , "exp2"
    , "exp2f"
    , "expf"
    , "expm1"
    , "expm1f"
    , "fabs"
    , "fabsf"
    , "fadd_rd"
    , "fadd_rn"
    , "fadd_ru"
    , "fadd_rz"
    , "fast_cosf"
    , "fast_exp10f"
    , "fast_expf"
    , "fast_fdividef"
    , "fast_log10f"
    , "fast_log2f"
    , "fast_logf"
    , "fast_powf"
    , "fast_sincosf"
    , "fast_sinf"
    , "fast_tanf"
    , "fdim"
    , "fdimf"
    , "fdiv_rd"
    , "fdiv_rn"
    , "fdiv_ru"
    , "fdiv_rz"
    , "ffs"
    , "ffsll"
    , "finitef"
    , "float2half_rn"
    , "float2int_rd"
    , "float2int_rn"
    , "float2int_ru"
    , "float2int_rz"
    , "float2ll_rd"
    , "float2ll_rn"
    , "float2ll_ru"
    , "float2ll_rz"
    , "float2uint_rd"
    , "float2uint_rn"
    , "float2uint_ru"
    , "float2uint_rz"
    , "float2ull_rd"
    , "float2ull_rn"
    , "float2ull_ru"
    , "float2ull_rz"
    , "float_as_int"
    , "floor"
    , "floorf"
    , "fma"
    , "fma_rd"
    , "fma_rn"
    , "fma_ru"
    , "fma_rz"
    , "fmaf"
    , "fmaf_rd"
    , "fmaf_rn"
    , "fmaf_ru"
    , "fmaf_rz"
    , "fmax"
    , "fmaxf"
    , "fmin"
    , "fminf"
    , "fmod"
    , "fmodf"
    , "fmul_rd"
    , "fmul_rn"
    , "fmul_ru"
    , "fmul_rz"
    , "frcp_rd"
    , "frcp_rn"
    , "frcp_ru"
    , "frcp_rz"
    , "frexp"
    , "frexpf"
    , "frsqrt_rn"
    , "fsqrt_rd"
    , "fsqrt_rn"
    , "fsqrt_ru"
    , "fsqrt_rz"
    , "fsub_rd"
    , "fsub_rn"
    , "fsub_ru"
    , "fsub_rz"
    , "hadd"
    , "half2float"
    , "hiloint2double"
    , "hypot"
    , "hypotf"
    , "ilogb"
    , "ilogbf"
    , "int2double_rn"
    , "int2float_rd"
    , "int2float_rn"
    , "int2float_ru"
    , "int2float_rz"
    , "int_as_float"
    , "isfinited"
    , "isinfd"
    , "isinff"
    , "isnand"
    , "isnanf"
    , "j0"
    , "j0f"
    , "j1"
    , "j1f"
    , "jn"
    , "jnf"
    , "ldexp"
    , "ldexpf"
    , "lgamma"
    , "lgammaf"
    , "ll2double_rd"
    , "ll2double_rn"
    , "ll2double_ru"
    , "ll2double_rz"
    , "ll2float_rd"
    , "ll2float_rn"
    , "ll2float_ru"
    , "ll2float_rz"
    , "llabs"
    , "llmax"
    , "llmin"
    , "llrint"
    , "llrintf"
    , "llround"
    , "llroundf"
    , "log"
    , "log10"
    , "log10f"
    , "log1p"
    , "log1pf"
    , "log2"
    , "log2f"
    , "logb"
    , "logbf"
    , "logf"
    , "longlong_as_double"
    , "max"
    , "min"
    , "modf"
    , "modff"
    , "mul24"
    , "mul64hi"
    , "mulhi"
    , "nan"
    , "nanf"
    , "nearbyint"
    , "nearbyintf"
    , "nextafter"
    , "nextafterf"
    , "normcdf"
    , "normcdff"
    , "normcdfinv"
    , "normcdfinvf"
    , "popc"
    , "popcll"
    , "pow"
    , "powf"
    , "powi"
    , "powif"
    , "rcbrt"
    , "rcbrtf"
    , "remainder"
    , "remainderf"
    , "remquo"
    , "remquof"
    , "rhadd"
    , "rint"
    , "rintf"
    , "round"
    , "roundf"
    , "rsqrt"
    , "rsqrtf"
    , "sad"
    , "saturatef"
    , "scalbn"
    , "scalbnf"
    , "signbitd"
    , "signbitf"
    , "sin"
    , "sincos"
    , "sincosf"
    , "sincospi"
    , "sincospif"
    , "sinf"
    , "sinh"
    , "sinhf"
    , "sinpi"
    , "sinpif"
    , "sqrt"
    , "sqrtf"
    , "tan"
    , "tanf"
    , "tanh"
    , "tanhf"
    , "tgamma"
    , "tgammaf"
    , "trunc"
    , "truncf"
    , "uhadd"
    , "uint2double_rn"
    , "uint2float_rd"
    , "uint2float_rn"
    , "uint2float_ru"
    , "uint2float_rz"
    , "ull2double_rd"
    , "ull2double_rn"
    , "ull2double_ru"
    , "ull2double_rz"
    , "ull2float_rd"
    , "ull2float_rn"
    , "ull2float_ru"
    , "ull2float_rz"
    , "ullmax"
    , "ullmin"
    , "umax"
    , "umin"
    , "umul24"
    , "umul64hi"
    , "umulhi"
    , "urhadd"
    , "usad"
    , "y0"
    , "y0f"
    , "y1"
    , "y1f"
    , "yn"
    , "ynf"
    ]

