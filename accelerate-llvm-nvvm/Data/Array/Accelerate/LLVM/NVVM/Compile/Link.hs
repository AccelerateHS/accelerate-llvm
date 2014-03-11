{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Compile.Link
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Compile.Link (

  withLibdevice

) where

-- llvm-general
import LLVM.General.Context
import qualified LLVM.General.Module                            as LLVM

import LLVM.General.AST                                         as AST
import LLVM.General.AST.Attribute
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Global                                  as G
import LLVM.General.AST.Linkage

-- accelerate
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Monad                 () -- instance IsString Name
import Data.Array.Accelerate.LLVM.CodeGen.Type

import qualified Data.Array.Accelerate.LLVM.NVVM.Debug          as Debug

-- cuda
import Foreign.CUDA.Analysis

-- standard library
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad.Error
import System.IO.Unsafe
import System.FilePath
import System.Directory
import Text.Printf

import Data.Hashable
import Data.HashSet                                             ( HashSet )
import qualified Data.ByteString                                as B
import qualified Data.HashSet                                   as Set

#include "accelerate.h"

instance Hashable Name where
  hashWithSalt salt (Name n)   = 0 `hashWithSalt` salt `hashWithSalt` n
  hashWithSalt salt (UnName n) = 1 `hashWithSalt` salt `hashWithSalt` n


-- | Lower an LLVM AST to C++ objects and link it against the libdevice module,
-- iff any libdevice functions are referenced from the base module.
--
-- Note: [Linking with libdevice]
--
-- The CUDA toolkit comes with an LLVM bitcode library called 'libdevice' that
-- implements many common mathematical functions. The library can be used as a
-- high performance math library for targets of the LLVM NVPTX backend, such as
-- this one. To link a module 'foo' with libdevice, the following compilation
-- pipeline is recommended:
--
--   1. Save all external functions in module 'foo'
--
--   2. Link module 'foo' with the appropriate 'libdevice_compute_XX.YY.bc'
--
--   3. Internalise all functions not in the list from (1)
--
--   4. Eliminate all unused internal functions
--
--   5. Run the NVVMReflect pass (see below)
--
--   6. Run the standard optimisation pipeline
--
--
-- Note: [NVVMReflect]
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

withLibdevice :: DeviceProperties -> Context -> Module -> (LLVM.Module -> IO a) -> IO a
withLibdevice dev ctx (analyse -> (externs, ast)) next =
  case Set.null externs of
    True        -> runError $ LLVM.withModuleFromAST ctx ast next
    False       ->
      let arch = computeCapability dev
      in do
      Debug.message Debug.dump_llvm $
        printf "llvm: linking with libdevice: %s"
               (intercalate ", " (map (\(Name s) -> s) (Set.toList externs)))

      runError $ LLVM.withModuleFromAST ctx ast                                    $ \mdl  -> do
      runError $ LLVM.withModuleFromAST ctx __nvvm_reflect                         $ \refl -> do
      runError $ LLVM.withModuleFromAST ctx (internalise externs (libdevice arch)) $ \libd -> do
        runError $ LLVM.linkModules False libd refl
        runError $ LLVM.linkModules False libd mdl
        next libd
  where
    runError e = either (INTERNAL_ERROR(error) "withLibdevice") id `fmap` runErrorT e

    __nvvm_reflect =
      let i32   = typeOf (integralType :: IntegralType Int32)
          i8p   = PointerType (typeOf (integralType :: IntegralType Int8)) (AddrSpace 0)
      in
      defaultModule {
        moduleDefinitions = [GlobalDefinition $ functionDefaults
          { name                 = "__nvvm_reflect"
          , returnType           = i32
          , parameters           = ( [Parameter i8p (UnName 0) []], False )
          , G.functionAttributes = [NoUnwind, ReadNone, AlwaysInline]
          , basicBlocks          = [
              BasicBlock "" [] (Do $ Ret (Just (constOp (num int32 0))) []) ]
          }]
        }

-- | Analyse the LLVM AST module and determine if any of the external
-- declarations should be substituted for functions defined in libdevice. The
-- set of functions that must be linked against libdevice are returned.
--
analyse :: Module -> (HashSet Name, Module)
analyse Module{..} =
  let (externs, defs)   = foldr1 (<>) $ map subst moduleDefinitions
  in
  (externs, Module { moduleDefinitions = defs, ..})


-- | Substitute suitable external declarations to calls to libdevice functions.
-- If an appropriate declaration is found (say 'sinf'), then we output two
-- declarations:
--
--   * The first is a replacement for the existing global declaration, that
--     replaces the empty function body with a call to the equivalent libdevice
--     function. The function is marked 'AlwaysInline', which will eliminate the
--     indirection.
--
--   * The second global is a declaration to the invoked libdevice function.
--     During compilation, the module must be linked against the libdevice
--     bitcode file.
--
-- If no substitutions are made then the Definition is returned unaltered. If
-- the substitution is made, the name of the libdevice function is returned in a
-- singleton set as the first parameter.
--
subst :: Definition -> (HashSet Name, [Definition])
subst (GlobalDefinition Function{..})
  | null basicBlocks
  , Set.member name libdeviceIndex
  , let __nv_name       = let Name f = name in Name ("__nv_" ++ f)
        attrs           = AlwaysInline : functionAttributes
        args            = [ n | Parameter _ n _ <- fst parameters ]
        toArgs          = map (\x -> (local x, []))
        blocks          = [ BasicBlock "" [ UnName 0 := Call False C [] (Right (global __nv_name)) (toArgs args) attrs [] ]
                                          ( Do        $ Ret (Just (local (UnName 0))) [])
                          ]
  = ( Set.singleton __nv_name
    , [ GlobalDefinition (Function { basicBlocks = blocks, functionAttributes = attrs, .. })
      , GlobalDefinition (Function { name = __nv_name, .. }) ]
    )

subst x = (Set.empty, [x])


-- | Mark all definitions in the module as internal linkage. This means that
-- unused definitions can be removed as dead code. Be careful to leave any
-- declarations as external.
--
internalise :: HashSet Name -> Module -> Module
internalise externals Module{..} =
  let internal (GlobalDefinition Function{..})
        | not (Set.member name externals)       -- we don't call this function directly; and
        , not (null basicBlocks)                -- it is not an external declaration
        = GlobalDefinition (Function { linkage=Internal, .. })

      internal x
        = x
  in
  Module { moduleDefinitions = map internal moduleDefinitions, .. }


-- | Return the appropriate libdevice bitcode file for a given architecture.
--
libdevice :: Compute -> Module
libdevice (Compute m n) =
  case (m,n) of
    (2,0) -> libdevice_20
    (3,0) -> libdevice_30
    (3,5) -> libdevice_35
    _     -> INTERNAL_ERROR(error) "libdevice" "no binary for this architecture"

-- Load the libdevice bitcode files. The top-level unsafePerformIO ensures that
-- the data is only read from disk once per program execution.
--
{-# NOINLINE libdevice_20 #-}
{-# NOINLINE libdevice_30 #-}
{-# NOINLINE libdevice_35 #-}
libdevice_20, libdevice_30, libdevice_35 :: Module
libdevice_20 = unsafePerformIO $ loadLibdevice (Compute 2 0)
libdevice_30 = unsafePerformIO $ loadLibdevice (Compute 3 0)
libdevice_35 = unsafePerformIO $ loadLibdevice (Compute 3 5)


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
loadLibdevice :: Compute -> IO Module
loadLibdevice (Compute m n) = do
  let arch       = printf "libdevice.compute_%d%d" m n
      err        = INTERNAL_ERROR(error) "loadLibdevice" (printf "not found: %s.YY.bc" arch)
      best f     = arch `isPrefixOf` f && takeExtension f == ".bc"

  path  <- libdevicePath
  files <- getDirectoryContents path
  name  <- maybe err return . listToMaybe . sortBy (flip compare) $ filter best files
  bc    <- B.readFile (path </> name)

  -- TLM: we have called 'withContext' again here, although the LLVM state
  --      already carries a version of the context. We do this so that we can
  --      fully apply this function that can be lifted out to a CAF and only
  --      executed once per program execution.
  --
  Module{..} <- withContext $ \ctx ->
    either (INTERNAL_ERROR(error) "loadLibdevice") id `fmap`
    runErrorT (LLVM.withModuleFromBitcode ctx (name,bc) LLVM.moduleAST)

  -- This is to avoid the warning message:
  --
  --   WARNING: Linking two modules of different target triples: map:
  --            'nvptx64-nvidia-cuda' and 'nvptx-nvidia-cl.1.0'
  --
  -- We can't use the second target, used by libdevice*.bc, because we get an
  -- unknown internal driver error code.
  --
  return $! Module { moduleTargetTriple=Nothing, .. }


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


-- The list of functions implemented by libdevice. These are all more-or-less
-- named consistently based on the standard mathematical functions they
-- implement, with the "__nv_" prefix stripped.
--
libdeviceIndex :: HashSet Name
libdeviceIndex = Set.fromList
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

