{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile (

  module Data.Array.Accelerate.LLVM.Compile,
  ExecutableR(..), Kernel(..),

) where

-- llvm-hs
import LLVM.AST                                                     hiding ( Module )
import qualified LLVM.AST                                           as AST
import qualified LLVM.AST.Name                                      as LLVM
import qualified LLVM.Analysis                                      as LLVM
import qualified LLVM.Context                                       as LLVM
import qualified LLVM.Module                                        as LLVM
import qualified LLVM.PassManager                                   as LLVM
import qualified LLVM.Target                                        as LLVM
import qualified LLVM.Internal.Module                               as LLVM.Internal
import qualified LLVM.Internal.FFI.LLVMCTypes                       as LLVM.Internal.FFI

-- accelerate
import Data.Array.Accelerate.Error                                  ( internalError )
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Trafo                                  ( DelayedOpenAcc )

import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.CodeGen.Environment               ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                    ( Module(..) )
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State
#ifdef ACCELERATE_USE_NVVM
import Data.Array.Accelerate.LLVM.Util
#endif

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.CodeGen
import Data.Array.Accelerate.LLVM.PTX.Compile.Link
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Foreign                       ( )
import Data.Array.Accelerate.LLVM.PTX.Target

import qualified  Data.Array.Accelerate.LLVM.PTX.Debug              as Debug

-- cuda
import qualified Foreign.CUDA.Analysis                              as CUDA
import qualified Foreign.CUDA.Driver                                as CUDA
#ifdef ACCELERATE_USE_NVVM
import qualified Foreign.NVVM                                       as NVVM
#endif

-- standard library
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString                                              ( ByteString )
import Data.List                                                    ( intercalate )
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Text.Printf                                                  ( printf )
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Char8                              as B8
import qualified Data.ByteString.Internal                           as B
import qualified Data.ByteString.Unsafe                             as B
import qualified Data.Map                                           as Map
import Prelude                                                      as P


instance Compile PTX where
  type ObjectR     PTX = ( String, ByteString, [(String, LaunchConfig)] )
  data ExecutableR PTX = PTXR { ptxKernel :: ![Kernel]
                              , ptxModule :: {-# UNPACK #-} !(Lifetime CUDA.Module)
                              }
  compileForTarget  = compile
  linkForTarget     = link


data Kernel = Kernel {
    kernelFun                   :: {-# UNPACK #-} !CUDA.Fun
  , kernelOccupancy             :: {-# UNPACK #-} !CUDA.Occupancy
  , kernelSharedMemBytes        :: {-# UNPACK #-} !Int
  , kernelThreadBlockSize       :: {-# UNPACK #-} !Int
  , kernelThreadBlocks          :: (Int -> Int)
  , kernelName                  :: String
  }


-- | Compile an Accelerate expression to object code.
--
-- This generates the target code together with a list of each kernel function
-- defined in the module paired with its occupancy information.
--
compile :: DelayedOpenAcc aenv a -> Gamma aenv -> LLVM PTX (ObjectR PTX)
compile acc aenv = do
  target <- gets llvmTarget

  -- Generate code for this Acc operation
  --
  let Module ast md = llvmOfOpenAcc target acc aenv
      dev           = ptxDeviceProperties target
      name          = moduleName ast

  -- Lower the generated LLVM into PTX assembly
  --
  liftIO . LLVM.withContext $ \ctx -> do
    ptx <- compileModule dev ctx ast
    return ( name, ptx, [ (f,x) | (LLVM.Name f, KM_PTX x) <- Map.toList md ] )


-- | Compile the LLVM module to PTX assembly. This is done either by the
-- closed-source libNVVM library, or via the standard NVPTX backend (which is
-- the default).
--
compileModule :: CUDA.DeviceProperties -> LLVM.Context -> AST.Module -> IO ByteString
compileModule dev ctx ast =
#ifdef ACCELERATE_USE_NVVM
  withLibdeviceNVVM  dev ctx ast (compileModuleNVVM  dev (moduleName ast))
#else
  withLibdeviceNVPTX dev ctx ast (compileModuleNVPTX dev)
#endif


#ifdef ACCELERATE_USE_NVVM
-- Compile and optimise the module to PTX using the (closed source) NVVM
-- library. This _may_ produce faster object code than the LLVM NVPTX compiler.
--
compileModuleNVVM :: CUDA.DeviceProperties -> String -> [(String, ByteString)] -> LLVM.Module -> IO ByteString
compileModuleNVVM dev name libdevice mdl = do
  _debug <- Debug.queryFlag Debug.debug_cc
  --
  let arch    = CUDA.computeCapability dev
      verbose = if _debug then [ NVVM.GenerateDebugInfo ] else []
      flags   = NVVM.Target arch : verbose

      -- Note: [NVVM and target datalayout]
      --
      -- The NVVM library does not correctly parse the target datalayout field,
      -- instead doing a (very dodgy) string compare against exactly two
      -- expected values. This means that it is sensitive to, e.g. the ordering
      -- of the fields, and changes to the representation in each LLVM release.
      --
      -- We get around this by only specifying the data layout in a separate
      -- (otherwise empty) module that we additionally link against.
      --
      header  = case bitSize (undefined::Int) of
                  32 -> "target triple = \"nvptx-nvidia-cuda\"\ntarget datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64\""
                  64 -> "target triple = \"nvptx64-nvidia-cuda\"\ntarget datalayout = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64\""
                  _  -> $internalError "compileModuleNVVM" "I don't know what architecture I am"

  Debug.when Debug.dump_cc   $ do
    Debug.when Debug.verbose $ do
      ll <- LLVM.moduleLLVMAssembly mdl -- TLM: unfortunate to do the lowering twice in debug mode
      Debug.traceIO Debug.verbose ll

  -- Lower the generated module to bitcode, then compile and link together with
  -- the shim header and libdevice library (if necessary)
  bc  <- LLVM.moduleBitcode mdl
  ptx <- NVVM.compileModules (("",header) : (name,bc) : libdevice) flags

  unless (B.null (NVVM.compileLog ptx)) $ do
    Debug.traceIO Debug.dump_cc $ "llvm: " ++ B.unpack (NVVM.compileLog ptx)

  -- Return the generated binary code
  return (NVVM.compileResult ptx)

#else

-- Compiling with the NVPTX backend uses LLVM-3.3 and above
--
compileModuleNVPTX :: CUDA.DeviceProperties -> LLVM.Module -> IO ByteString
compileModuleNVPTX dev mdl =
  withPTXTargetMachine dev $ \nvptx -> do

    -- Run the standard optimisation pass
    --
    let pss        = LLVM.defaultCuratedPassSetSpec { LLVM.optLevel = Just 3 }
        runError e = either ($internalError "compileModuleNVPTX") id `fmap` runExceptT e

    LLVM.withPassManager pss $ \pm -> do
#ifdef ACCELERATE_INTERNAL_CHECKS
      runError $ LLVM.verify mdl
#endif
      b1      <- LLVM.runPassManager pm mdl

      -- debug printout
      Debug.when Debug.dump_cc $ do
        Debug.traceIO Debug.dump_cc $ printf "llvm: optimisation did work? %s" (show b1)
        Debug.traceIO Debug.verbose =<< LLVM.moduleLLVMAssembly mdl

      -- Lower the LLVM module into target assembly (PTX)
      runError (moduleTargetAssembly nvptx mdl)
#endif


-- | Produce target specific assembly as a 'ByteString'.
--
moduleTargetAssembly :: LLVM.TargetMachine -> LLVM.Module -> ExceptT String IO ByteString
moduleTargetAssembly tm m = unsafe0 =<< LLVM.Internal.emitToByteString LLVM.Internal.FFI.codeGenFileTypeAssembly tm m
  where
    -- Ensure that the ByteString is NULL-terminated, so that it can be passed
    -- directly to C. This will unsafely mutate the underlying ForeignPtr if the
    -- string is not NULL-terminated but the last character is a whitespace
    -- character (there are usually a few blank lines at the end).
    --
    unsafe0 :: ByteString -> ExceptT String IO ByteString
    unsafe0 bs@(B.PS fp s l) =
      liftIO . withForeignPtr fp $ \p -> do
        let p' :: Ptr Word8
            p' = p `plusPtr` (s+l-1)
        --
        x <- peek p'
        case x of
          0                    -> return bs
          _ | B.isSpaceWord8 x -> poke p' 0 >> return bs
          _                    -> return (B.snoc bs 0)


-- | Load the generated object code into the current CUDA context.
--
link :: ObjectR PTX -> LLVM PTX (ExecutableR PTX)
link (name, ptx, nm) = do
  target <- gets llvmTarget
  liftIO $ do
    _verbose      <- Debug.queryFlag Debug.verbose
    _debug        <- Debug.queryFlag Debug.debug_cc
    --
    let v         = if _verbose then [ CUDA.Verbose ]                                  else []
        d         = if _debug   then [ CUDA.GenerateDebugInfo, CUDA.GenerateLineInfo ] else []
        flags     = concat [v,d]
    --
    Debug.when (Debug.dump_asm) $
      Debug.traceIO Debug.verbose (B8.unpack ptx)

    -- Load the PTX into the current CUDA context
    jit     <- B.unsafeUseAsCString ptx $ \p -> CUDA.loadDataFromPtrEx (castPtr p) flags
    let mdl  = CUDA.jitModule jit
        info = CUDA.jitInfoLog jit

    Debug.traceIO Debug.dump_asm $
      printf "ptx: compiled entry function \"%s\" in %s"
             name
             (Debug.showFFloatSIBase (Just 2) 1000 (CUDA.jitTime jit / 1000) "s")

    Debug.when Debug.verbose $
      unless (B.null info) $
        Debug.traceIO Debug.dump_asm ("ptx: " ++ B8.unpack info)

    -- Extract the kernel functions
    kernels <- mapM (uncurry (linkFunction mdl)) nm
    mdl'    <- newLifetime mdl

    -- Finalise the module by unloading it from the CUDA context
    addFinalizer mdl' $ do
      Debug.traceIO Debug.dump_gc
        $ printf "gc: unload module: %s"
        $ intercalate "," (P.map kernelName kernels)
      withContext (ptxContext target) (CUDA.unload mdl)

    return $! PTXR kernels mdl'


-- | Extract the named function from the module and package into a Kernel
-- object, which includes meta-information on resource usage.
--
-- If we are in debug mode, print statistics on kernel resource usage, etc.
--
linkFunction
    :: CUDA.Module                      -- the compiled module
    -> String                           -- __global__ entry function name
    -> LaunchConfig                     -- launch configuration for this global function
    -> IO Kernel
linkFunction mdl name configure = do
  f     <- CUDA.getFun mdl name
  regs  <- CUDA.requires f CUDA.NumRegs
  ssmem <- CUDA.requires f CUDA.SharedSizeBytes
  cmem  <- CUDA.requires f CUDA.ConstSizeBytes
  lmem  <- CUDA.requires f CUDA.LocalSizeBytes
  maxt  <- CUDA.requires f CUDA.MaxKernelThreadsPerBlock

  let
      (occ, cta, grid, dsmem) = configure maxt regs ssmem

      msg1, msg2 :: String
      msg1 = printf "kernel function '%s' used %d registers, %d bytes smem, %d bytes lmem, %d bytes cmem"
                      name regs (ssmem + dsmem) lmem cmem

      msg2 = printf "multiprocessor occupancy %.1f %% : %d threads over %d warps in %d blocks"
                      (CUDA.occupancy100 occ)
                      (CUDA.activeThreads occ)
                      (CUDA.activeWarps occ)
                      (CUDA.activeThreadBlocks occ)

  Debug.traceIO Debug.dump_cc (printf "cc: %s\n  ... %s" msg1 msg2)
  return $ Kernel f occ dsmem cta grid name


{--
-- | Extract the names of the function definitions from the module.
--
-- Note: [Extracting global function names]
--
-- It is important to run this on the module given to us by code generation.
-- After combining modules with 'libdevice', extra function definitions,
-- corresponding to basic maths operations, will be added to the module. These
-- functions will not be callable as __global__ functions.
--
-- The list of names will be exported in the order that they appear in the
-- module.
--
globalFunctions :: [Definition] -> [String]
globalFunctions defs =
  [ n | GlobalDefinition Function{..} <- defs
      , not (null basicBlocks)
      , let Name n = name
      ]
--}

