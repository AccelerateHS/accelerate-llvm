{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Compile
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Compile (

  module Data.Array.Accelerate.LLVM.Compile

) where

-- llvm-general
import LLVM.General.AST                                         hiding ( Module )
import LLVM.General.AST.Global
import qualified LLVM.General.Analysis                          as LLVM
import qualified LLVM.General.Module                            as LLVM
import qualified LLVM.General.PassManager                       as LLVM

-- accelerate
import Data.Array.Accelerate.Trafo                              ( DelayedOpenAcc )

import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                ( Module(..) )
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.NVVM.Target
import Data.Array.Accelerate.LLVM.NVVM.Compile.Link
import Data.Array.Accelerate.LLVM.NVVM.CodeGen                  ( )
import Data.Array.Accelerate.LLVM.NVVM.Analysis.Launch

import qualified  Data.Array.Accelerate.LLVM.NVVM.Debug         as Debug

-- cuda
import qualified Foreign.CUDA.Analysis                          as CUDA
import qualified Foreign.CUDA.Driver                            as CUDA
#ifdef ACCELERATE_USE_LIBNVVM
import qualified Foreign.LibNVVM                                as NVVM
#endif

-- standard library
import Data.ByteString                                          ( ByteString )
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Text.Printf
import System.IO.Unsafe
import qualified Data.ByteString.Char8                          as B

#include "accelerate.h"


instance Compile NVVM where
  compileForTarget = compileForNVPTX


-- | Compile a given module for the NVPTX backend. This produces a CUDA module
-- as well as a list of the kernel functions in the module, together with some
-- occupancy information.
--
compileForNVPTX
    :: DelayedOpenAcc aenv a
    -> Gamma aenv
    -> LLVM NVVM (ExecutableR NVVM)
compileForNVPTX acc aenv = do
  nvvm   <- gets llvmTarget
  ctx    <- asks llvmContext
  let Module ast = llvmOfAcc nvvm acc aenv
      dev        = nvvmDeviceProperties nvvm
      name       = moduleName ast
      globals    = globalFunctions (moduleDefinitions ast)

  liftIO $ do
    ptx  <- withLibdevice dev ctx ast (compileModule dev name)
    funs <- sequence [ linkFunction acc dev ptx f | f <- globals ]
    return $! NVVMR funs ptx


-- | Compile the LLVM module to produce a CUDA module.
--
--    * If we are using libNVVM, this includes all LLVM optimisations plus some
--    sekrit optimisations.
--
--    * If we are just using the llvm ptx backend, we still need to run the
--    standard optimisations.
--
compileModule :: CUDA.DeviceProperties -> String -> LLVM.Module -> IO CUDA.Module
compileModule dev name mdl =
#ifdef ACCELERATE_USE_LIBNVVM
  compileModuleNVVM dev name mdl
#else
  compileModuleNVPTX dev name mdl
#endif

#ifdef ACCELERATE_USE_LIBNVVM
-- Compiling the module with libNVVM implicitly uses LLVM-3.2.
--
compileModuleNVVM :: CUDA.DeviceProperties -> String -> LLVM.Module -> IO CUDA.Module
compileModuleNVVM dev name mdl = do
  -- Lower the module to bitcode and have libNVVM compile to PTX
  let arch    = CUDA.computeCapability dev
      verbose = if Debug.mode Debug.debug then [ NVVM.GenerateDebugInfo ] else []

  ll    <- LLVM.moduleString mdl        -- no LLVM.moduleBitcode in llvm-general-3.2.*
  ptx   <- NVVM.compileModule (B.pack ll) name (Target arch : verbose)

  -- Debugging
  Debug.when Debug.dump_llvm $ do
    Debug.message Debug.verbose ll
    Debug.message Debug.dump_llvm $ "llvm: " ++ B.unpack (NVVM.nvvmLog ptx)

  -- Link into a new CUDA module in the current context
  linkPTX name (NVVM.nvvmResult ptx)

#else
-- Compiling with the NVPTX backend uses LLVM-3.3 and above
--
compileModuleNVPTX :: CUDA.DeviceProperties -> String -> LLVM.Module -> IO CUDA.Module
compileModuleNVPTX dev name mdl =
  withNVVMTargetMachine dev $ \nvptx -> do

    -- Run the standard optimisation pass
    let pss        = LLVM.defaultCuratedPassSetSpec { LLVM.optLevel = Just 3 }
        runError e = either (INTERNAL_ERROR(error) "compileModuleNVPTX") id `fmap` runErrorT e

    LLVM.withPassManager pss $ \pm -> do
      runError $ LLVM.verify mdl
      void     $ LLVM.runPassManager pm mdl

      -- Lower the LLVM module into target assembly (PTX)
      ptx <- runError (LLVM.moduleTargetAssembly nvptx mdl)

      -- debug printout
      Debug.when Debug.dump_llvm $
        Debug.message Debug.verbose =<< LLVM.moduleLLVMAssembly mdl

      -- Link into a new CUDA module in the current context
      linkPTX name (B.pack ptx)
#endif

-- | Load the given CUDA PTX into a new module that is linked into the current
-- context.
--
linkPTX :: String -> ByteString -> IO CUDA.Module
linkPTX name ptx = do
  let v         = if Debug.mode Debug.verbose then [ CUDA.Verbose ]                                  else []
      d         = if Debug.mode Debug.debug   then [ CUDA.GenerateDebugInfo, CUDA.GenerateLineInfo ] else []
      flags     = concat [v,d]
  --
  jit   <- CUDA.loadDataEx ptx flags

  Debug.when Debug.dump_ptx $ do
    Debug.message Debug.verbose (B.unpack ptx)
    Debug.message Debug.dump_ptx $
      printf "ptx: compiled entry function \"%s\" in %s\n%s"
             name
             (Debug.showFFloatSIBase (Just 2) 1000 (CUDA.jitTime jit / 1000) "s")
             (B.unpack (CUDA.jitInfoLog jit))

  return $! CUDA.jitModule jit


-- | Extract the named function from the module and package into a Kernel
-- object, which includes meta-information on resource usage.
--
-- If we are in debug mode, print statistics on kernel resource usage, etc.
--
linkFunction
    :: DelayedOpenAcc aenv a
    -> CUDA.DeviceProperties            -- device properties
    -> CUDA.Module                      -- the compiled module
    -> String                           -- __global__ entry function name
    -> IO Kernel
linkFunction acc dev mdl name = do
  f     <- CUDA.getFun mdl name
  regs  <- CUDA.requires f CUDA.NumRegs
  ssmem <- CUDA.requires f CUDA.SharedSizeBytes
  cmem  <- CUDA.requires f CUDA.ConstSizeBytes
  lmem  <- CUDA.requires f CUDA.LocalSizeBytes
  maxt  <- CUDA.requires f CUDA.MaxKernelThreadsPerBlock

  let occ               = determineOccupancy acc dev maxt regs ssmem
      (cta,blocks,smem) = launchConfig acc dev occ

      msg1, msg2 :: String
      msg1 = printf "entry function '%s' used %d registers, %d bytes smem, %d bytes lmem, %d bytes cmem"
                      name regs smem lmem cmem

      msg2 = printf "multiprocessor occupancy %.1f %% : %d threads over %d warps in %d blocks"
                      (CUDA.occupancy100 occ)
                      (CUDA.activeThreads occ)
                      (CUDA.activeWarps occ)
                      (CUDA.activeThreadBlocks occ)

  Debug.message Debug.dump_cc (printf "cc: %s\n  ... %s" msg1 msg2)
  return $ Kernel f occ smem cta blocks name


-- | Extract the names of the global function definitions from the module. These
-- correspond to the __global__ entry functions.
--
globalFunctions :: [Definition] -> [String]
globalFunctions defs =
  [ n | GlobalDefinition Function{..} <- defs
      , not (null basicBlocks)
      , let Name n = name
      ]

