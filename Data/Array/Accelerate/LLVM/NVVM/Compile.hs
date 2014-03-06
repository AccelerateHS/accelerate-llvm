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
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Text.Printf
import System.IO.Unsafe
import qualified Data.ByteString.Char8                          as B

import GHC.Conc                                                 ( par )

#ifdef ACCELERATE_USE_LIBNVVM
#include "accelerate.h"
#endif


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

  -- We use 'unsafePerformIO' here to leverage Haskell's non-strict semantics,
  -- so that we only block on module generation once the function is truly
  -- needed during the execution phase.
  --
  let exe = unsafePerformIO $ do
              ptx  <- either error return =<< runErrorT
                        (LLVM.withModuleFromAST ctx ast (compileModule dev (moduleName ast)))
              --
              funs <- sequence [ linkFunction acc dev ptx f | f <- globalFunctions (moduleDefinitions ast) ]
              return (NVVMR funs ptx)

  exe `par` return exe


-- | Compile the LLVM module to produce a CUDA module.
--
--    * If we are using libNVVM, this includes all LLVM optimisations plus some
--    sekrit optimisations.
--
--    * If we are just using the llvm ptx backend, we still need to run the
--    standard optimisations.
--
compileModule :: CUDA.DeviceProperties -> String -> LLVM.Module -> IO CUDA.Module
compileModule _dev _name mdl =
#ifdef ACCELERATE_USE_LIBNVVM
  compileModuleNVVM _dev _name mdl
#else
  compileModuleNVPTX mdl
#endif

#ifdef ACCELERATE_USE_LIBNVVM
compileModuleNVVM :: CUDA.DeviceProperties -> String -> LLVM.Module -> IO CUDA.Module
compileModuleNVVM dev name mdl = do
  -- Lower the module to bitcode and have libNVVM compile to PTX
  let CUDA.Compute m n   = CUDA.computeCapability dev
  bc                    <- LLVM.moduleBitcode mdl
  NVVM.Result msg r     <- NVVM.compileModule bc name [printf "-arch=sm_%d%d" m n]

  -- Check errors, do some debug prints
  case r of
    Left err    -> INTERNAL_ERROR(error) "compile" (printf "error (%s): %s" (show err) (B.unpack msg))
    Right ptx   -> do
      Debug.message Debug.dump_llvm =<< LLVM.moduleLLVMAssembly mdl
      Debug.when Debug.dump_ptx $ do
        Debug.message Debug.dump_ptx (B.unpack ptx)
        Debug.message Debug.verbose  (B.unpack msg)
      --
      CUDA.loadData ptx
#endif

compileModuleNVPTX :: LLVM.Module -> IO CUDA.Module
compileModuleNVPTX mdl =
  withNVVMTargetMachine $ \nvptx -> do

    -- Run the standard optimisation pass
    let pss = LLVM.defaultCuratedPassSetSpec { LLVM.optLevel = Just 3 }
    LLVM.withPassManager pss $ \pm -> do
      void $ LLVM.runPassManager pm mdl

      -- Dump the module to target assembly (PTX)
      ptx <- either error id `fmap` runErrorT (LLVM.moduleTargetAssembly nvptx mdl)
      Debug.message Debug.dump_llvm =<< LLVM.moduleLLVMAssembly mdl
      Debug.message Debug.dump_ptx ptx

      -- Load to a CUDA module
      CUDA.loadData (B.pack ptx)


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

  Debug.message Debug.dump_ptx (printf "%s\n     ... %s" msg1 msg2)
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

