{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile (

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

import qualified  Data.Array.Accelerate.LLVM.Debug              as Debug

-- cuda
import qualified Foreign.CUDA.Analysis                          as CUDA
import qualified Foreign.CUDA.Driver                            as CUDA
#ifdef ACCELERATE_USE_LIBNVVM
import qualified Foreign.LibNVVM                                as NVVM
#endif

-- standard library
import Numeric
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString                                          ( ByteString )
import Data.List
import System.IO.Unsafe


instance Compile NVVM where
  compileForTarget = compileForNVPTX

-- | Compile a given module for the NVPTX backend. If we linked with libNVVM
-- then use that to do the final module creation, otherwise just use llvm. The
-- advantage with libNVVM is that it performs additional (sekrit) optimisations.
--
compileForNVPTX
    :: DelayedOpenAcc aenv a
    -> Gamma aenv
    -> LLVM NVVM (ExecutableR NVVM)
compileForNVPTX acc aenv = do
  nvvm   <- gets llvmTarget
  ctx    <- asks llvmContext
  let Module ast = llvmOfAcc nvvm acc aenv
      pss        = LLVM.defaultCuratedPassSetSpec { LLVM.optLevel = Just 3 }
      dev        = nvvmDeviceProperties nvvm

  -- We use 'unsafePerformIO' here to leverage Haskell's non-strict semantics,
  -- so that we only block on module generation once the function is truly
  -- needed during the execution phase.
  --
  let exe = unsafePerformIO $ do
              ptx  <- either error return =<< runErrorT (
                        LLVM.withModuleFromAST ctx ast $ \mdl ->
                        LLVM.withPassManager pss       $ \pm  -> do
                          void $ LLVM.runPassManager pm mdl
                          compileModule (moduleName ast) =<< LLVM.moduleBitcode mdl)
              --
              funs <- sequence [ linkFunction acc dev ptx f | f <- globalFunctions (moduleDefinitions ast) ]
              return (NVVMR funs ptx)

  return exe


-- | Compile the named bitcode to produce a CUDA module. This might also include
-- optimisations with libNVVM.
--
compileModule :: String -> ByteString -> IO CUDA.Module
compileModule _name bc =
#ifdef ACCELERATE_USE_LIBNVVM
  NVVM.Result _ bc      <- NVVM.compileModule _name bc []
#endif
  CUDA.loadData bc

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

      msg1 = "entry function '" ++ name ++ "' used "
              ++ shows regs " registers, "  ++ shows smem " bytes smem, "
              ++ shows lmem " bytes lmem, " ++ shows cmem " bytes cmem"
      msg2 = "multiprocessor occupancy " ++ showFFloat (Just 1) (CUDA.occupancy100 occ) "% : "
              ++ shows (CUDA.activeThreads occ)      " threads over "
              ++ shows (CUDA.activeWarps occ)        " warps in "
              ++ shows (CUDA.activeThreadBlocks occ) " blocks"

  -- make sure kernel/stats are printed together. Use 'intercalate' rather than
  -- 'unlines' to avoid a trailing newline.
  --
  message (intercalate "\n     ... " [msg1, msg2])
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


-- Debug
-- -----

{-# INLINE message #-}
message :: MonadIO m => String -> m ()
message msg = trace msg $ return ()

{-# INLINE trace #-}
trace :: MonadIO m => String -> m a -> m a
trace msg next = Debug.message Debug.dump_llvm ("ll: " ++ msg) >> next

