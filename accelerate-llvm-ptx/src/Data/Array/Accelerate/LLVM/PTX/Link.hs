{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Link
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Link (

  module Data.Array.Accelerate.LLVM.Link,
  ExecutableR(..), FunctionTable(..), Kernel(..), ObjectCode,
  linkFunctionQ,

) where

import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.Link
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.Compile
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Link.Cache
import Data.Array.Accelerate.LLVM.PTX.Link.Object
import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

import qualified Foreign.CUDA.Analysis                              as CUDA
import qualified Foreign.CUDA.Driver                                as CUDA

import Control.Monad.State
import Data.ByteString.Short.Char8                                  ( ShortByteString, unpack )
import Formatting
import Foreign.Ptr
import Data.Array.Accelerate.TH.Compat
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Unsafe                             as B
import Prelude                                                      as P hiding ( lookup )


instance Link PTX where
  data ExecutableR PTX = PTXR { ptxExecutable :: {-# UNPACK #-} !(Lifetime FunctionTable)
                              }
  linkForTarget = link


-- | Load the generated object code into the current CUDA context.
--
link :: ObjectR PTX -> LLVM PTX (ExecutableR PTX)
link (ObjectR uid cfg objFname) = do
  target <- gets llvmTarget
  cache  <- gets ptxKernelTable
  funs   <- liftIO $ dlsym uid cache $ do
    -- Load the SASS object code into the current CUDA context
    obj <- B.readFile objFname
    jit <- B.unsafeUseAsCString obj $ \p -> CUDA.loadDataFromPtrEx (castPtr p) []
    let mdl = CUDA.jitModule jit

    -- Extract the kernel functions
    nm  <- FunctionTable `fmap` mapM (uncurry (linkFunction mdl)) cfg
    oc  <- newLifetime mdl

    -- Finalise the module by unloading it from the CUDA context
    addFinalizer oc $ do
      Debug.traceM Debug.dump_ld ("ld: unload module: " % formatFunctionTable) nm
      withContext (ptxContext target) (CUDA.unload mdl)

    return (nm, oc)
  --
  return $! PTXR funs


-- | Extract the named function from the module and package into a Kernel
-- object, which includes meta-information on resource usage.
--
-- If we are in debug mode, print statistics on kernel resource usage, etc.
--
linkFunction
    :: CUDA.Module                      -- the compiled module
    -> ShortByteString                  -- __global__ entry function name
    -> LaunchConfig                     -- launch configuration for this global function
    -> IO Kernel
linkFunction mdl name configure =
  fst `fmap` linkFunctionQ mdl name configure

linkFunctionQ
    :: CUDA.Module
    -> ShortByteString
    -> LaunchConfig
    -> IO (Kernel, CodeQ (Int -> Int))
linkFunctionQ mdl name configure = do
  f     <- CUDA.getFun mdl name
  regs  <- CUDA.requires f CUDA.NumRegs
  ssmem <- CUDA.requires f CUDA.SharedSizeBytes
  cmem  <- CUDA.requires f CUDA.ConstSizeBytes
  lmem  <- CUDA.requires f CUDA.LocalSizeBytes
  maxt  <- CUDA.requires f CUDA.MaxKernelThreadsPerBlock

  let
      (occ, cta, grid, dsmem, gridQ) = configure maxt regs ssmem

      msg1 = bformat ("kernel function " % squoted string % " used " % int % " registers, " % int % " bytes smem, " % int % " bytes lmem, " % int % " bytes cmem")
                      (unpack name) regs (ssmem + dsmem) lmem cmem

      msg2 = bformat ("multiprocessor occupancy " % fixed 1 % "% : " % int % " threads over " % int % " warps in " % int % " blocks")
                      (CUDA.occupancy100 occ)
                      (CUDA.activeThreads occ)
                      (CUDA.activeWarps occ)
                      (CUDA.activeThreadBlocks occ)

  Debug.traceM Debug.dump_cc ("cc: " % builder % "\n               " % builder) msg1 msg2
  return (Kernel name f dsmem cta grid, gridQ)


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

