{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Link
-- Copyright   : [2017..2018] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Link (

  module Data.Array.Accelerate.LLVM.Link,
  ExecutableR(..), FunctionTable(..), Kernel(..), ObjectCode,
  withExecutable,
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

-- cuda
import qualified Foreign.CUDA.Analysis                              as CUDA
import qualified Foreign.CUDA.Driver                                as CUDA

-- standard library
import Control.Monad.State
import Data.ByteString.Short.Char8                                  ( ShortByteString, unpack )
import Foreign.Ptr
import Language.Haskell.TH
import Text.Printf                                                  ( printf )
import qualified Data.ByteString.Unsafe                             as B
import Prelude                                                      as P hiding ( lookup )


instance Link PTX where
  data ExecutableR PTX = PTXR { ptxExecutable :: {-# UNPACK #-} !(Lifetime FunctionTable)
                              }
  linkForTarget = link


-- | Load the generated object code into the current CUDA context.
--
link :: ObjectR PTX -> LLVM PTX (ExecutableR PTX)
link (ObjectR uid cfg obj) = do
  target <- gets llvmTarget
  cache  <- gets ptxKernelTable
  funs   <- liftIO $ dlsym uid cache $ do
    -- Load the SASS object code into the current CUDA context
    jit <- B.unsafeUseAsCString obj $ \p -> CUDA.loadDataFromPtrEx (castPtr p) []
    let mdl = CUDA.jitModule jit

    -- Extract the kernel functions
    nm  <- FunctionTable `fmap` mapM (uncurry (linkFunction mdl)) cfg
    oc  <- newLifetime mdl

    -- Finalise the module by unloading it from the CUDA context
    addFinalizer oc $ do
      Debug.traceIO Debug.dump_gc ("gc: unload module: " ++ show nm)
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
    -> IO (Kernel, Q (TExp (Int -> Int)))
linkFunctionQ mdl name configure = do
  f     <- CUDA.getFun mdl name
  regs  <- CUDA.requires f CUDA.NumRegs
  ssmem <- CUDA.requires f CUDA.SharedSizeBytes
  cmem  <- CUDA.requires f CUDA.ConstSizeBytes
  lmem  <- CUDA.requires f CUDA.LocalSizeBytes
  maxt  <- CUDA.requires f CUDA.MaxKernelThreadsPerBlock

  let
      (occ, cta, grid, dsmem, gridQ) = configure maxt regs ssmem

      msg1, msg2 :: String
      msg1 = printf "kernel function '%s' used %d registers, %d bytes smem, %d bytes lmem, %d bytes cmem"
                      (unpack name) regs (ssmem + dsmem) lmem cmem

      msg2 = printf "multiprocessor occupancy %.1f %% : %d threads over %d warps in %d blocks"
                      (CUDA.occupancy100 occ)
                      (CUDA.activeThreads occ)
                      (CUDA.activeWarps occ)
                      (CUDA.activeThreadBlocks occ)

  Debug.traceIO Debug.dump_cc (printf "cc: %s\n  ... %s" msg1 msg2)
  return (Kernel name f dsmem cta grid, gridQ)


-- | Execute some operation with the supplied executable functions
--
withExecutable :: MonadIO m => ExecutableR PTX -> (FunctionTable -> m b) -> m b
withExecutable PTXR{..} f = do
  r <- f (unsafeGetValue ptxExecutable)
  liftIO $ touchLifetime ptxExecutable
  return r


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

