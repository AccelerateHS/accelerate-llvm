{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Link
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Link (

  module Data.Array.Accelerate.LLVM.Link,
  ExecutableR(..), Kernel(..), ObjectCode,

) where

import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.Link
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.Compile
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

-- cuda
import qualified Foreign.CUDA.Analysis                              as CUDA
import qualified Foreign.CUDA.Driver                                as CUDA

-- standard library
import Control.Monad.State
import Data.ByteString.Internal                                     ( w2c )
import Data.ByteString.Short                                        ( ShortByteString, unpack )
import Data.List                                                    ( intercalate )
import Foreign.Ptr
import Text.Printf                                                  ( printf )
import qualified Data.ByteString.Unsafe                             as B
import Prelude                                                      as P


instance Link PTX where
  data ExecutableR PTX = PTXR { ptxKernel :: ![Kernel]
                              , ptxModule :: {-# UNPACK #-} !ObjectCode
                              }
  linkForTarget = link


data Kernel = Kernel
  { kernelName                  :: {-# UNPACK #-} !ShortByteString
  , kernelFun                   :: {-# UNPACK #-} !CUDA.Fun
  , kernelOccupancy             :: {-# UNPACK #-} !CUDA.Occupancy
  , kernelSharedMemBytes        :: {-# UNPACK #-} !Int
  , kernelThreadBlockSize       :: {-# UNPACK #-} !Int
  , kernelThreadBlocks          :: (Int -> Int)
  }

type ObjectCode = Lifetime CUDA.Module


-- | Load the generated object code into the current CUDA context.
--
link :: ObjectR PTX -> LLVM PTX (ExecutableR PTX)
link (ObjectR nm obj) = do
  target <- gets llvmTarget
  liftIO $ do
    -- Load the SASS object code into the current CUDA context
    jit     <- B.unsafeUseAsCString obj $ \p -> CUDA.loadDataFromPtrEx (castPtr p) []
    let mdl  = CUDA.jitModule jit

    -- Extract the kernel functions
    kernels <- mapM (uncurry (linkFunction mdl)) nm
    mdl'    <- newLifetime mdl

    -- Finalise the module by unloading it from the CUDA context
    addFinalizer mdl' $ do
      Debug.traceIO Debug.dump_gc
        $ printf "gc: unload module: %s"
        $ intercalate ","
        $ P.map (P.map w2c . unpack . kernelName) kernels
      withContext (ptxContext target) (CUDA.unload mdl)

    return $! PTXR kernels mdl'


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
linkFunction mdl name configure = do
  f     <- CUDA.getFun mdl (map w2c (unpack name))
  regs  <- CUDA.requires f CUDA.NumRegs
  ssmem <- CUDA.requires f CUDA.SharedSizeBytes
  cmem  <- CUDA.requires f CUDA.ConstSizeBytes
  lmem  <- CUDA.requires f CUDA.LocalSizeBytes
  maxt  <- CUDA.requires f CUDA.MaxKernelThreadsPerBlock

  let
      (occ, cta, grid, dsmem) = configure maxt regs ssmem

      msg1, msg2 :: String
      msg1 = printf "kernel function '%s' used %d registers, %d bytes smem, %d bytes lmem, %d bytes cmem"
                      (show name) regs (ssmem + dsmem) lmem cmem

      msg2 = printf "multiprocessor occupancy %.1f %% : %d threads over %d warps in %d blocks"
                      (CUDA.occupancy100 occ)
                      (CUDA.activeThreads occ)
                      (CUDA.activeWarps occ)
                      (CUDA.activeThreadBlocks occ)

  Debug.traceIO Debug.dump_cc (printf "cc: %s\n  ... %s" msg1 msg2)
  return $ Kernel name f occ dsmem cta grid


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

