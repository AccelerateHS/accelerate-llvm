{-# LANGUAGE CPP            #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Target
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Target
  where

-- llvm-general
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.DataLayout
import LLVM.General.Target                                      hiding ( Target )
import qualified LLVM.General.Target                            as LLVM
import qualified LLVM.General.Relocation                        as R
import qualified LLVM.General.CodeModel                         as CM
import qualified LLVM.General.CodeGenOpt                        as CGO

-- accelerate
import Data.Array.Accelerate.LLVM.Target
import Data.Array.Accelerate.LLVM.Util

import Data.Array.Accelerate.LLVM.NVVM.Array.Table              ( MemoryTable )
import Data.Array.Accelerate.LLVM.NVVM.Execute.Stream           ( Reservoir )

-- CUDA
import Foreign.CUDA.Driver                                      as CUDA
import Foreign.CUDA.Analysis                                    as CUDA

-- standard library
import Control.Monad.Error
import System.IO.Unsafe
import qualified Data.Map                                       as Map
import qualified Data.Set                                       as Set

#include "accelerate.h"


-- | The NVVM/PTX execution target for NVIDIA GPUs
--
data NVVM = NVVM {
    nvvmContext                 :: {-# UNPACK #-} !CUDA.Context
  , nvvmDeviceProperties        :: {-# UNPACK #-} !CUDA.DeviceProperties
  , nvvmMemoryTable             :: {-# UNPACK #-} !MemoryTable
  , nvvmStreamReservoir         :: {-# UNPACK #-} !Reservoir
  }

data Kernel = Kernel {
    kernelFun                   :: {-# UNPACK #-} !CUDA.Fun
  , kernelOccupancy             :: {-# UNPACK #-} !CUDA.Occupancy
  , kernelSharedMemBytes        :: {-# UNPACK #-} !Int
  , kernelThreadBlockSize       :: {-# UNPACK #-} !Int
  , kernelThreadBlocks          :: (Int -> Int)
  , kernelName                  :: String
  }

instance Target NVVM where
  data ExecutableR NVVM = NVVMR { nvvmKernel :: [Kernel]
                                , nvvmModule :: CUDA.Module
                                }
  targetTriple _     = Just nvvmTargetTriple
  targetDataLayout _ = Just nvvmDataLayout


-- | A description of the various data layout properties that may be used during
-- optimisation. For CUDA the following data layouts are supported:
--
-- 32-bit:
--   e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64
--
-- 64-bit:
--   e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64
--
-- Thus, only the size of the pointer layout changes depending on the host
-- architecture.
--
nvvmDataLayout :: DataLayout
nvvmDataLayout = DataLayout
  { endianness          = Just LittleEndian
  , stackAlignment      = Nothing
  , pointerLayouts      = Map.fromList
      [ (AddrSpace 0, (wordSize, AlignmentInfo wordSize (Just wordSize))) ]
  , typeLayouts         = Map.fromList $
      [ ((IntegerAlign, 1), AlignmentInfo 8 (Just 8)) ] ++
      [ ((IntegerAlign, i), AlignmentInfo i (Just i)) | i <- [8,16,32,64]] ++
      [ ((VectorAlign,  v), AlignmentInfo v (Just v)) | v <- [16,32,64,128]] ++
      [ ((FloatAlign,   f), AlignmentInfo f (Just f)) | f <- [32,64] ]
  , nativeSizes         = Just $ Set.fromList [ 16,32,64 ]
  }
  where
    wordSize = bitSize (undefined :: Int)


-- | String that describes the target host.
--
nvvmTargetTriple :: String
nvvmTargetTriple =
  case bitSize (undefined::Int) of
    32  -> "nvptx-nvidia-cuda"
    64  -> "nvptx64-nvidia-cuda"
    _   -> INTERNAL_ERROR(error) "nvvmTargetTriple" "I don't know what architecture I am"


-- | Bracket creation and destruction of the NVVM TargetMachine.
--
withNVVMTargetMachine :: (TargetMachine -> IO a) -> IO a
withNVVMTargetMachine f =
  withTargetOptions $ \options -> do
    withTargetMachine nvvmTarget nvvmTargetTriple "" Set.empty options R.Default CM.Default CGO.Default f


-- | The NVPTX target for this host.
--
-- The top-level 'unsafePerformIO' is so that 'initializeAllTargets' is run once
-- per program execution (although that might not be necessary?)
--
{-# NOINLINE nvvmTarget #-}
nvvmTarget :: LLVM.Target
nvvmTarget = unsafePerformIO $ do
  initializeAllTargets
  either error fst `fmap` runErrorT (lookupTarget Nothing nvvmTargetTriple)

