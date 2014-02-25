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
import LLVM.General.AST
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Constant
import LLVM.General.AST.DataLayout

-- accelerate
import Data.Array.Accelerate.LLVM.Target
import Data.Array.Accelerate.LLVM.Util

-- standard library
import Data.Word
import Data.ByteString                          ( ByteString )
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


-- | The NVVM/PTX execution target for NVIDIA GPUs
--
data NVVM

instance Target NVVM where
  data ExecutableR NVVM = NVVMR { executableR :: ByteString }
  targetTriple _     = Just nvvmTargetTriple
  targetDataLayout _ = Just nvvmDataLayout

  compileForTarget _ = error "todo: compileForTarget NVVM"


-- A description of the various data layout properties that may be used during
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


-- String that describes the target host.
--
nvvmTargetTriple :: String
nvvmTargetTriple = "nvptx-nvidia-cl.1.0"


-- Kernel annotation
--
annotateAsKernel :: Name -> Word -> Definition
annotateAsKernel kernel metaID =
  MetadataNodeDefinition (MetadataNodeID metaID)
    [ Just $ ConstantOperand (GlobalReference kernel)
    , Just $ MetadataStringOperand "kernel"
    , Just $ ConstantOperand (Int 32 1)
    ]

