
module Data.Array.Accelerate.LLVM.CodeGen.CUDA
  where

-- llvm-general
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.DataLayout

-- standard library
import Data.Bits
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


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
dataLayout :: DataLayout
dataLayout = DataLayout
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
    wordSize = fromIntegral (bitSize (undefined :: Int))


-- String that describes the target host.
--
targetTriple :: String
targetTriple = "nvptx-nvidia-cl.1.0"

