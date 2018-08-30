{-# LANGUAGE CPP               #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Target
-- Copyright   : [2014..2018] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Target (

  module Data.Array.Accelerate.LLVM.Target,
  module Data.Array.Accelerate.LLVM.PTX.Target,

) where

-- llvm-hs
import LLVM.AST.AddrSpace
import LLVM.AST.DataLayout
import LLVM.Target                                                  hiding ( Target )
import qualified LLVM.Target                                        as LLVM
import qualified LLVM.Relocation                                    as R
import qualified LLVM.CodeModel                                     as CM
import qualified LLVM.CodeGenOpt                                    as CGO

-- accelerate
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.Extra
import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.PTX.Array.Table                   ( MemoryTable )
import Data.Array.Accelerate.LLVM.PTX.Context                       ( Context, deviceProperties )
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream.Reservoir      ( Reservoir )
import Data.Array.Accelerate.LLVM.PTX.Link.Cache                    ( KernelTable )

-- CUDA
import qualified Foreign.CUDA.Driver                                as CUDA

-- standard library
import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short                                        ( ShortByteString )
import Data.String
import System.IO.Unsafe
import Text.Printf
import qualified Data.Map                                           as Map
import qualified Data.Set                                           as Set


-- | The PTX execution target for NVIDIA GPUs.
--
-- The execution target carries state specific for the current execution
-- context. The data here --- device memory and execution streams --- are
-- implicitly tied to this CUDA execution context.
--
-- Don't store anything here that is independent of the context, for example
-- state related to [persistent] kernel caching should _not_ go here.
--
data PTX = PTX {
    ptxContext                  :: {-# UNPACK #-} !Context
  , ptxMemoryTable              :: {-# UNPACK #-} !MemoryTable
  , ptxKernelTable              :: {-# UNPACK #-} !KernelTable
  , ptxStreamReservoir          :: {-# UNPACK #-} !Reservoir
  }

instance Target PTX where
  targetTriple     = Just ptxTargetTriple
#if ACCELERATE_USE_NVVM
  targetDataLayout = Nothing              -- see note: [NVVM and target data layout]
#else
  targetDataLayout = Just ptxDataLayout
#endif


-- | Extract the properties of the device the current PTX execution state is
-- executing on.
--
ptxDeviceProperties :: PTX -> CUDA.DeviceProperties
ptxDeviceProperties = deviceProperties . ptxContext


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
ptxDataLayout :: DataLayout
ptxDataLayout = DataLayout
  { endianness          = LittleEndian
  , mangling            = Nothing
  , aggregateLayout     = AlignmentInfo 0 64
  , stackAlignment      = Nothing
  , pointerLayouts      = Map.fromList
      [ (AddrSpace 0, (wordSize, AlignmentInfo wordSize wordSize)) ]
  , typeLayouts         = Map.fromList $
      [ ((IntegerAlign, 1), AlignmentInfo 8 8) ] ++
      [ ((IntegerAlign, i), AlignmentInfo i i) | i <- [8,16,32,64]] ++
      [ ((VectorAlign,  v), AlignmentInfo v v) | v <- [16,32,64,128]] ++
      [ ((FloatAlign,   f), AlignmentInfo f f) | f <- [32,64] ]
  , nativeSizes         = Just $ Set.fromList [ 16,32,64 ]
  }
  where
    wordSize = bitSize (undefined :: Int)


-- | String that describes the target host.
--
ptxTargetTriple :: ShortByteString
ptxTargetTriple =
  case bitSize (undefined::Int) of
    32  -> "nvptx-nvidia-cuda"
    64  -> "nvptx64-nvidia-cuda"
    _   -> $internalError "ptxTargetTriple" "I don't know what architecture I am"


-- | Bracket creation and destruction of the NVVM TargetMachine.
--
withPTXTargetMachine
    :: CUDA.DeviceProperties
    -> (TargetMachine -> IO a)
    -> IO a
withPTXTargetMachine dev go =
  let CUDA.Compute m n = CUDA.computeCapability dev
      isa              = CPUFeature (ptxISAVersion m n)
      sm               = fromString (printf "sm_%d%d" m n)
  in
  withTargetOptions $ \options -> do
    withTargetMachine
        ptxTarget
        ptxTargetTriple
        sm
        (Map.singleton isa True)    -- CPU features
        options                     -- target options
        R.Default                   -- relocation model
        CM.Default                  -- code model
        CGO.Default                 -- optimisation level
        go

-- Compile using the earliest version of the PTX ISA supported by the given
-- compute device. We could also take the LUB of the latest version supported by
-- the installed version of both LLVM and CUDA.
--
-- Note that we require at least ptx40 for some libnvvm device functions.
--
-- See table NVPTX supported processors:
--
--   https://github.com/llvm-mirror/llvm/blob/master/lib/Target/NVPTX/NVPTX.td
--
-- PTX ISA verison history:
--
--   https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#release-notes
--
ptxISAVersion :: Int -> Int -> ByteString
ptxISAVersion 3 7 = "ptx41"
ptxISAVersion 5 2 = "ptx41"
ptxISAVersion 5 3 = "ptx42"
ptxISAVersion 6 _ = "ptx50"
ptxISAVersion 7 0 = "ptx60"
ptxISAVersion 7 2 = "ptx61"
ptxISAVersion _ _ = "ptx40"


-- | The NVPTX target for this host.
--
-- The top-level 'unsafePerformIO' is so that 'initializeAllTargets' is run once
-- per program execution (although that might not be necessary?)
--
{-# NOINLINE ptxTarget #-}
ptxTarget :: LLVM.Target
ptxTarget = unsafePerformIO $ do
  initializeAllTargets
  fst `fmap` lookupTarget Nothing ptxTargetTriple

