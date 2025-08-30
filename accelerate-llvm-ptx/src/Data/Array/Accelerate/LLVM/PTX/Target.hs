{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Target
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Target (

  module Data.Array.Accelerate.LLVM.Target,
  module Data.Array.Accelerate.LLVM.PTX.Target,

) where

-- accelerate
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.Extra
import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.PTX.Array.Table                   ( MemoryTable )
import Data.Array.Accelerate.LLVM.PTX.Context                       ( Context, deviceProperties, deviceName )
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream.Reservoir      ( Reservoir )
import Data.Array.Accelerate.LLVM.PTX.Link.Cache                    ( KernelTable )

-- CUDA
import Foreign.CUDA.Analysis.Device                                 ( DeviceProperties )

-- standard library
import Data.ByteString.Short                                        ( ShortByteString )
import Data.Primitive.ByteArray
import Foreign.C.String
import Foreign.Ptr


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
  targetDataLayout = Nothing


-- | Extract the properties of the device the current PTX execution state is
-- executing on.
--
ptxDeviceProperties :: PTX -> DeviceProperties
ptxDeviceProperties = deviceProperties . ptxContext

-- | Extract the name of the device of the current execution context
--
ptxDeviceName :: PTX -> CString
ptxDeviceName = castPtr . byteArrayContents . deviceName . ptxContext


-- | String that describes the target host.
--
ptxTargetTriple :: HasCallStack => ShortByteString
ptxTargetTriple =
  case bitSize (undefined::Int) of
    32  -> "nvptx-nvidia-cuda"
    64  -> "nvptx64-nvidia-cuda"
    _   -> internalError "I don't know what architecture I am"
