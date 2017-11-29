-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Target
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Target (

  module Data.Array.Accelerate.LLVM.Target,
  module Data.Array.Accelerate.LLVM.Native.Target

) where

-- llvm-general
import LLVM.Target                                                  hiding ( Target )
import LLVM.AST.DataLayout                                          ( DataLayout )

-- accelerate
import Data.Array.Accelerate.LLVM.Native.Link.Cache                 ( LinkCache )
import Data.Array.Accelerate.LLVM.Target                            ( Target(..) )
import Control.Parallel.Meta                                        ( Executable )

-- standard library
import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short                                        ( ShortByteString )
import System.IO.Unsafe


-- | Native machine code JIT execution target
--
data Native = Native
  { gangSize      :: {-# UNPACK #-} !Int
  , linkCache     :: {-# UNPACK #-} !LinkCache
  , fillS         :: {-# UNPACK #-} !Executable
  , fillP         :: {-# UNPACK #-} !Executable
  , segmentOffset :: !Bool
  }

instance Target Native where
  targetTriple     _ = Just nativeTargetTriple
  targetDataLayout _ = Just nativeDataLayout


-- | String that describes the native target
--
{-# NOINLINE nativeTargetTriple #-}
nativeTargetTriple :: ShortByteString
nativeTargetTriple = unsafePerformIO $
    -- A target triple suitable for loading code into the current process
    getProcessTargetTriple

-- | A description of the various data layout properties that may be used during
-- optimisation.
--
{-# NOINLINE nativeDataLayout #-}
nativeDataLayout :: DataLayout
nativeDataLayout
  = unsafePerformIO
  $ withNativeTargetMachine getTargetMachineDataLayout

-- | String that describes the host CPU
--
{-# NOINLINE nativeCPUName #-}
nativeCPUName :: ByteString
nativeCPUName = unsafePerformIO $ getHostCPUName


-- | Bracket the creation and destruction of a target machine for the native
-- backend running on this host.
--
withNativeTargetMachine
    :: (TargetMachine -> IO a)
    -> IO a
withNativeTargetMachine = withHostTargetMachine

