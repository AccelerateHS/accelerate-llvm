-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Target
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Target (

  module Data.Array.Accelerate.LLVM.Target,
  module Data.Array.Accelerate.LLVM.Native.Target

) where

-- llvm-hs
import LLVM.Target                                                  hiding ( Target )
import LLVM.AST.DataLayout                                          ( DataLayout )
import qualified LLVM.Relocation                                    as RelocationModel
import qualified LLVM.CodeModel                                     as CodeModel
import qualified LLVM.CodeGenOpt                                    as CodeOptimisation

-- accelerate
import Data.Array.Accelerate.LLVM.Native.Link.Cache                 ( LinkCache )
import Data.Array.Accelerate.LLVM.Native.Execute.Scheduler          ( Workers )
import Data.Array.Accelerate.LLVM.Target                            ( Target(..) )

-- standard library
import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short                                        ( ShortByteString )
import System.IO.Unsafe


-- | Native machine code JIT execution target
--
data Native = Native
  { linkCache     :: !LinkCache
  , workers       :: !Workers
  }

instance Target Native where
  targetTriple     = Just nativeTargetTriple
  targetDataLayout = Just nativeDataLayout


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
withNativeTargetMachine k = do
  initializeNativeTarget
  nativeCPUFeatures <- getHostCPUFeatures
  (nativeTarget, _) <- lookupTarget Nothing nativeTargetTriple
  withTargetOptions $ \targetOptions ->
    withTargetMachine
        nativeTarget
        nativeTargetTriple
        nativeCPUName
        nativeCPUFeatures
        targetOptions
        RelocationModel.PIC
        CodeModel.Default
        CodeOptimisation.Default
        k

