{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Target
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Target
  where

-- llvm-general
import LLVM.General.Target                                      hiding ( Target )
import LLVM.General.AST.DataLayout

-- accelerate
import Data.Array.Accelerate.LLVM.Target
import Data.Array.Accelerate.LLVM.Native.Compile

-- standard library
import Control.Monad.Error
import System.IO.Unsafe
import Foreign.Ptr


-- | Native machine code JIT execution target
--
nativeTarget :: Target
nativeTarget = Target nativeTargetTriple nativeDataLayout


{-# NOINLINE nativeTargetTriple #-}
nativeTargetTriple :: Maybe String
nativeTargetTriple = Just $ unsafePerformIO getProcessTargetTriple

{-# NOINLINE nativeDataLayout #-}
nativeDataLayout :: Maybe DataLayout
nativeDataLayout = unsafePerformIO $
  either error Just `fmap` (runErrorT $ withDefaultTargetMachine getTargetMachineDataLayout)


{--
data Native

instance Target Native where
  data ExecutableR Native = NativeR { executableR :: [FunPtr ()] }

  targetTriple _     = nativeTargetTriple
  targetDataLayout _ = nativeDataLayout

  compileForTarget m n =
    NativeR `fmap` compileForMCJIT m n
--}

