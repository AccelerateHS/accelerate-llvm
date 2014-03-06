{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Target
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Target (

  module Data.Array.Accelerate.LLVM.Target,
  module Data.Array.Accelerate.LLVM.Native.Target

) where

-- llvm-general
import qualified LLVM.General.AST                               as AST
import LLVM.General.Target                                      hiding ( Target )

-- accelerate
import Data.Array.Accelerate.LLVM.Target

-- standard library
import Control.Monad.Error
import System.IO.Unsafe


-- | Native machine code JIT execution target
--
data Native = Native

instance Target Native where
  data ExecutableR Native = NativeR { executableR :: AST.Module }
--  data ExecutableR Native = NativeR { executableR :: [FunPtr ()] }

  {-# NOINLINE targetTriple #-}
  targetTriple _ = Just $ unsafePerformIO
#if MIN_VERSION_llvm_general(3,3,0)
    -- A target triple suitable for loading code into the current process
    getProcessTargetTriple
#else
    -- The default target triple LLVM has been configured to produce code for
    getDefaultTargetTriple
#endif

  {-# NOINLINE targetDataLayout #-}
  targetDataLayout _ = unsafePerformIO $
    either error Just `fmap` (runErrorT $ withDefaultTargetMachine getTargetMachineDataLayout)

