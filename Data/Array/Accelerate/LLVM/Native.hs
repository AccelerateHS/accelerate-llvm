-- |
-- Module      : Data.Array.Accelerate.LLVM.Native
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native
  where

-- llvm-general
import LLVM.General.Target                                      hiding ( Target )

-- accelerate
import Data.Array.Accelerate.LLVM.Target

-- standard library
import Control.Monad.Error
import System.IO.Unsafe


-- | Native machine code JIT execution target
--
nativeTarget :: Target
nativeTarget = unsafePerformIO $ do
  triple <- getProcessTargetTriple
  layout <- either error id `fmap`
              (runErrorT $ withDefaultTargetMachine getTargetMachineDataLayout)

  return  $ Target { targetTriple     = Just triple
                   , targetDataLayout = Just layout
                   }

