-- |
-- Module      : Data.Array.Accelerate.LLVM.Target
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Target
  where

-- llvm-general
import LLVM.General.AST.DataLayout                      ( DataLayout )


-- | Describes some target specific information needed for code generation
--
data Target = Target
  {
    targetTriple          :: Maybe String
  , targetDataLayout      :: Maybe DataLayout
  }


-- A dummy instance
--
defaultTarget :: Target
defaultTarget =  Target
  { targetTriple     = Nothing
  , targetDataLayout = Nothing
  }

