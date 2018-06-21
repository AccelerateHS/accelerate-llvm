-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Monad-boot
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Monad ( CodeGen )
  where

import Control.Monad.State
import Data.Array.Accelerate.LLVM.State

data CodeGenState
newtype CodeGen target a = CodeGen { runCodeGen :: StateT CodeGenState (LLVM target) a }

