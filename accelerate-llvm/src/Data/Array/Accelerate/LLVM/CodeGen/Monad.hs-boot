-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Monad-boot
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Monad ( CodeGen )
  where

import Control.Monad.Reader
import Control.Monad.State
import Data.Array.Accelerate.LLVM.State

data CodeGenState
data CodeGenContext
newtype CodeGen target a = CodeGen
  { runCodeGen :: ReaderT CodeGenContext (StateT CodeGenState (LLVM target)) a }

