-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Exp-boot
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Exp
  where

import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

indexArray       :: IRArray (Array sh e) -> Operands sh  -> IROpenExp arch env aenv e
linearIndexArray :: IRArray (Array sh e) -> Operands Int -> IROpenExp arch env aenv e

