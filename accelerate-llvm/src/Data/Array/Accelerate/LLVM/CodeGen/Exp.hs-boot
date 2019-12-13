-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Exp-boot
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Exp
  where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

indexArray       :: (Shape sh, Elt e) => IRArray (Array sh e) -> IR sh  -> IROpenExp arch env aenv e
linearIndexArray :: (Shape sh, Elt e) => IRArray (Array sh e) -> IR Int -> IROpenExp arch env aenv e

