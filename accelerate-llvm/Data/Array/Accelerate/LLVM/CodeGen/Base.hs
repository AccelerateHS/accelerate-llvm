-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Base
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Base
  where


import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Global
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.LLVM.CodeGen.IR


-- References
-- ----------

local :: ScalarType a -> Name a -> IR a
local t x = ir t (LocalReference t x)

global :: ScalarType a -> Name a -> IR a
global t x = ir t (ConstantOperand (GlobalReference t x))

param :: ScalarType t -> Name t -> Parameter t
param = Parameter

