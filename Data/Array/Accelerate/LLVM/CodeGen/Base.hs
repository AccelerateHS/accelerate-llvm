{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Base
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Base
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Elt, eltType )

import Data.Array.Accelerate.LLVM.CodeGen.Type

-- llvm-general
import LLVM.General.AST.Name


varNames :: Elt a => String -> a -> [Name]
varNames base t
  | n <- length (llvmOfTupleType (eltType t))
  = [ Name (base ++ show i) | i <- [n-1, n-2 .. 0] ]

