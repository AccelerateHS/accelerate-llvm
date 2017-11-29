{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Operand
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Operand (

  Operand(..),

) where

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Name
import LLVM.AST.Type.Representation


-- | An 'Operand' is roughly anything that is an argument to an 'Instruction'
--
data Operand a where
  LocalReference        :: Type a -> Name a -> Operand a
  ConstantOperand       :: Constant a -> Operand a

