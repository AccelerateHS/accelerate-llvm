{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.General.AST.Type.Operand
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.General.AST.Type.Operand (

  Operand(..),

) where

import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Constant

import Data.Array.Accelerate.Type


-- | An 'Operand' is roughly anything that is an argument to an 'Instruction'
--
data Operand a where
  LocalReference        :: ScalarType a -> Name a -> Operand a
  ConstantOperand       :: Constant a -> Operand a

