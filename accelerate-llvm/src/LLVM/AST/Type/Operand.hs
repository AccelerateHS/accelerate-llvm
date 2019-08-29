{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Operand
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Operand (

  Operand(..),

) where

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Name
import LLVM.AST.Type.Representation

import qualified LLVM.AST.Operand                                   as LLVM


-- | An 'Operand' is roughly anything that is an argument to an 'Instruction'
--
data Operand a where
  LocalReference        :: Type a -> Name a -> Operand a
  ConstantOperand       :: Constant a -> Operand a


-- | Convert to llvm-hs
--
instance Downcast (Operand a) LLVM.Operand where
  downcast (LocalReference t n) = LLVM.LocalReference (downcast t) (downcast n)
  downcast (ConstantOperand c)  = LLVM.ConstantOperand (downcast c)

instance TypeOf Operand where
  typeOf (LocalReference t _) = t
  typeOf (ConstantOperand c)  = typeOf c

