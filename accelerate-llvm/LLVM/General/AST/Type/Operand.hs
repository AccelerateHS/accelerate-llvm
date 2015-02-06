{-# LANGUAGE GADTs #-}
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

  MetadataNodeID(..),
  MetadataNode(..),
  Operand(..),

) where

import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Constant

import LLVM.General.AST.Operand                         ( MetadataNodeID(..) )

import Data.Array.Accelerate.Type


-- | <http://llvm.org/docs/LangRef.html#metadata>
--
-- Metadata does not have a type, and is not a value.
--
data MetadataNode a
  = MetadataNode [Maybe (Operand a)]            -- FIXME ??
  | MetadataNodeReference MetadataNodeID


-- | An 'Operand' is roughly anything that is an argument to an 'Instruction'
--
data Operand a where
  LocalReference        :: ScalarType a -> Name a -> Operand a
  ConstantOperand       :: Constant a -> Operand a

--  MetadataStringOperand :: String -> Operand a
--  MetadataNodeOperand   :: MetadataNode a -> Operand a

