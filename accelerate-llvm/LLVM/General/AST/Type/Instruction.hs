{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs      #-}
-- |
-- Module      : LLVM.General.AST.Type.Instruction
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.General.AST.Type.Instruction
  where

import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand

import Data.Array.Accelerate.Type


-- | <http://llvm.org/docs/LangRef.html#metadata-nodes-and-metadata-strings>
--
-- Metadata can be attached to an instruction.
--
type InstructionMetadata = forall a. [(String, MetadataNode a)]         -- FIXME ??


-- | <http://llvm.org/docs/LangRef.html#terminators>
--
-- TLM: well, I don't think the types of these terminators make any sense. When
--      we branch, we are not propagating a particular value, just moving the
--      program counter, and anything we have declared already is available for
--      later computations. Maybe, we can make some of this explicit in the
--      @phi@ node?
--
data Terminator a where

  -- TLM: Combine Ret/RetVal somehow? In llvm-general a maybe type is used, but
  -- if there is no return value the type should be unit, and the Maybe doesn't
  -- reflect this information.
  --
  Ret           :: Terminator ()

  RetVal        :: Operand a
                -> Terminator a

  CondBr        :: Operand Bool
                -> Label
                -> Label
                -> Terminator ()

  Br            :: Label
                -> Terminator ()



data Instruction a where
  Add           :: NumType a -> Operand a -> Operand a -> Instruction a

{--
  Sub           :: (SingleValueType a, IsNum a) => Operand a -> Operand a -> Instruction a
  Mul           :: (SingleValueType a, IsNum a) => Operand a -> Operand a -> Instruction a

  Div           :: (SingleValueType a, IsNum a) => Operand a -> Operand a -> Instruction a      -- hmm

  Rem           :: (SingleValueType a, IsIntegral a)
                => Operand a
                -> Operand a
                -> Instruction a

  Round         :: (Floating a, Integral b)
                => Operand a
                -> Instruction b

--  BitCast       :: BitSizeEq a b ~ True
--                => Operand a
--                -> Instruction b

  Phi           :: [(Operand a, Label)]
                -> Instruction a
--}


instructionType :: Instruction a -> ScalarType a
instructionType ins =
  case ins of
    Add t _ _   -> NumScalarType t


-- data Named (i :: * -> *) where
--   (:=) :: Name a -> i a -> Named i
--   Do   :: i ()          -> Named i

