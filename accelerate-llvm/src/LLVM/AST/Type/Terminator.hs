{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Terminator
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Terminator
  where

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Downcast

import qualified LLVM.AST.Instruction                               as LLVM


-- | <http://llvm.org/docs/LangRef.html#terminators>
--
-- TLM: well, I don't think the types of these terminators make any sense. When
--      we branch, we are not propagating a particular value, just moving the
--      program counter, and anything we have declared already is available for
--      later computations. Maybe, we can make some of this explicit in the
--      @phi@ node?
--
data Terminator a where
  -- <http://llvm.org/docs/LangRef.html#ret-instruction>
  --
  Ret           :: Terminator ()

  -- <http://llvm.org/docs/LangRef.html#ret-instruction>
  --
  RetVal        :: Operand a
                -> Terminator a

  -- <http://llvm.org/docs/LangRef.html#br-instruction>
  --
  Br            :: Label
                -> Terminator ()

  -- <http://llvm.org/docs/LangRef.html#br-instruction>
  --
  CondBr        :: Operand Bool
                -> Label
                -> Label
                -> Terminator ()

  -- <http://llvm.org/docs/LangRef.html#switch-instruction>
  --
  Switch        :: Operand a
                -> Label
                -> [(Constant a, Label)]
                -> Terminator ()


-- | Convert to llvm-hs
--
instance Downcast (Terminator a) LLVM.Terminator where
  downcast = \case
    Ret           -> LLVM.Ret Nothing md
    RetVal x      -> LLVM.Ret (Just (downcast x)) md
    Br l          -> LLVM.Br (downcast l) md
    CondBr p t f  -> LLVM.CondBr (downcast p) (downcast t) (downcast f) md
    Switch p d a  -> LLVM.Switch (downcast p) (downcast d) (downcast a) md
    where
      md :: LLVM.InstructionMetadata
      md = []

