{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Terminator
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Terminator
  where

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand


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

