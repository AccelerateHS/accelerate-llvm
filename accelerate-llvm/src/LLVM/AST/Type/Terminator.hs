{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Terminator
-- Copyright   : [2015..2020] The Accelerate Team
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

import qualified Text.LLVM                                          as LLVM

import Data.Bifunctor                                               ( bimap )


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
instance Downcast (Terminator a) LLVM.Instr where
  downcast = \case
    Ret           -> LLVM.RetVoid
    RetVal x      -> LLVM.Ret (downcast x)
    Br l          -> LLVM.Jump (LLVM.Named (labelToPrettyI l))
    CondBr p t f  -> LLVM.Br (downcast p) (LLVM.Named (labelToPrettyI t)) (LLVM.Named (labelToPrettyI f))
    Switch p d a  -> LLVM.Switch (downcast p)
                                 (labelToPrettyBL d)
                                 (map (bimap fromConstant labelToPrettyBL) a)
      where
        fromConstant :: Constant a -> Integer
        fromConstant cnst = case downcast @_ @(LLVM.Typed LLVM.Value) cnst of
          LLVM.Typed _ (LLVM.ValInteger n) -> n
          _ -> error "TODO: llvm-pretty supports only integral cases for Switch"
