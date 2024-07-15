{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Instruction.RMW
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Instruction.RMW
  where

import Data.Array.Accelerate.Error

import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Representation

import qualified Text.LLVM                                          as LLVM


-- | Operations for the 'AtomicRMW' instruction.
--
-- <http://llvm.org/docs/LangRef.html#atomicrmw-instruction>
--
data RMWOperation
    = Exchange
    | Add
    | Sub
    | And
    | Nand
    | Or
    | Xor
    | Min
    | Max


-- | Convert to llvm-pretty
--
instance Downcast (NumType a, RMWOperation) LLVM.AtomicRWOp where
  downcast (IntegralNumType t, rmw) = downcast (t,rmw)
  downcast (FloatingNumType t, rmw) = downcast (t,rmw)

instance Downcast (IntegralType a, RMWOperation) LLVM.AtomicRWOp where
  downcast (t, rmw) =
    case rmw of
      Exchange        -> LLVM.AtomicXchg
      Add             -> LLVM.AtomicAdd
      Sub             -> LLVM.AtomicSub
      And             -> LLVM.AtomicAnd
      Or              -> LLVM.AtomicOr
      Xor             -> LLVM.AtomicXor
      Nand            -> LLVM.AtomicNand
      Min | signed t  -> LLVM.AtomicMin
          | otherwise -> LLVM.AtomicUMin
      Max | signed t  -> LLVM.AtomicMax
          | otherwise -> LLVM.AtomicUMax

instance Downcast (FloatingType a, RMWOperation) LLVM.AtomicRWOp where
  downcast (_, rmw) =
    case rmw of
      Exchange        -> LLVM.AtomicXchg
      Add             -> LLVM.AtomicFAdd
      Sub             -> LLVM.AtomicFSub
      _               -> internalError "unsupported operand type to RMWOperation"

