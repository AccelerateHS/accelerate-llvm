{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Instruction.RMW
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Instruction.RMW
  where

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

