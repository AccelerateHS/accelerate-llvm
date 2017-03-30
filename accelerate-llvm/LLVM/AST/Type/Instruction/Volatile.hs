{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Instruction.Volatile
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Instruction.Volatile
  where

-- | Loads and stores may be marked as 'volatile'. The LLVM optimiser will not
-- change the number of volatile operations or their order with respect to other
-- volatile operations, but may change the order of volatile operations relative
-- to non-volatile operations.
--
-- Note that in LLVM IR, volatility and atomicity are orthogonal; 'volatile' has
-- no cross-thread synchronisation behaviour.
--
-- <http://llvm.org/docs/LangRef.html#volatile-memory-accesses>
--
data Volatility = Volatile | NonVolatile

