{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Instruction.Atomic
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Instruction.Atomic
  where

-- | Atomic instructions take ordering parameters that determine which other
-- atomic instructions on the same address they synchronise with.
--
-- <http://llvm.org/docs/Atomics.html>
--
-- <http://llvm.org/docs/LangRef.html#atomic-memory-ordering-constraints>
--
data MemoryOrdering
    = Unordered
    | Monotonic
    | Acquire
    | Release
    | AcquireRelease
    | SequentiallyConsistent

-- | If an atomic operation is marked as 'singlethread', it only participates in
-- modifications and SequentiallyConsistent total orderings with other
-- operations running in the same thread.
--
-- <http://llvm.org/docs/LangRef.html#singlethread>

data Synchronisation
    = SingleThread
    | CrossThread

-- | The atomicity of an instruction determines the constraints on the
-- visibility of the effects of that instruction.
--
type Atomicity = (Synchronisation, MemoryOrdering)

