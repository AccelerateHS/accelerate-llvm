{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Instruction.Atomic
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Instruction.Atomic
  where

import LLVM.AST.Type.Downcast
import qualified Text.LLVM                                          as LLVM


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


-- | Convert to llvm-pretty
--
instance Downcast MemoryOrdering LLVM.AtomicOrdering where
  downcast Unordered              = LLVM.Unordered
  downcast Monotonic              = LLVM.Monotonic
  downcast Acquire                = LLVM.Acquire
  downcast Release                = LLVM.Release
  downcast AcquireRelease         = LLVM.AcqRel
  downcast SequentiallyConsistent = LLVM.SeqCst

-- llvm-pretty's typing is weak here
instance Downcast Synchronisation (Maybe String) where
  downcast SingleThread = Just "singlethread"
  downcast CrossThread  = Nothing  -- "system"

