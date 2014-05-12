{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Target
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Target
  where

-- llvm-general
import LLVM.General.AST.DataLayout                              ( DataLayout )


-- | Describes some target specific information needed for code generation
--
class Target t where
  targetTriple          :: t {- dummy -} -> Maybe String
  targetDataLayout      :: t {- dummy -} -> Maybe DataLayout

