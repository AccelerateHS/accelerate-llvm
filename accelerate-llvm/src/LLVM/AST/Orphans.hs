{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : LLVM.AST.Orphans
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Orphans ()
  where

import LLVM.AST.Name

import Data.Hashable

instance Hashable Name where
  hashWithSalt s (Name n)   = s `hashWithSalt` (1::Int) `hashWithSalt` n
  hashWithSalt s (UnName n) = s `hashWithSalt` (2::Int) `hashWithSalt` n

