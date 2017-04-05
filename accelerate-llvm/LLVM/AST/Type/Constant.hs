{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Constant
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Constant
  where

import LLVM.AST.Type.Name
import LLVM.AST.Type.Representation


-- | Although constant expressions and instructions have many similarities,
-- there are important differences - so they're represented using different
-- types in this AST. At the cost of making it harder to move an code back and
-- forth between being constant and not, this approach embeds more of the rules
-- of what IR is legal into the Haskell types.
--
-- <http://llvm.org/docs/LangRef.html#constants>
--
-- <http://llvm.org/docs/LangRef.html#constant-expressions>
--
data Constant a where
  ScalarConstant        :: ScalarType a
                        -> a
                        -> Constant a

  UndefConstant         :: Type a
                        -> Constant a

  GlobalReference       :: Type a
                        -> Name a
                        -> Constant a

