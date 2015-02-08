{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- |
-- Module      : LLVM.General.AST.Type.Global
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.General.AST.Type.Global
  where

import Foreign.Ptr

import Data.Array.Accelerate.Type
import LLVM.General.AST.Type.Name


-- | Parameters for functions
--
data Parameter a where
  ScalarParameter       :: ScalarType a -> Name a -> Parameter a
  PtrParameter          :: ScalarType a -> Name a -> Parameter (Ptr a)

