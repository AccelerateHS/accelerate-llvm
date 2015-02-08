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

import Data.Array.Accelerate.Type

import LLVM.General.AST.Type.Name


-- class ParameterAttribute a where
--   pattrs :: a -> [L.ParameterAttribute]
--
-- instance ParameterAttribute (Ptr a) where
--   pattrs _ = [L.NoAlias, L.Align 4]
--
-- instance ParameterAttribute a where
--   pattrs _ = []

-- | Parameters for functions
--
data Parameter a where
  Parameter :: ScalarType a -> Name a -> Parameter a

