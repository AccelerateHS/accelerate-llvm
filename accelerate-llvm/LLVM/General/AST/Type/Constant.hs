{-# LANGUAGE GADTs #-}
-- |
-- Module      : LLVM.General.AST.Type.Constant
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.General.AST.Type.Constant
  where

import Data.Array.Accelerate.Type

import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Representation

data Constant a where
  IntegralConstant      :: (IsIntegral a, SingleValueType a) => a -> Constant a
  FloatingConstant      :: (IsFloating a, SingleValueType a) => a -> Constant a
  NonNumConstant        :: (IsNonNum a,   SingleValueType a) => Integer -> Constant a   -- arbitrary bit pattern stored as an integer

  GlobalReference       :: IsType a => Name a -> Constant a
  Undef                 :: IsType a => Constant a

{--
  Add                   :: NSW
                        -> NUW
                        -> Constant a
                        -> Constant a
                        -> Constant a

  -- 'fptoui' or 'fptosi'
  Round                 :: (Integral a, Floating b)
                        => Constant a
                        -> Constant b

  FromIntegral          :: (Floating a, Integral b)
                        => Constant a
                        -> Constant b
--}

