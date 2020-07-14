{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Foreign
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Foreign
  where

import Data.Array.Accelerate.Sugar.Foreign                          as A

import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Execute.Async


-- | Interface for backends to provide foreign function implementations for
-- array and scalar expressions.
--
class Foreign arch where
  foreignAcc :: A.Foreign asm
             => asm (a -> b)
             -> Maybe (a -> Par arch (FutureR arch b))
  foreignAcc _ = Nothing

  foreignExp :: A.Foreign asm
             => asm (x -> y)
             -> Maybe (IRFun1 arch () (x -> y))
  foreignExp _ = Nothing

