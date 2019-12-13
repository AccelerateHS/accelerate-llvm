{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Foreign
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Foreign
  where

import Data.Array.Accelerate.Array.Sugar                            as A

import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Execute.Async

import Data.Typeable


-- | Interface for backends to provide foreign function implementations for
-- array and scalar expressions.
--
class Foreign arch where
  foreignAcc :: (A.Foreign asm, A.Arrays a, A.Arrays b)
             => asm (a -> b)
             -> Maybe (ArrRepr a -> Par arch (FutureR arch (ArrRepr b)))
  foreignAcc _ = Nothing

  foreignExp :: (A.Foreign asm, Typeable x, Typeable y)
             => asm (x -> y)
             -> Maybe (IRFun1 arch () (x -> y))
  foreignExp _ = Nothing

