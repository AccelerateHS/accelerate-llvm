{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Foreign
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Foreign
  where

import Data.Array.Accelerate.Array.Sugar                            as A

import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.State

import Data.Typeable


-- | Interface for backends to provide foreign function implementations for
-- array and scalar expressions.
--
class Foreign arch where
  foreignAcc :: (A.Foreign asm, Typeable a, Typeable b)
             => arch {- dummy -}
             -> asm (a -> b)
             -> Maybe (StreamR arch -> a -> LLVM arch b)
  foreignAcc _ _ = Nothing

  foreignExp :: (A.Foreign asm, Typeable x, Typeable y)
             => arch {- dummy -}
             -> asm (x -> y)
             -> Maybe (IRFun1 arch () (x -> y))
  foreignExp _ _ = Nothing

