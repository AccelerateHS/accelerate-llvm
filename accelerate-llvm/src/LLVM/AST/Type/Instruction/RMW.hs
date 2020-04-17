{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Instruction.RMW
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Instruction.RMW
  where

import Data.Array.Accelerate.Error

import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Representation

import qualified LLVM.AST.RMWOperation                              as LLVM


-- | Operations for the 'AtomicRMW' instruction.
--
-- <http://llvm.org/docs/LangRef.html#atomicrmw-instruction>
--
data RMWOperation
    = Exchange
    | Add
    | Sub
    | And
    | Nand
    | Or
    | Xor
    | Min
    | Max


-- | Convert to llvm-hs
--
instance Downcast (NumType a, RMWOperation) LLVM.RMWOperation where
  downcast (IntegralNumType t, rmw) = downcast (t,rmw)
  downcast (FloatingNumType t, rmw) = downcast (t,rmw)

instance Downcast (IntegralType a, RMWOperation) LLVM.RMWOperation where
  downcast (t, rmw) =
    case rmw of
      Exchange        -> LLVM.Xchg
      Add             -> LLVM.Add
      Sub             -> LLVM.Sub
      And             -> LLVM.And
      Or              -> LLVM.Or
      Xor             -> LLVM.Xor
      Nand            -> LLVM.Nand
      Min | signed t  -> LLVM.Min
          | otherwise -> LLVM.UMin
      Max | signed t  -> LLVM.Max
          | otherwise -> LLVM.UMax

instance Downcast (FloatingType a, RMWOperation) LLVM.RMWOperation where
  downcast (_, rmw) =
    case rmw of
      Exchange        -> LLVM.Xchg
#if MIN_VERSION_llvm_hs_pure(10,0,0)
      Add             -> LLVM.FAdd
      Sub             -> LLVM.FSub
#endif
      _               -> $internalError "downcast" "unsupported operand type to RMWOperation"

