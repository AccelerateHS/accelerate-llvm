{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeOperators        #-}
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
import LLVM.General.AST.Type.Operand


-- | Parameters for functions
--
data Parameter a where
  ScalarParameter       :: ScalarType a -> Name a -> Parameter a
  PtrParameter          :: ScalarType a -> Name a -> Parameter (Ptr a)

-- | Attributes for the function call instruction
--
data FunctionAttribute
  = NoReturn
  | NoUnwind
  | ReadOnly
  | ReadNone
  | AlwaysInline

-- | A global function definition
--
-- Note that because we just use the reified dictionary structure of Accelerate
-- types, our functions are limited to operating over scalar types only; no
-- pointers to functions and nothing that returns void.
--
data GlobalFunction args t where
  Body :: ScalarType r -> Label                              -> GlobalFunction '[]         r
  Lam  :: ScalarType a -> Operand a -> GlobalFunction args t -> GlobalFunction (a ': args) t

data HList (l :: [*]) where
  HNil  ::                 HList '[]
  HCons :: e -> HList l -> HList (e ': l)

