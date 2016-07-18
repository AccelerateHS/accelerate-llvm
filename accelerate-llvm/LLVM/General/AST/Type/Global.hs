{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_HADDOCK hide #-}
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

import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Representation


-- | Parameters for functions
--
data Parameter a where
  Parameter :: PrimType a -> Name a -> Parameter a

-- | Attributes for the function call instruction
--
data FunctionAttribute
  = NoReturn
  | NoUnwind
  | ReadOnly
  | ReadNone
  | AlwaysInline

-- | Attribute groups are groups of attributes that are referenced by
-- objects within the IR. To use an attribute group, an object must
-- reference its GroupID.
--
data GroupID = GroupID !Word

-- | A global function definition.
--
data GlobalFunction args t where
  Body :: Type r     -> Label                              -> GlobalFunction '[]         r
  Lam  :: PrimType a -> Operand a -> GlobalFunction args t -> GlobalFunction (a ': args) t

data HList (l :: [*]) where
  HNil  ::                 HList '[]
  HCons :: e -> HList l -> HList (e ': l)

