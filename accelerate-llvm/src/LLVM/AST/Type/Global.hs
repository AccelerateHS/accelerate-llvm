{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Global
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Global
  where

import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import qualified LLVM.AST.Attribute                                 as LLVM
import qualified LLVM.AST.Global                                    as LLVM
import qualified LLVM.AST.Name                                      as LLVM
import qualified LLVM.AST.Type                                      as LLVM


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
  | NoDuplicate
  | Convergent

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


-- | Convert to llvm-hs
--
instance Downcast (GlobalFunction args t) LLVM.Global where
  downcast f = LLVM.functionDefaults { LLVM.name       = nm
                                     , LLVM.returnType = res
                                     , LLVM.parameters = (params, False)
                                     }
    where
      trav :: GlobalFunction args t -> ([LLVM.Type], LLVM.Type, LLVM.Name)
      trav (Body t n)  = ([], downcast t, downcast n)
      trav (Lam a _ l) = let (as, r, n) = trav l
                         in  (downcast a : as, r, n)
      --
      (args, res, nm)  = trav f
      params           = [ LLVM.Parameter t (LLVM.UnName i) [] | t <- args | i <- [0..] ]


instance Downcast GroupID LLVM.GroupID where
  downcast (GroupID n) = LLVM.GroupID n

instance Downcast FunctionAttribute LLVM.FunctionAttribute where
  downcast NoReturn     = LLVM.NoReturn
  downcast NoUnwind     = LLVM.NoUnwind
  downcast ReadOnly     = LLVM.ReadOnly
  downcast ReadNone     = LLVM.ReadNone
  downcast AlwaysInline = LLVM.AlwaysInline
  downcast NoDuplicate  = LLVM.NoDuplicate
  downcast Convergent   = LLVM.Convergent

instance Downcast (Parameter a) LLVM.Parameter where
  downcast (Parameter t n) = LLVM.Parameter (downcast t) (downcast n) attrs
    where
      attrs | PtrPrimType{} <- t = [LLVM.NoAlias, LLVM.NoCapture] -- XXX: alignment
            | otherwise          = []

