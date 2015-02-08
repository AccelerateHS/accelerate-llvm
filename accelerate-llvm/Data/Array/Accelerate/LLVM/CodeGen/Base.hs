{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Base
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Base
  where


import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Global
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.Downcast
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR

import qualified LLVM.General.AST.Global                                as LLVM

import qualified Data.IntMap                                            as IM


-- References
-- ----------

local :: ScalarType a -> Name a -> IR a
local t x = ir t (LocalReference t x)

global :: ScalarType a -> Name a -> IR a
global t x = ir t (ConstantOperand (GlobalReference t x))


-- Names
-- -----

travTupleType :: forall t a. Elt t => t {- dummy -} -> (forall t'. ScalarType t' -> Int -> a) -> [a]
travTupleType _ f = snd $ go (eltType (undefined::t)) 0
  where
    go :: TupleType s -> Int -> (Int, [a])
    go UnitTuple         i = (i,   [])
    go (SingleTuple t)   i = (i+1, [f t i])
    go (PairTuple t2 t1) i = let (i1, r1) = go t1 i
                                 (i2, r2) = go t2 i1
                             in
                             (i2, r2 ++ r1)


-- names :: Elt a => a -> Label -> [Label]
-- names t (Label n) = travT t (\_ i -> Label (n ++ show i))
--
-- arrayData :: forall sh e. Elt e => Array sh e -> Label -> [Label]
-- arrayData _ (Label n) = names (undefined::e) (Label (n++".ad"))


-- Functions
-- ---------

scalarParameter :: ScalarType t -> Name t -> LLVM.Parameter
scalarParameter t x = downcast (ScalarParameter t x)

ptrParameter :: ScalarType t -> Name t -> LLVM.Parameter
ptrParameter t x = downcast (PtrParameter t x)


-- | Unpack the array environment into a set of input parameters to a function.
-- The environment here refers only to the actual free array variables that are
-- accessed by the function.
--
envParam :: forall aenv. Aval aenv -> [LLVM.Parameter]
envParam aenv = concatMap (\(n, Idx' v) -> toParam v n) (IM.elems aenv)
  where
    toParam :: forall sh e. (Shape sh, Elt e) => Idx aenv (Array sh e) -> Label -> [LLVM.Parameter]
    toParam _ name = arrayParam (undefined::Array sh e) name


-- | Generate function parameters for an Array with given base name.
--
arrayParam :: forall sh e. (Shape sh, Elt e) => Array sh e -> Label -> [LLVM.Parameter]
arrayParam _ (Label name) = ad ++ sh
  where
    ad = travTupleType (undefined :: e)  (\t i -> ptrParameter    t (Name (name ++ ".ad" ++ show i)))
    sh = travTupleType (undefined :: sh) (\t i -> scalarParameter t (Name (name ++ ".sh" ++ show i)))

