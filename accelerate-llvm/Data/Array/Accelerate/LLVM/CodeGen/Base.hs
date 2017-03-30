{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Base
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Base (

  -- References
  Name(..),
  local, global,

  -- Arrays
  irArray,
  mutableArray,

  -- Functions & parameters
  call,
  scalarParameter, ptrParameter,
  envParam,
  arrayParam,

) where

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Constant
import LLVM.AST.Type.Global
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.Downcast
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import qualified LLVM.AST.Global                                    as LLVM

import qualified Data.IntMap                                        as IM


-- References
-- ----------

local :: ScalarType a -> Name a -> IR a
local t x = ir t (LocalReference (PrimType (ScalarPrimType t)) x)

global :: ScalarType a -> Name a -> IR a
global t x = ir t (ConstantOperand (GlobalReference (PrimType (ScalarPrimType t)) x))


-- Generating names for things
-- ---------------------------

-- | Names of array data components
--
arrayName :: Name (Array sh e) -> Int -> Name e'        -- for the i-th component of the ArrayData
arrayName (Name n)   i = Name (n ++ ".ad" ++ show i)
arrayName (UnName n) i = arrayName (Name (show n)) i

-- | Names of shape components
--
shapeName :: Name (Array sh e) -> Int -> Name sh'       -- for the i-th component of the shape structure
shapeName (Name n)   i = Name (n ++ ".sh" ++ show i)
shapeName (UnName n) i = shapeName (Name (show n)) i

-- | Names of array data elements
--
{-# INLINEABLE irArray #-}
irArray
    :: forall sh e. (Shape sh, Elt e)
    => Name (Array sh e)
    -> IRArray (Array sh e)
irArray n
  = IRArray (travTypeToIR (undefined::sh) (\t i -> LocalReference (PrimType (ScalarPrimType t)) (shapeName n i)))
            (travTypeToIR (undefined::e)  (\t i -> LocalReference (PrimType (ScalarPrimType t)) (arrayName n i)))
            defaultAddrSpace
            NonVolatile


-- | Generate typed local names for array data components as well as function
-- parameters to bind those names
--
{-# INLINEABLE mutableArray #-}
mutableArray
    :: forall sh e. (Shape sh, Elt e)
    => Name (Array sh e)
    -> (IRArray (Array sh e), [LLVM.Parameter])
mutableArray name =
  ( irArray name
  , arrayParam name )


{-# INLINEABLE travTypeToList #-}
travTypeToList
    :: forall t a. Elt t
    => t {- dummy -}
    -> (forall s. ScalarType s -> Int -> a)
    -> [a]
travTypeToList t f = snd $ go (eltType t) 0
  where
    -- DANGER: [1] must traverse in the same order as [2]
    go :: TupleType s -> Int -> (Int, [a])
    go UnitTuple         i = (i,   [])
    go (SingleTuple t')  i = (i+1, [f t' i])
    go (PairTuple t2 t1) i = let (i1, r1) = go t1 i
                                 (i2, r2) = go t2 i1
                             in
                             (i2, r2 ++ r1)

travTypeToIR
    :: Elt t
    => t {- dummy -}
    -> (forall s. ScalarType s -> Int -> Operand s)
    -> IR t
travTypeToIR t f = IR . snd $ go (eltType t) 0
  where
    -- DANGER: [2] must traverse in the same order as [1]
    go :: TupleType s -> Int -> (Int, Operands s)
    go UnitTuple         i = (i,   OP_Unit)
    go (SingleTuple t')  i = (i+1, ir' t' $ f t' i)
    go (PairTuple t2 t1) i = let (i1, r1) = go t1 i
                                 (i2, r2) = go t2 i1
                             in
                             (i2, OP_Pair r2 r1)

-- travTypeToIRPtr
--     :: forall t. Elt t
--     => AddrSpace
--     -> t {- dummy -}
--     -> (forall s. ScalarType s -> Int -> Operand (Ptr s))
--     -> IR (Ptr t)
-- travTypeToIRPtr as t f = IR . snd $ go (eltType t) 0
--   where
--     -- DANGER: [2] must traverse in the same order as [1]
--     -- go :: TupleType s -> Int -> (Int, Operands (Ptr s))
--     go :: TupleType (EltRepr s) -> Int -> (Int, Operands (EltRepr (Ptr s)))   -- TLM: ugh ):
--     go UnitTuple         i = (i,   OP_Unit)
--     go (SingleTuple t')  i = (i+1, ir' (PtrPrimType t' as) $ f t' i)
--     go (PairTuple t2 t1) i = let (i1, r1) = go t1 i
--                                  (i2, r2) = go t2 i1
--                              in
--                              (i2, OP_Pair r2 r1)


-- Function parameters
-- -------------------

-- | Call a global function. The function declaration is inserted into the
-- symbol table.
--
call :: GlobalFunction args t -> [FunctionAttribute] -> CodeGen (IR t)
call f attrs = do
  let decl      = (downcast f) { LLVM.functionAttributes = downcast attrs' }
      attrs'    = map Right attrs
  --
  declare decl
  instr (Call f attrs')


scalarParameter :: ScalarType t -> Name t -> LLVM.Parameter
scalarParameter t x = downcast (Parameter (ScalarPrimType t) x)

ptrParameter :: ScalarType t -> Name (Ptr t) -> LLVM.Parameter
ptrParameter t x = downcast (Parameter (PtrPrimType (ScalarPrimType t) defaultAddrSpace) x)


-- | Unpack the array environment into a set of input parameters to a function.
-- The environment here refers only to the actual free array variables that are
-- accessed by the function.
--
envParam :: forall aenv. Gamma aenv -> [LLVM.Parameter]
envParam aenv = concatMap (\(Label n, Idx' v) -> toParam v (Name n)) (IM.elems aenv)
  where
    toParam :: forall sh e. (Shape sh, Elt e) => Idx aenv (Array sh e) -> Name (Array sh e) -> [LLVM.Parameter]
    toParam _ name = arrayParam name


-- | Generate function parameters for an Array with given base name.
--
{-# INLINEABLE arrayParam #-}
arrayParam
    :: forall sh e. (Shape sh, Elt e)
    => Name (Array sh e)
    -> [LLVM.Parameter]
arrayParam name = ad ++ sh
  where
    ad = travTypeToList (undefined :: e)  (\t i -> ptrParameter    t (arrayName name i))
    sh = travTypeToList (undefined :: sh) (\t i -> scalarParameter t (shapeName name i))

