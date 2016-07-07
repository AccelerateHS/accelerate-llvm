{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Base
-- Copyright   : [2015] Trevor L. McDonell
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

import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Global
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.Downcast
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import qualified LLVM.General.AST.Global                                as LLVM

import qualified Data.IntMap                                            as IM

#if !MIN_VERSION_llvm_general_pure(3,4,0)
#error "llvm-3.4 or later is required"
#endif


-- References
-- ----------

local :: ScalarType a -> Name a -> IR a
local t x = ir t (LocalReference t x)

global :: ScalarType a -> Name a -> IR a
global t x = ir t (ConstantOperand (GlobalReference (Just t) x))


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
irArray :: forall sh e. (Shape sh, Elt e)
        => Name (Array sh e)
        -> IRArray (Array sh e)
irArray n
  = IRArray (travTypeToIR (undefined::sh) (\t i -> LocalReference t (shapeName n i)))
            (travTypeToIR (undefined::e)  (\t i -> LocalReference t (arrayName n i)))


-- | Generate typed local names for array data components as well as function
-- parameters to bind those names
--
mutableArray
    :: forall sh e. (Shape sh, Elt e)
    => Name (Array sh e)
    -> (IRArray (Array sh e), [LLVM.Parameter])
mutableArray name =
  ( irArray name
  , arrayParam name )


travTypeToList :: forall t a. Elt t
    => t {- dummy -}
    -> (forall t'. ScalarType t' -> Int -> a)
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
    :: forall t. Elt t
    => t {- dummy -}
    -> (forall t'. ScalarType t' -> Int -> Operand t')
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


-- Function parameters
-- -------------------

-- | Call a global function. The function declaration is inserted into the
-- symbol table.
--
call :: GlobalFunction args t -> [FunctionAttribute] -> CodeGen (IR t)
call f attrs = do
  let decl      = (downcast f) { LLVM.functionAttributes = downcast attrs' }
#if   MIN_VERSION_llvm_general_pure(3,5,0)
      attrs'    = map Right attrs
#elif MIN_VERSION_llvm_general_pure(3,4,0)
      attrs'    = attrs
#endif
  --
  declare decl
  instr (Call f attrs')


scalarParameter :: ScalarType t -> Name t -> LLVM.Parameter
scalarParameter t x = downcast (ScalarParameter t x)

ptrParameter :: ScalarType t -> Name t -> LLVM.Parameter
ptrParameter t x = downcast (PtrParameter t x)


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
arrayParam
    :: forall sh e. (Shape sh, Elt e)
    => Name (Array sh e)
    -> [LLVM.Parameter]
arrayParam name = ad ++ sh
  where
    ad = travTypeToList (undefined :: e)  (\t i -> ptrParameter    t (arrayName name i))
    sh = travTypeToList (undefined :: sh) (\t i -> scalarParameter t (shapeName name i))

