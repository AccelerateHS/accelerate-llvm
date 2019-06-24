{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Base
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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
  delayedArray,

  -- Functions & parameters
  call,
  parameter, scalarParameter, ptrParameter,
  envParam,
  arrayParam,

) where

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Global
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import {-# SOURCE #-} Data.Array.Accelerate.LLVM.CodeGen.Exp

import qualified LLVM.AST.Global                                    as LLVM

import Data.Monoid
import Data.String
import Text.Printf
import qualified Data.IntMap                                        as IM
import Prelude                                                      as P


-- References
-- ----------

local :: forall a. Elt a => Name a -> IR a
local n  = travTypeToIR @a (\t i -> LocalReference (PrimType (ScalarPrimType t)) (rename n i))

global :: forall a. Elt a => Name a -> IR a
global n = travTypeToIR @a (\t i -> ConstantOperand (GlobalReference (PrimType (ScalarPrimType t)) (rename n i)))


-- Generating names for things
-- ---------------------------

-- | Names of array data components
--
arrayName :: Name (Array sh e) -> Int -> Name e'        -- for the i-th component of the ArrayData
arrayName (Name n)   i = Name (n <> fromString (printf   ".ad%d"   i))
arrayName (UnName n) i = Name (     fromString (printf "%d.ad%d" n i))

-- | Names of shape components
--
shapeName :: Name (Array sh e) -> Int -> Name sh'       -- for the i-th component of the shape structure
shapeName (Name n)   i = Name (n <> fromString (printf   ".sh%d"   i))
shapeName (UnName n) i = Name (     fromString (printf "%d.sh%d" n i))

-- | Names combined with traversing
--
rename :: Name t -> Int -> Name t'                      -- for the i-th component of the named variable
rename (Name   n) i = Name (n <> fromString (printf    "%d"   i))
rename (UnName n) i = Name (     fromString (printf "%d.%d" n i))


-- | Names of array data elements
--
{-# INLINEABLE irArray #-}
irArray
    :: forall sh e. (Shape sh, Elt e)
    => Name    (Array sh e)
    -> IRArray (Array sh e)
irArray n
  = IRArray (travTypeToIR @sh (\t i -> LocalReference (PrimType (ScalarPrimType t)) (shapeName n i)))
            (travTypeToIR @e  (\t i -> LocalReference (PrimType (ScalarPrimType t)) (arrayName n i)))
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

-- | Generate a delayed array representation for input arrays which come in
-- either delayed (fused) or manifest representation.
--
{-# INLINEABLE delayedArray #-}
delayedArray
    :: (Shape sh, Elt e)
    => Name (Array sh e)
    -> MIRDelayed arch aenv (Array sh e)
    -> (IRDelayed arch aenv (Array sh e), [LLVM.Parameter])
delayedArray name = \case
  Just a  -> (a, [])
  Nothing -> let (arr, param) = mutableArray name
              in ( IRDelayed { delayedExtent      = return (irArrayShape arr)
                             , delayedIndex       = IRFun1 (indexArray arr)
                             , delayedLinearIndex = IRFun1 (linearIndexArray arr)
                             }
                 , param
                 )

{-# INLINEABLE travTypeToList #-}
travTypeToList
    :: forall t a. Elt t
    => (forall s. ScalarType s -> Int -> a)
    -> [a]
travTypeToList f = snd $ go (eltType @t) 0
  where
    -- DANGER: [1] must traverse in the same order as [2]
    go :: TupleType s -> Int -> (Int, [a])
    go TypeRunit         i = (i,   [])
    go (TypeRscalar t')  i = (i+1, [f t' i])
    go (TypeRpair t2 t1) i = let (i1, r1) = go t1 i
                                 (i2, r2) = go t2 i1
                             in
                             (i2, r2 ++ r1)

{-# INLINEABLE travTypeToIR #-}
travTypeToIR
    :: forall t. Elt t
    => (forall s. ScalarType s -> Int -> Operand s)
    -> IR t
travTypeToIR f = IR . snd $ go (eltType @t) 0
  where
    -- DANGER: [2] must traverse in the same order as [1]
    go :: TupleType s -> Int -> (Int, Operands s)
    go TypeRunit         i = (i,   OP_Unit)
    go (TypeRscalar t')  i = (i+1, ir' t' $ f t' i)
    go (TypeRpair t2 t1) i = let (i1, r1) = go t1 i
                                 (i2, r2) = go t2 i1
                             in
                             (i2, OP_Pair r2 r1)

-- travTypeToIRPtr
--     :: forall t. Elt t
--     => AddrSpace
--     -> t {- dummy -}
--     -> (forall s. ScalarType s -> Int -> Operand (Ptr s))
--     -> IR (Ptr t)
-- travTypeToIRPtr as t f = IR . snd $ go (eltType @t) 0
--   where
--     -- DANGER: [2] must traverse in the same order as [1]
--     -- go :: TypeR s -> Int -> (Int, Operands (Ptr s))
--     go :: TypeR (EltRepr s) -> Int -> (Int, Operands (EltRepr (Ptr s)))   -- TLM: ugh ):
--     go TypeRunit         i = (i,   OP_Unit)
--     go (TypeRscalar t')  i = (i+1, ir' (PtrPrimType t' as) $ f t' i)
--     go (TypeRpair t2 t1) i = let (i1, r1) = go t1 i
--                                  (i2, r2) = go t2 i1
--                              in
--                              (i2, OP_Pair r2 r1)


-- Function parameters
-- -------------------

-- | Call a global function. The function declaration is inserted into the
-- symbol table.
--
call :: GlobalFunction args t -> [FunctionAttribute] -> CodeGen arch (IR t)
call f attrs = do
  let decl      = (downcast f) { LLVM.functionAttributes = downcast attrs' }
      attrs'    = map Right attrs
  --
  declare decl
  instr (Call f attrs')


parameter :: forall t. Elt t => Name t -> [LLVM.Parameter]
parameter n = travTypeToList @t (\s i -> scalarParameter s (rename n i))

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
    ad = travTypeToList @e  (\t i -> ptrParameter    t (arrayName name i))
    sh = travTypeToList @sh (\t i -> scalarParameter t (shapeName name i))

