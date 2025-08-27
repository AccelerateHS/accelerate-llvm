{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Base
-- Copyright   : [2015..2020] The Accelerate Team
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

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Function
import LLVM.AST.Type.Global
import LLVM.AST.Type.InlineAssembly
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.Representation.Array                   ( Array, ArrayR(..) )
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import {-# SOURCE #-} Data.Array.Accelerate.LLVM.CodeGen.Exp

import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty     as LP

import Data.Monoid
import Data.String
import Text.Printf
import qualified Data.IntMap                                        as IM
import Prelude                                                      as P


-- References
-- ----------

local :: TypeR a -> Name a -> Operands a
local  tp n = travTypeToOperands tp (\t i -> LocalReference (PrimType (ScalarPrimType t)) (rename n i))

global :: TypeR a -> Name a -> Operands a
global tp n = travTypeToOperands tp (\t i -> ConstantOperand (GlobalReference (PrimType (ScalarPrimType t)) (rename n i)))


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
    :: ArrayR  (Array sh e)
    -> Name    (Array sh e)
    -> IRArray (Array sh e)
irArray repr@(ArrayR shr tp) n
  = IRArray repr
            (travTypeToOperands (shapeType shr) (\t i -> LocalReference (PrimType (ScalarPrimType t)) (shapeName n i)))
            (travTypeToOperands tp              (\t i -> LocalReference (PrimType (ScalarPrimType t)) (arrayName n i)))
            defaultAddrSpace
            NonVolatile

-- | Generate typed local names for array data components as well as function
-- parameters to bind those names
--
{-# INLINEABLE mutableArray #-}
mutableArray
    :: ArrayR (Array sh e)
    -> Name (Array sh e)
    -> (IRArray (Array sh e), [LP.Typed LP.Ident])
mutableArray repr name =
  ( irArray repr name
  , arrayParam repr name )

-- | Generate a delayed array representation for input arrays which come in
-- either delayed (fused) or manifest representation.
--
{-# INLINEABLE delayedArray #-}
delayedArray
    :: Name (Array sh e)
    -> MIRDelayed arch aenv (Array sh e)
    -> (IRDelayed arch aenv (Array sh e), [LP.Typed LP.Ident])
delayedArray name = \case
  IRDelayedJust a -> (a, [])
  IRDelayedNothing repr ->
    let (arr, param) = mutableArray repr name
    in ( IRDelayed { delayedRepr        = repr
                  , delayedExtent      = return (irArrayShape arr)
                  , delayedIndex       = IRFun1 (indexArray arr)
                  , delayedLinearIndex = IRFun1 (linearIndexArray arr)
                  }
      , param
      )

{-# INLINEABLE travTypeToList #-}
travTypeToList
    :: forall tp a.
       TypeR tp
    -> (forall s. ScalarType s -> Int -> a)
    -> [a]
travTypeToList tp f = snd $ go tp 0
  where
    -- DANGER: [1] must traverse in the same order as [2]
    go :: TypeR s -> Int -> (Int, [a])
    go TupRunit         i = (i,   [])
    go (TupRsingle t')  i = (i+1, [f t' i])
    go (TupRpair t2 t1) i = let (i1, r1) = go t1 i
                                (i2, r2) = go t2 i1
                            in
                            (i2, r2 ++ r1)

{-# INLINEABLE travTypeToOperands #-}
travTypeToOperands
    :: TypeR t
    -> (forall s. ScalarType s -> Int -> Operand s)
    -> Operands t
travTypeToOperands tp f = snd $ go tp 0
  where
    -- DANGER: [2] must traverse in the same order as [1]
    go :: TypeR s -> Int -> (Int, Operands s)
    go TupRunit         i = (i,   OP_Unit)
    go (TupRsingle t')  i = (i+1, ir t' $ f t' i)
    go (TupRpair t2 t1) i = let (i1, r1) = go t1 i
                                (i2, r2) = go t2 i1
                            in
                            (i2, OP_Pair r2 r1)

-- travTypeToOperandsPtr
--     :: forall t. Elt t
--     => AddrSpace
--     -> t {- dummy -}
--     -> (forall s. ScalarType s -> Int -> Operand (Ptr s))
--     -> Operands (Ptr t)
-- travTypeToOperandsPtr as t f = snd $ go (eltType @t) 0
--   where
--     -- DANGER: [2] must traverse in the same order as [1]
--     -- go :: TypeR s -> Int -> (Int, Operands (Ptr s))
--     go :: TypeR (EltRepr s) -> Int -> (Int, Operands (EltRepr (Ptr s)))   -- TLM: ugh ):
--     go TypeRunit         i = (i,   OP_Unit)
--     go (TypeRscalar t')  i = (i+1, ir (PtrPrimType t' as) $ f t' i)
--     go (TypeRpair t2 t1) i = let (i1, r1) = go t1 i
--                                  (i2, r2) = go t2 i1
--                              in
--                              (i2, OP_Pair r2 r1)


-- Function parameters
-- -------------------

-- | Call a global function.
--
-- If the function being called has a special prefix (see 'labelIsAccPrelude'),
-- the call instruction is generated as-is; otherwise (the common case), a
-- function declaration is also inserted into the symbol table, resulting in a
-- 'declare' statement in the LLVM module. A special prelude function is
-- currently not allowed to have any function attributes.
--
-- TODO: The original llvm-hs code put function attributes both on the external
-- function declaration and on the call instruction; this code puts them only
-- on the declaration. We should compare LLVM IR / benchmark to see if this is
-- an issue (@llvm-pretty@ does not yet support function attributes on call
-- instructions).
--
call :: GlobalFunction args t -> [FunctionAttribute] -> CodeGen arch (Operands t)
call f attrs = do
  let decl      = (downcast f) { LP.decAttrs = downcast attrs }
      --
      go :: GlobalFunction args t -> (Label, Function (Either InlineAssembly Label) args t)
      go (Body t k l) = (l, Body t k (Right l))
      go (Lam t x l)  = Lam t x <$> go l
  --
  let (lab, f') = go f
  if labelIsAccPrelude lab
    then case attrs of
           [] -> return ()
           _ -> internalError "Function attributes passed to a call to an acc prelude LLVM function"
    else declareExternFunc decl

  instr (Call f')


parameter :: TypeR t -> Name t -> [LP.Typed LP.Ident]
parameter tp n = travTypeToList tp (\s i -> LP.Typed (downcast s) (nameToPrettyI (rename n i)))

scalarParameter :: ScalarType t -> Name t -> LP.Typed LP.Ident
scalarParameter t x = downcast (Parameter (ScalarPrimType t) x)

ptrParameter :: ScalarType t -> Name (Ptr t) -> LP.Typed LP.Ident
ptrParameter t x = downcast (Parameter (PtrPrimType (ScalarPrimType t) defaultAddrSpace) x)


-- | Unpack the array environment into a set of input parameters to a function.
-- The environment here refers only to the actual free array variables that are
-- accessed by the function.
--
envParam :: forall aenv. Gamma aenv -> [LP.Typed LP.Ident]
envParam aenv = concatMap (\(Label n, Idx' repr _) -> arrayParam repr (Name n)) (IM.elems aenv)


-- | Generate function parameters for an Array with given base name.
--
{-# INLINEABLE arrayParam #-}
arrayParam
    :: ArrayR (Array sh e)
    -> Name (Array sh e)
    -> [LP.Typed LP.Ident]
arrayParam (ArrayR shr tp) name = ad ++ sh
  where
    ad = travTypeToList tp              (\t i -> ptrParameter    t (arrayName name i))
    sh = travTypeToList (shapeType shr) (\t i -> scalarParameter t (shapeName name i))

