{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleInstances   #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Base
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Base
  where

-- accelerate
import Data.Array.Accelerate.AST                                ( Idx )
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt, eltType )

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Attribute
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant
import LLVM.General.AST.Global                                  as G

-- standard library
import qualified Data.IntMap                                    as IM


-- Names & Operands
-- ================

-- Generate some names from a given base name and type
--
varNames :: Elt a => a -> String -> [Name]
varNames t base = [ Name (base ++ show i) | i <- [n-1, n-2 .. 0] ]
  where
    n = length (llvmOfTupleType (eltType t))

arrayData :: forall sh e. Elt e => Array sh e -> Name -> [Name]
arrayData _ base = varNames (undefined::e) (s ++ ".ad")
  where
    s = case base of
          UnName v -> (show v)
          Name v   -> v

arrayShape :: forall sh e. Shape sh => Array sh e -> Name -> [Name]
arrayShape _ base = varNames (undefined::sh) (s ++ ".sh")
  where
    s = case base of
          UnName v -> (show v)
          Name v   -> v

-- References
--
local :: Name -> Operand
local = LocalReference

global :: Name -> Operand
global = ConstantOperand . GlobalReference

class Rvalue a where
  rvalue :: a -> Operand

instance Rvalue Name where
  rvalue = local

instance Rvalue Operand where
  rvalue = id

instance Rvalue Constant where
  rvalue = ConstantOperand


-- Code generation
-- ===============

-- | The code generator produces a sequence of operands representing the LLVM
-- instructions needed to execute the expression of type `t` in surrounding
-- environment `env`. These are just phantom types.
--
-- The result consists of a list of operands, each representing the single field
-- of a (flattened) tuple expression down to atomic types.
--
type IR env aenv t = [Operand]

type IRExp aenv t  = CodeGen [Operand]

class IROperand a where
  toIRExp :: a -> CodeGen [Operand]

instance IROperand [Operand] where
  toIRExp = return

instance IROperand Name where
  toIRExp = getVariable

-- | The code generator for scalar functions emits monadic operations. Since
-- LLVM IR is static single assignment, we need to generate new operand names
-- each time the function is applied.
--
type IRFun1 aenv f = forall a. IROperand a
                     => a              -> CodeGen [Operand]
type IRFun2 aenv f = forall a b. (IROperand a, IROperand b)
                     => a -> b -> CodeGen [Operand]

-- | A wrapper representing the state of code generation for a delayed array
--
data IRDelayed aenv a where
  IRDelayed :: (Shape sh, Elt e) =>
    { delayedExtent       :: IRExp  aenv sh
    , delayedIndex        :: IRFun1 aenv (sh  -> e)
    , delayedLinearIndex  :: IRFun1 aenv (Int -> e)
    }                     -> IRDelayed aenv (Array sh e)


-- Functions & Declarations
-- ========================

-- | Call a global function. A function declaration is inserted into the symbol
-- table.
--
call :: Name                    -- ^ function name
     -> Type                    -- ^ return type
     -> [(Type, Operand)]       -- ^ list of function argument types and input operand
     -> [FunctionAttribute]     -- ^ optional function attributes list (only: noreturn, nounwind, readonly, readnone)
     -> CodeGen Operand
call fn rt tyargs attrs = do
  let (ty,args) = unzip tyargs
      params    = [ Parameter t (UnName n) [] | t <- ty | n <- [0..] ]
      toArgs    = map (,[])
      decl      = functionDefaults { name                 = fn
                                   , returnType           = rt
                                   , parameters           = (params,False)
                                   , G.functionAttributes = attrs }
  --
  declare decl
  instr $ Call False C [] (Right (global fn)) (toArgs args) attrs []


-- | Unpack the array environment into a set of input parameters to a function.
-- The environment here refers only to the actual free array variables that are
-- accessed by the function.
--
envParam :: forall aenv. Gamma aenv -> [Parameter]
envParam aenv = concatMap (\(n, Idx' v) -> toParam v n) (IM.elems aenv)
  where
    toParam :: forall sh e. (Shape sh, Elt e) => Idx aenv (Array sh e) -> Name -> [Parameter]
    toParam _ name = arrayParam (undefined::Array sh e) name


-- | Specify an array of particular type and base name as an input parameter to
-- a function.
--
arrayParam :: forall sh e. (Shape sh, Elt e) => Array sh e -> Name -> [Parameter]
arrayParam _ name =
  let ptr t = PointerType t (AddrSpace 0)
  in
  [ Parameter (ptr t) v [NoAlias, NoCapture]                  -- accelerate arrays won't alias
      | t <- llvmOfTupleType (eltType (undefined::e))
      | v <- arrayData (undefined::Array sh e) name ] ++
  [ Parameter t v []
      | t <- llvmOfTupleType (eltType (undefined::sh))
      | v <- arrayShape (undefined::Array sh e) name ]

