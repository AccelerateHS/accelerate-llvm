{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Base
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Base
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt, eltType )

import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.Attribute
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant
import LLVM.General.AST.Global                                  as G

-- standard library
import Control.Monad.State
import qualified Data.Map                                       as Map
import qualified Data.Sequence                                  as Seq

#include "accelerate.h"


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

-- | The code generator for scalar functions emits monadic operations. Since
-- LLVM IR is static single assignment, we need to generate new operand names
-- each time the function is applied.
--
type IRFun1 aenv f = [Operand]              -> CodeGen [Operand]
type IRFun2 aenv f = [Operand] -> [Operand] -> CodeGen [Operand]

-- | A wrapper representing the state of code generation for a delayed array
--
data IRDelayed aenv a where
  IRDelayed :: (Shape sh, Elt e) =>
    { delayedExtent       :: IRExp  aenv sh
    , delayedIndex        :: IRFun1 aenv (sh  -> e)
    , delayedLinearIndex  :: IRFun1 aenv (Int -> e)
    }                     -> IRDelayed aenv (Array sh e)


-- Control flow
-- ============

-- | Unconditional branch
--
br :: Name -> CodeGen Name
br target = terminate $ Do (Br target [])

-- | Conditional branch
--
cbr :: Operand -> Name -> Name -> CodeGen Name
cbr cond t f = terminate $ Do (CondBr cond t f [])

-- | Phi nodes. These are always inserted at the start of the instruction
-- stream, at the top of the basic block.
--
phi :: Type                 -- ^ type of the incoming value
    -> [(Operand, Name)]    -- ^ list of operands and the predecessor basic block they come from
    -> CodeGen Operand
phi t incoming = do
  name  <- lift freshName
  block <- gets currentBlock
  phi' block name t incoming

phi' :: Name -> Name -> Type -> [(Operand, Name)] -> CodeGen Operand
phi' block crit t incoming = do
  let op            = Phi t incoming []
      --
      push Nothing  = INTERNAL_ERROR(error) "phi" "unknown basic block"
      push (Just b) = Just $ b { instructions = crit := op Seq.<| instructions b }
  --
  modify $ \s -> s { blockChain = Map.alter push block (blockChain s) }
  return (LocalReference crit)


-- Functions & Declarations
-- ========================

-- | Call a global function. A function declaration is inserted into the symbol
-- table.
--
call :: Name                    -- ^ function name
     -> Type                    -- ^ return type
     -> [(Type, Operand)]       -- ^ list of function argument types and input operand
     -> [FunctionAttribute]     -- ^ optional function attributes list (only: noreturn, nounwind, readonl, readnone)
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

