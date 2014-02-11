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
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar                        ( Elt, eltType )

import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.Attribute
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant
import LLVM.General.AST.Global                                  as G

#include "accelerate.h"


-- Generate some names from a given base name and type
--
varNames :: Elt a => String -> a -> [Name]
varNames base t
  | n <- length (llvmOfTupleType (eltType t))
  = [ Name (base ++ show i) | i <- [n-1, n-2 .. 0] ]


-- References
--
local :: Name -> Operand
local = LocalReference

global :: Name -> Operand
global = ConstantOperand . GlobalReference


-- Call a global function. The function and arguments have no metadata attached.
-- A function declaration is inserted into the symbol table.
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


{--
-- Floating point function calls are usually postfixed with 'f'.
--
postfix :: Name -> FloatingType a -> Name
postfix (UnName _) _ = INTERNAL_ERROR(error) "postfix" "attempt to call unnamed function"
postfix (Name fn)  t = Name $
  case t of
    TypeFloat _  -> fn ++ "f"
    TypeCFloat _ -> fn ++ "f"
    _            -> fn
--}
{--
-- Call an LLVM intrinsic functions
--
intrinsic :: Name -> FloatingType a -> [Operand] -> CodeGen Operand
intrinsic (UnName _) _ _    = INTERNAL_ERROR(error) "intrinsic" "attempt to call unnamed function"
intrinsic (Name f)   t args = error "intrinsic" -- call name args
  where
    name = Name $
      case typeBits (llvmOfFloatingType t) of
        32 -> f ++ ".f32"
        64 -> f ++ ".f64"
        _  -> INTERNAL_ERROR(error) "intrinsic" "unsupported floating point size"
--}
{--}
-- | Create a LLVM global function definition using the default options:
-- external C linkage, and no attributes or alignment annotations.
--
globalFunction :: Name -> Type -> [Parameter] -> [BasicBlock] -> Global
globalFunction name returnType args basicBlocks
  = functionDefaults
    { name        = name
    , returnType  = returnType
    , parameters  = (args,False)
    , basicBlocks = basicBlocks
    }
--}

