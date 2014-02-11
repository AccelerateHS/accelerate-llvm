{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
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
import LLVM.General.AST.Global

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


-- Convert a list of operands into a list suitable for use as arguments to a
-- function (
--
toArgs :: [Operand] -> [(Operand, [ParameterAttribute])]
toArgs = map (,[])


-- Call a global function. The function and arguments have no metadata attached.
--
call :: Name -> [Operand] -> LLVM Operand
call fn args = instr $ Call False C [] (Right (global fn)) (toArgs args) [] []


-- Floating point function calls are usually postfixed with 'f'.
--
postfix :: Name -> FloatingType a -> Name
postfix (UnName _) _ = INTERNAL_ERROR(error) "postfix" "attempt to call unnamed function"
postfix (Name fn)  t = Name $
  case t of
    TypeFloat _  -> fn ++ "f"
    TypeCFloat _ -> fn ++ "f"
    _            -> fn


-- Call an LLVM intrinsic functions
--
intrinsic :: Name -> FloatingType a -> [Operand] -> LLVM Operand
intrinsic (UnName _) _ _    = INTERNAL_ERROR(error) "intrinsic" "attempt to call unnamed function"
intrinsic (Name f)   t args = call name args
  where
    name = Name $
      case typeBits (llvmOfFloatingType t) of
        32 -> f ++ ".f32"
        64 -> f ++ ".f64"
        _  -> INTERNAL_ERROR(error) "intrinsic" "unsupported floating point size"


-- | Create a LLVM global function definition using the default options:
-- external C linkage, and no attributes or alignment annotations.
--
globalFunction :: Name -> Type -> [Parameter] -> [BasicBlock] -> Definition
globalFunction name returnType args basicBlocks
  = GlobalDefinition
  $ functionDefaults
    { name        = name
    , returnType  = returnType
    , parameters  = (args,False)
    , basicBlocks = basicBlocks
    }

