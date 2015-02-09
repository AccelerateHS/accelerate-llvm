{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Array
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Array (

  writeArray,

) where

import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Instruction

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar


-- | Write a value into an array at the given index
--
writeArray :: forall sh e. IRArray (Array sh e) -> IR Int -> IR e -> CodeGen ()
writeArray (IRArray (IR arrs)) (op integralType -> ix) (IR val) =
  writeArrayData NonVolatile ix (eltType (undefined::e)) arrs val


writeArrayData :: Volatile -> Operand Int -> TupleType t -> Operands t -> Operands t -> CodeGen ()
writeArrayData volatile ix = write
  where
    write :: TupleType e -> Operands e -> Operands e -> CodeGen ()
    write UnitTuple          OP_Unit         OP_Unit        = return ()
    write (PairTuple t2 t1) (OP_Pair a2 a1) (OP_Pair v2 v1) = write t1 a1 v1 >> write t2 a2 v2
    write (SingleTuple _t)  (OP_Scalar arr) (OP_Scalar val) = writeArrayPrim volatile arr ix val

writeArrayPrim :: Volatile -> Operand e -> Operand Int -> Operand e -> CodeGen ()
writeArrayPrim volatile arr i v = do
  ptr   <- instr $ GetElementPtr arr [i]
  _     <- instr $ Store volatile ptr v
  return ()

