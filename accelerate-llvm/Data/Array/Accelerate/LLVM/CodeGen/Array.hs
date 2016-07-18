{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
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

  readArray,  readVolatileArray,
  writeArray, writeVolatileArray,

) where

import Control.Applicative
import Foreign.Ptr
import Prelude                                                          hiding ( read )

import LLVM.General.AST.Type.AddrSpace
import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Representation

import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar



-- | Read a value from an array at the given index
--
readArray :: forall int sh e. IsIntegral int => IRArray (Array sh e) -> IR int -> CodeGen (IR e)
readArray (IRArray _ (IR adata)) (op integralType -> ix) =
  IR <$> readArrayData NonVolatile ix (eltType (undefined::e)) adata

-- | Read a value from a volatile array at the given index
--
readVolatileArray :: forall int sh e. IsIntegral int => IRArray (Array sh e) -> IR int -> CodeGen (IR e)
readVolatileArray (IRArray _ (IR adata)) (op integralType -> ix) =
  IR <$> readArrayData Volatile ix (eltType (undefined::e)) adata

readArrayData :: Volatile -> Operand int -> TupleType t -> Operands t -> CodeGen (Operands t)
readArrayData volatile ix = read
  where
    read :: TupleType t -> Operands t -> CodeGen (Operands t)
    read UnitTuple          OP_Unit        = return OP_Unit
    read (PairTuple t2 t1) (OP_Pair a2 a1) = OP_Pair <$> read t2 a2 <*> read t1 a1
    read (SingleTuple t)   (ptr t -> arr)  = ir' t   <$> readArrayPrim t volatile arr ix

readArrayPrim :: ScalarType e -> Volatile -> Operand (Ptr e) -> Operand int -> CodeGen (Operand e)
readArrayPrim t volatile arr ix = do
  p <- instr' $ GetElementPtr arr [ix]
  v <- instr' $ Load t volatile p
  return v


-- | Write a value into an array at the given index
--
writeArray :: forall int sh e. IsIntegral int => IRArray (Array sh e) -> IR int -> IR e -> CodeGen ()
writeArray (IRArray _ (IR adata)) (op integralType -> ix) (IR val) =
  writeArrayData NonVolatile ix (eltType (undefined::e)) adata val

-- | Write a value into a volatile array at the given index
--
writeVolatileArray :: forall int sh e. IsIntegral int => IRArray (Array sh e) -> IR int -> IR e -> CodeGen ()
writeVolatileArray (IRArray _ (IR adata)) (op integralType -> ix) (IR val) =
  writeArrayData Volatile ix (eltType (undefined::e)) adata val


writeArrayData :: Volatile -> Operand int -> TupleType t -> Operands t -> Operands t -> CodeGen ()
writeArrayData volatile ix = write
  where
    write :: TupleType e -> Operands e -> Operands e -> CodeGen ()
    write UnitTuple          OP_Unit         OP_Unit        = return ()
    write (PairTuple t2 t1) (OP_Pair a2 a1) (OP_Pair v2 v1) = write t1 a1 v1 >> write t2 a2 v2
    write (SingleTuple t)   (ptr t -> arr)  (op' t -> val)  = writeArrayPrim volatile arr ix val

writeArrayPrim :: Volatile -> Operand (Ptr e) -> Operand int -> Operand e -> CodeGen ()
writeArrayPrim volatile arr i v = do
  p <- instr' $ GetElementPtr arr [i]
  _ <- instr' $ Store volatile p v
  return ()


-- TODO: We should have IRArray store the array payloads with pointer type,
--       rather than faking it here.
--
ptr :: ScalarType t -> Operands t -> Operand (Ptr t)
ptr t (op' t -> x) =
  let
      ptr_t             = PrimType (PtrPrimType t defaultAddrSpace)
      rename (Name n)   = Name n
      rename (UnName n) = UnName n
  in
  case x of
    LocalReference _ n                    -> LocalReference ptr_t (rename n)
    ConstantOperand (GlobalReference _ n) -> ConstantOperand (GlobalReference ptr_t (rename n))
    ConstantOperand ScalarConstant{}      -> error "unexpected scalar constant"

