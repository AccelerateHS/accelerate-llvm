{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Array
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Array (

  readArray,
  writeArray,

) where

import Control.Applicative
import Prelude                                                          hiding ( read )
import Data.Bits

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Ptr
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.CodeGen.Constant


-- | Read a value from an array at the given index
--
{-# INLINEABLE readArray #-}
readArray
    :: forall arch int sh e. (IsIntegral int, Elt e)
    => IRArray (Array sh e)
    -> IR int
    -> CodeGen arch (IR e)
readArray (IRArray _ (IR adata) addrspace volatility) (op integralType -> ix) =
  IR <$> readArrayData addrspace volatility integralType ix (eltType @e) adata

readArrayData
    :: AddrSpace
    -> Volatility
    -> IntegralType int
    -> Operand int
    -> TupleType e
    -> Operands e
    -> CodeGen arch (Operands e)
readArrayData a v i ix = read
  where
    read :: TupleType e -> Operands e -> CodeGen arch (Operands e)
    read TypeRunit          OP_Unit                 = return OP_Unit
    read (TypeRpair t2 t1) (OP_Pair a2 a1)          = OP_Pair <$> read t2 a2 <*> read t1 a1
    read (TypeRscalar e)   (asPtr a . op' e -> arr) = ir' e   <$> readArrayPrim a v e i arr ix

readArrayPrim
    :: AddrSpace
    -> Volatility
    -> ScalarType e
    -> IntegralType int
    -> Operand (Ptr e)
    -> Operand int
    -> CodeGen arch (Operand e)
readArrayPrim a v e i arr ix = do
  p <- getElementPtr a e i arr ix
  x <- instr' $ Load e v p
  return x


-- | Write a value into an array at the given index
--
{-# INLINEABLE writeArray #-}
writeArray
    :: forall arch int sh e. (IsIntegral int, Elt e)
    => IRArray (Array sh e)
    -> IR int
    -> IR e
    -> CodeGen arch ()
writeArray (IRArray _ (IR adata) addrspace volatility) (op integralType -> ix) (IR val) =
  writeArrayData addrspace volatility ix (eltType @e) adata val

writeArrayData :: AddrSpace -> Volatility -> Operand int -> TupleType t -> Operands t -> Operands t -> CodeGen arch ()
writeArrayData as v ix = write
  where
    write :: TupleType e -> Operands e -> Operands e -> CodeGen arch ()
    write TypeRunit          OP_Unit                   OP_Unit        = return ()
    write (TypeRpair t2 t1) (OP_Pair a2 a1)           (OP_Pair v2 v1) = write t1 a1 v1 >> write t2 a2 v2
    write (TypeRscalar t)   (asPtr as . op' t -> arr) (op' t -> val)  = writeArrayPrim v arr ix val

writeArrayPrim :: Volatility -> Operand (Ptr e) -> Operand int -> Operand e -> CodeGen arch ()
writeArrayPrim v arr i x = do
  p <- instr' $ GetElementPtr arr [i]
  _ <- do_    $ Store v p x
  return ()


-- | A wrapper around the GetElementPtr instruction, which correctly
-- computes the pointer offset for non-power-of-two SIMD types
--
getElementPtr
    :: AddrSpace
    -> ScalarType e
    -> IntegralType int
    -> Operand (Ptr e)
    -> Operand int
    -> CodeGen arch (Operand (Ptr e))
getElementPtr _ SingleScalarType{}   _ arr ix = instr' $ GetElementPtr arr [ix]
getElementPtr a (VectorScalarType v) i arr ix
  | popCount n == 1                           = instr' $ GetElementPtr arr [ix]
  | IntegralDict <- integralDict i            = do
      -- The GEP instruction only computes pointer offsets for element
      -- types which are a power-of-two number of bytes. To work around
      -- this we compute the offset ourselves using the base (non-SIMD)
      -- type.
      arr' <- instr' $ PtrCast ps arr
      ix'  <- instr' $ Mul (IntegralNumType i) ix (integral i (fromIntegral n))
      p'   <- instr' $ GetElementPtr arr' [ix']
      p    <- instr' $ PtrCast pv p'
      return p
  where
    VectorType n t = v
    pv             = PtrPrimType (ScalarPrimType (VectorScalarType v)) a
    ps             = PtrPrimType (ScalarPrimType (SingleScalarType t)) a

