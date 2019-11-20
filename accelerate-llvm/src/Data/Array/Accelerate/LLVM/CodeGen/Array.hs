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
import Prelude                                                      hiding ( read )
import Data.Bits

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import Data.Array.Accelerate.Array.Sugar                            hiding ( size )

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
  x <- load a e v p
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
  writeArrayData addrspace volatility integralType ix (eltType @e) adata val

writeArrayData
    :: AddrSpace
    -> Volatility
    -> IntegralType int
    -> Operand int
    -> TupleType e
    -> Operands e
    -> Operands e
    -> CodeGen arch ()
writeArrayData a v i ix = write
  where
    write :: TupleType e -> Operands e -> Operands e -> CodeGen arch ()
    write TypeRunit          OP_Unit                  OP_Unit        = return ()
    write (TypeRpair t2 t1) (OP_Pair a2 a1)          (OP_Pair v2 v1) = write t1 a1 v1 >> write t2 a2 v2
    write (TypeRscalar e)   (asPtr a . op' e -> arr) (op' e -> val)  = writeArrayPrim a v e i arr ix val

writeArrayPrim
    :: AddrSpace
    -> Volatility
    -> ScalarType e
    -> IntegralType int
    -> Operand (Ptr e)
    -> Operand int
    -> Operand e
    -> CodeGen arch ()
writeArrayPrim a v e i arr ix x = do
  p <- getElementPtr a e i arr ix
  _ <- store a v e p x
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
  | VectorType n _ <- v
  , IntegralDict   <- integralDict i
  = if popCount n == 1
       then instr' $ GetElementPtr arr [ix]
       else do
          -- Note the initial zero into to the GEP instruction. It is not
          -- really recommended to use GEP to index into vector elements, but
          -- is not forcefully disallowed (at this time)
          ix'  <- instr' $ Mul (IntegralNumType i) ix (integral i (fromIntegral n))
          p'   <- instr' $ GetElementPtr arr [integral i 0, ix']
          p    <- instr' $ PtrCast (PtrPrimType (ScalarPrimType (VectorScalarType v)) a) p'
          return p


-- | A wrapper around the Load instruction, which splits non-power-of-two
-- SIMD types into a sequence of smaller reads.
--
-- Note: [Non-power-of-two loads and stores]
--
-- Splitting this operation a sequence of smaller power-of-two stores does
-- not work because those instructions may (will) violate alignment
-- restrictions, causing a general protection fault. So, we simply
-- implement those stores as a sequence of stores for each individual
-- element.
--
-- We could do runtime checks for what the pointer alignment is and perform
-- a vector store when we align on the right boundary, but I'm not sure the
-- extra complexity is worth it.
--
load :: AddrSpace
     -> ScalarType e
     -> Volatility
     -> Operand (Ptr e)
     -> CodeGen arch (Operand e)
load addrspace e v p
  | SingleScalarType{} <- e = instr' $ Load e v p
  | VectorScalarType s <- e
  , VectorType n base  <- s
  , m                  <- fromIntegral n
  = if popCount m == 1
       then instr' $ Load e v p
       else do
         p' <- instr' $ PtrCast (PtrPrimType (ScalarPrimType (SingleScalarType base)) addrspace) p
         --
         let go i w
               | i >= m    = return w
               | otherwise = do
                   q  <- instr' $ GetElementPtr p' [integral integralType i]
                   r  <- instr' $ Load (SingleScalarType base) v q
                   w' <- instr' $ InsertElement i w r
                   go (i+1) w'
         --
         go 0 (undef e)


-- | A wrapper around the Store instruction, which splits non-power-of-two
-- SIMD types into a sequence of smaller writes.
--
-- See: [Non-power-of-two loads and stores]
--
store :: AddrSpace
      -> Volatility
      -> ScalarType e
      -> Operand (Ptr e)
      -> Operand e
      -> CodeGen arch ()
store addrspace volatility e p v
  | SingleScalarType{} <- e = do_ $ Store volatility p v
  | VectorScalarType s <- e
  , VectorType n base  <- s
  , m                  <- fromIntegral n
  = if popCount m == 1
       then do_ $ Store volatility p v
       else do
         p' <- instr' $ PtrCast (PtrPrimType (ScalarPrimType (SingleScalarType base)) addrspace) p
         --
         let go i
               | i >= m    = return ()
               | otherwise = do
                   x <- instr' $ ExtractElement i v
                   q <- instr' $ GetElementPtr p' [integral integralType i]
                   _ <- instr' $ Store volatility q x
                   go (i+1)
         go 0

{--
      let
          go :: forall arch n t. SingleType t -> Int32 -> Operand (Ptr t) -> Operand (Vec n t) -> CodeGen arch ()
          go t offset ptr' val'
            | offset >= size = return ()
            | otherwise      = do
                let remaining = size - offset
                    this      = setBit 0 (finiteBitSize remaining - countLeadingZeros remaining - 1)

                    vec'      = VectorType (fromIntegral this) t
                    ptr_vec'  = PtrPrimType (ScalarPrimType (VectorScalarType vec')) addrspace

                    repack :: Int32 -> Operand (Vec m t) -> CodeGen arch (Operand (Vec m t))
                    repack j u
                      | j >= this = return u
                      | otherwise = do
                          x <- instr' $ ExtractElement (offset + j) val'
                          v <- instr' $ InsertElement j u x
                          repack (j+1) v

                if remaining == 1
                   then do
                     x <- instr' $ ExtractElement offset val'
                     _ <- instr' $ Store volatility ptr' x
                     return ()

                   else do
                     v <- repack 0 $ undef (VectorScalarType vec')
                     p <- instr' $ PtrCast ptr_vec' ptr'
                     _ <- instr' $ Store volatility p v

                     q <- instr' $ GetElementPtr ptr' [integral integralType this]
                     go t (offset + this) q val'

      ptr' <- instr' $ PtrCast (PtrPrimType (ScalarPrimType (SingleScalarType base)) addrspace) ptr
      go base 0 ptr' val

  where
    VectorType (fromIntegral -> size) base = vec
--}

