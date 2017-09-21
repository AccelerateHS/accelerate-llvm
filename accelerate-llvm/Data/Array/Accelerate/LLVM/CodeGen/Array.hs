{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Array
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Array (

  readArray,
  writeArray,

) where

import Control.Applicative
import Prelude                                                          hiding ( read )

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


-- | Read a value from an array at the given index
--
{-# INLINEABLE readArray #-}
readArray :: forall int sh e. IsIntegral int => IRArray (Array sh e) -> IR int -> CodeGen (IR e)
readArray (IRArray _ (IR adata) addrspace volatility) (op integralType -> ix) =
  IR <$> readArrayData addrspace volatility ix (eltType (undefined::e)) adata

readArrayData :: AddrSpace -> Volatility -> Operand int -> TupleType t -> Operands t -> CodeGen (Operands t)
readArrayData as v ix = read
  where
    read :: TupleType t -> Operands t -> CodeGen (Operands t)
    read UnitTuple          OP_Unit                  = return OP_Unit
    read (PairTuple t2 t1) (OP_Pair a2 a1)           = OP_Pair <$> read t2 a2 <*> read t1 a1
    read (SingleTuple t)   (asPtr as . op' t -> arr) = ir' t   <$> readArrayPrim t v arr ix

readArrayPrim :: ScalarType e -> Volatility -> Operand (Ptr e) -> Operand int -> CodeGen (Operand e)
readArrayPrim t v arr ix = do
  p <- instr' $ GetElementPtr arr [ix]
  x <- instr' $ Load t v p
  return x


-- | Write a value into an array at the given index
--
{-# INLINEABLE writeArray #-}
writeArray :: forall int sh e. IsIntegral int => IRArray (Array sh e) -> IR int -> IR e -> CodeGen ()
writeArray (IRArray _ (IR adata) addrspace volatility) (op integralType -> ix) (IR val) =
  writeArrayData addrspace volatility ix (eltType (undefined::e)) adata val

writeArrayData :: AddrSpace -> Volatility -> Operand int -> TupleType t -> Operands t -> Operands t -> CodeGen ()
writeArrayData as v ix = write
  where
    write :: TupleType e -> Operands e -> Operands e -> CodeGen ()
    write UnitTuple          OP_Unit                   OP_Unit        = return ()
    write (PairTuple t2 t1) (OP_Pair a2 a1)           (OP_Pair v2 v1) = write t1 a1 v1 >> write t2 a2 v2
    write (SingleTuple t)   (asPtr as . op' t -> arr) (op' t -> val)  = writeArrayPrim v arr ix val

writeArrayPrim :: Volatility -> Operand (Ptr e) -> Operand int -> Operand e -> CodeGen ()
writeArrayPrim v arr i x = do
  p <- instr' $ GetElementPtr arr [i]
  _ <- do_    $ Store v p x
  return ()

