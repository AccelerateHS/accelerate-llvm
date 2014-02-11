{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Type
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Type
  where

import Data.Array.Accelerate.LLVM.Util

-- accelerate
import Data.Array.Accelerate.Type

-- llvm-general
import LLVM.General.AST.Type


llvmOfTupleType :: TupleType a -> [Type]
llvmOfTupleType UnitTuple         = []
llvmOfTupleType (SingleTuple t)   = [llvmOfScalarType t]
llvmOfTupleType (PairTuple t1 t2) = llvmOfTupleType t1 ++ llvmOfTupleType t2

llvmOfScalarType :: ScalarType a -> Type
llvmOfScalarType (NumScalarType t)    = llvmOfNumType t
llvmOfScalarType (NonNumScalarType t) = llvmOfNonNumType t


llvmOfNumType :: NumType a -> Type
llvmOfNumType (IntegralNumType i) = llvmOfIntegralNumType i
llvmOfNumType (FloatingNumType f) = llvmOfFloatingNumType f

llvmOfIntegralNumType :: IntegralType a -> Type
llvmOfIntegralNumType i =
  case i of
    TypeInt8 _    -> IntegerType 8
    TypeInt16 _   -> IntegerType 16
    TypeInt32 _   -> IntegerType 32
    TypeInt64 _   -> IntegerType 64
    TypeWord8 _   -> IntegerType 8
    TypeWord16 _  -> IntegerType 16
    TypeWord32 _  -> IntegerType 32
    TypeWord64 _  -> IntegerType 64
    TypeCShort _  -> IntegerType 16
    TypeCUShort _ -> IntegerType 16
    TypeCInt _    -> IntegerType 32
    TypeCUInt _   -> IntegerType 32
    TypeCLong _   -> IntegerType (bitSize (undefined::Int))
    TypeCULong _  -> IntegerType (bitSize (undefined::Int))
    TypeCLLong _  -> IntegerType 64
    TypeCULLong _ -> IntegerType 64
    TypeInt _     -> IntegerType (bitSize (undefined::Int))
    TypeWord _    -> IntegerType (bitSize (undefined::Int))

llvmOfFloatingNumType :: FloatingType a -> Type
llvmOfFloatingNumType f =
  case f of
    TypeFloat  _  -> FloatingPointType 32 IEEE
    TypeCFloat _  -> FloatingPointType 32 IEEE
    TypeDouble _  -> FloatingPointType 64 IEEE
    TypeCDouble _ -> FloatingPointType 64 IEEE


llvmOfNonNumType :: NonNumType t -> Type
llvmOfNonNumType t =
  case t of
    TypeBool _ -> IntegerType 8
    TypeChar _ -> IntegerType 32        -- Haskell char
    _          -> IntegerType 8         -- signed and unsigned C characters

