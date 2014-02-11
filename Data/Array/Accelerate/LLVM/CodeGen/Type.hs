{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-- accelerate
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Util

-- llvm-general
import LLVM.General.AST.Float
import LLVM.General.AST.Type

-- standard library
import Foreign.C.Types


-- Generate the LLVM type for atomic types
--
class TypeOf a where
  typeOf :: a -> Type

instance TypeOf (ScalarType a) where
  typeOf = llvmOfScalarType

instance TypeOf (NumType a) where
  typeOf = llvmOfNumType

instance TypeOf (IntegralType a) where
  typeOf = llvmOfIntegralType

instance TypeOf (FloatingType a) where
  typeOf = llvmOfFloatingType

instance TypeOf (NonNumType a) where
  typeOf = llvmOfNonNumType


llvmOfTupleType :: TupleType a -> [Type]
llvmOfTupleType UnitTuple         = []
llvmOfTupleType (SingleTuple t)   = [llvmOfScalarType t]
llvmOfTupleType (PairTuple t1 t2) = llvmOfTupleType t1 ++ llvmOfTupleType t2

llvmOfScalarType :: ScalarType a -> Type
llvmOfScalarType (NumScalarType t)    = llvmOfNumType t
llvmOfScalarType (NonNumScalarType t) = llvmOfNonNumType t


llvmOfNumType :: NumType a -> Type
llvmOfNumType (IntegralNumType i) = llvmOfIntegralType i
llvmOfNumType (FloatingNumType f) = llvmOfFloatingType f

llvmOfIntegralType :: forall a. IntegralType a -> Type
llvmOfIntegralType i | IntegralDict <- integralDict i = IntegerType (bitSize (undefined::a))

llvmOfFloatingType :: FloatingType a -> Type
llvmOfFloatingType f =
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


signedIntegralNum :: IntegralType a -> Bool
signedIntegralNum t =
  case t of
    TypeInt _    -> True
    TypeInt8 _   -> True
    TypeInt16 _  -> True
    TypeInt32 _  -> True
    TypeInt64 _  -> True
    TypeCShort _ -> True
    TypeCInt _   -> True
    TypeCLong _  -> True
    TypeCLLong _ -> True
    _            -> False

unsignedIntegralNum :: IntegralType a -> Bool
unsignedIntegralNum = not . signedIntegralNum

float :: FloatingType a -> a -> SomeFloat
float t f =
  case t of
    TypeFloat  _                    -> Single f
    TypeDouble _                    -> Double f
    TypeCFloat _  | CFloat f'  <- f -> Single f'
    TypeCDouble _ | CDouble f' <- f -> Double f'

