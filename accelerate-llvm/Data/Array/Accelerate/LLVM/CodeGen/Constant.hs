{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Constant
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Constant (

  constant, primConst,

) where


import Data.Array.Accelerate.AST                                ( PrimConst(..) )
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.LLVM.CodeGen.IR

import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Operand


-- | Primitive constant values
--
primConst :: PrimConst t -> t
primConst (PrimMinBound t) = primMinBound t
primConst (PrimMaxBound t) = primMaxBound t
primConst (PrimPi t)       = primPi t

primMinBound :: BoundedType a -> a
primMinBound (IntegralBoundedType t) | IntegralDict <- integralDict t = minBound
primMinBound (NonNumBoundedType t)   | NonNumDict   <- nonNumDict t   = minBound

primMaxBound :: BoundedType a -> a
primMaxBound (IntegralBoundedType t) | IntegralDict <- integralDict t = maxBound
primMaxBound (NonNumBoundedType t)   | NonNumDict   <- nonNumDict t   = maxBound

primPi :: FloatingType a -> a
primPi t | FloatingDict <- floatingDict t = pi


-- | A constant value
--
constant :: TupleType a -> a -> Operands a
constant UnitTuple         ()    = OP_Unit
constant (PairTuple ta tb) (a,b) = OP_Pair (constant ta a) (constant tb b)
constant (SingleTuple t)   a     = scalar t a

scalar :: ScalarType a -> a -> Operands a
scalar (NumScalarType t)    = num t
scalar (NonNumScalarType t) = nonnum t

num :: NumType a -> a -> Operands a
num (IntegralNumType t) = integral t
num (FloatingNumType t) = floating t

integral :: IntegralType a -> a -> Operands a
integral (TypeInt     _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeInt8    _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeInt16   _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeInt32   _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeInt64   _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeWord    _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeWord8   _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeWord16  _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeWord32  _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeWord64  _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeCShort  _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeCUShort _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeCInt    _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeCUInt   _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeCLong   _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeCULong  _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeCLLong  _) = OP_Scalar . ConstantOperand . IntegralConstant
integral (TypeCULLong _) = OP_Scalar . ConstantOperand . IntegralConstant

floating :: FloatingType a -> a -> Operands a
floating (TypeFloat   _) = OP_Scalar . ConstantOperand . FloatingConstant
floating (TypeDouble  _) = OP_Scalar . ConstantOperand . FloatingConstant
floating (TypeCFloat  _) = OP_Scalar . ConstantOperand . FloatingConstant
floating (TypeCDouble _) = OP_Scalar . ConstantOperand . FloatingConstant

nonnum :: NonNumType a -> a -> Operands a
nonnum (TypeBool   _) = OP_Scalar . ConstantOperand . NonNumConstant . toInteger . fromEnum
nonnum (TypeChar   _) = OP_Scalar . ConstantOperand . NonNumConstant . toInteger . fromEnum
nonnum (TypeCChar  _) = OP_Scalar . ConstantOperand . NonNumConstant . toInteger . fromEnum
nonnum (TypeCSChar _) = OP_Scalar . ConstantOperand . NonNumConstant . toInteger . fromEnum
nonnum (TypeCUChar _) = OP_Scalar . ConstantOperand . NonNumConstant . toInteger . fromEnum

