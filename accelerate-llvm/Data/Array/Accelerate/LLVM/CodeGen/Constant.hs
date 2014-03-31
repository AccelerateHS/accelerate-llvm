{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Constant
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Constant
  where

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.Constant                        ( Constant )
import qualified LLVM.General.AST.Constant              as C

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Type


-- Constants
-- =========

-- | Helper to view a constant as an operand
--
constOp :: Constant -> Operand
constOp = ConstantOperand

-- | Primitive constants
--
primConst :: PrimConst t -> Constant
primConst (PrimMinBound t) = primMinBound t
primConst (PrimMaxBound t) = primMaxBound t
primConst (PrimPi t)       = primPi t

primMinBound :: forall a. BoundedType a -> Constant
primMinBound (IntegralBoundedType t) | IntegralDict <- integralDict t = integral t minBound
primMinBound (NonNumBoundedType t)   | NonNumDict   <- nonNumDict t   = nonnum t minBound

primMaxBound :: forall a. BoundedType a -> Constant
primMaxBound (IntegralBoundedType t) | IntegralDict <- integralDict t = integral t maxBound
primMaxBound (NonNumBoundedType t)   | NonNumDict   <- nonNumDict t   = nonnum t maxBound

primPi :: forall a. FloatingType a -> Constant
primPi t | FloatingDict <- floatingDict t = floating t pi


-- | A constant value. Note that this follows the EltRepr representation of the
-- type, meaning that any nested tupling of the surface type is flattened.
--
constant :: TupleType a -> a -> [Constant]
constant UnitTuple         _       = []
constant (SingleTuple t)   c       = [scalar t c]
constant (PairTuple t1 t2) (c1,c2) = constant t1 c1 ++ constant t2 c2


-- | A constant scalar value
--
scalar :: ScalarType a -> a -> Constant
scalar (NumScalarType t)    = num t
scalar (NonNumScalarType t) = nonnum t

-- | A constant numeric value
--
num :: NumType a -> a -> Constant
num (IntegralNumType t) c = integral t c
num (FloatingNumType t) c = floating t c

-- | A constant integral value
--
integral :: IntegralType a -> a -> Constant
integral t c | IntegralDict <- integralDict t = C.Int (typeBits (llvmOfIntegralType t)) (toInteger c)

-- | A constant floating-point value
--
floating :: FloatingType a -> a -> Constant
floating t c = C.Float (someFloat t c)

-- | A constant non-numeric value
--
nonnum :: NonNumType a -> a -> Constant
nonnum t c | NonNumDict <- nonNumDict t = C.Int (typeBits (llvmOfNonNumType t)) (fromIntegral (fromEnum c))

