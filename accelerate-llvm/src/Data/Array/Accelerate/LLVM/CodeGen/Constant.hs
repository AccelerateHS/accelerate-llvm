{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Constant
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Constant (

  primConst,
  constant, scalar, num, integral, floating, nonnum,
  undef,

) where


import Data.Array.Accelerate.AST                                ( PrimConst(..) )
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.LLVM.CodeGen.IR

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation


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
constant (SingleTuple t)   a     = ir' t (scalar t a)

scalar :: ScalarType a -> a -> Operand a
scalar t = ConstantOperand . ScalarConstant t

num :: NumType a -> a -> Operand a
num t = scalar (NumScalarType t)

integral :: IntegralType a -> a -> Operand a
integral t = num (IntegralNumType t)

floating :: FloatingType a -> a -> Operand a
floating t = num (FloatingNumType t)

nonnum :: NonNumType a -> a -> Operand a
nonnum t = scalar (NonNumScalarType t)


-- | The string 'undef' can be used anywhere a constant is expected, and
-- indicates that the program is well defined no matter what value is used.
--
-- <http://llvm.org/docs/LangRef.html#undefined-values>
--
undef :: ScalarType a -> Operand a
undef t = ConstantOperand (UndefConstant (PrimType (ScalarPrimType t)))

