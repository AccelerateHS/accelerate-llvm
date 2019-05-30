{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Constant
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Constant (

  primConst,
  constant, scalar, single, vector, num, integral, floating, nonnum,
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
constant TypeRunit         ()    = OP_Unit
constant (TypeRpair ta tb) (a,b) = OP_Pair (constant ta a) (constant tb b)
constant (TypeRscalar t)   a     = ir' t (scalar t a)

scalar :: ScalarType a -> a -> Operand a
scalar t = ConstantOperand . ScalarConstant t

single :: SingleType a -> a -> Operand a
single t = scalar (SingleScalarType t)

vector :: VectorType (Vec n a) -> (Vec n a) -> Operand (Vec n a)
vector t = scalar (VectorScalarType t)

num :: NumType a -> a -> Operand a
num t = single (NumSingleType t)

integral :: IntegralType a -> a -> Operand a
integral t = num (IntegralNumType t)

floating :: FloatingType a -> a -> Operand a
floating t = num (FloatingNumType t)

nonnum :: NonNumType a -> a -> Operand a
nonnum t = single (NonNumSingleType t)


-- | The string 'undef' can be used anywhere a constant is expected, and
-- indicates that the program is well defined no matter what value is used.
--
-- <http://llvm.org/docs/LangRef.html#undefined-values>
--
undef :: ScalarType a -> Operand a
undef t = ConstantOperand (UndefConstant (PrimType (ScalarPrimType t)))

