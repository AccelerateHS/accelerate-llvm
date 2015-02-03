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
scalar t = OP_Scalar . ConstantOperand . ScalarConstant t

