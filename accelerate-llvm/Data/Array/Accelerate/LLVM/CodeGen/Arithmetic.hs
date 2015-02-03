{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
  where

-- import Data.Default.Class
import Control.Applicative

import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Representation

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad


add :: NumType a -> IR a -> IR a -> CodeGen (IR a)
add t (op t -> x) (op t -> y) = ir t <$> instr (Add t x y)

-- mul :: (SingleValueType a, IsNum a) => Operand a -> Operand a -> CodeGen (Operand a)
-- mul x y = instr $ Mul x y

-- sub :: Operand a -> Operand a -> CodeGen (Operand a)
-- sub x y = instr $ Add



{--
-- For various reasons, constant values get inlined into the instruction stream,
-- and are never bound to names to be reused.
--
--
constant :: forall a. (IsNum a, SingleValueType a) => a -> CodeGen (Operand a)
constant =
  case numType :: NumType a of
    IntegralNumType ty | IntegralDict <- integralDict ty -> return . ConstantOperand . IntegralConstant
    FloatingNumType ty | FloatingDict <- floatingDict ty -> return . ConstantOperand . FloatingConstant
--}
{--
instance (IsNum a, SingleValueType a) => Num (CodeGen (Operand a)) where
  (+) = app2 Add
  (-) = app2 Sub
  (*) = app2 Mul

  fromInteger x
    | IntegralNumType ty        <- numType :: NumType a
    , IntegralDict              <- integralDict ty
    = return . ConstantOperand . IntegralConstant $ fromInteger x

  fromInteger x
    | FloatingNumType ty        <- numType :: NumType a
    , FloatingDict              <- floatingDict ty
    = return . ConstantOperand . FloatingConstant $ fromInteger x


instance (IsNum a, SingleValueType a) => Real (CodeGen (Operand a))
instance (IsScalar a, SingleValueType a) => Enum (CodeGen (Operand a))

instance (IsScalar a, SingleValueType a) => Eq (CodeGen (Operand a)) where
  (==) = error "todo"

instance (IsScalar a, SingleValueType a) => Ord (CodeGen (Operand a)) where
  min = error "todo"

instance (IsIntegral a, SingleValueType a) => Integral (CodeGen (Operand a)) where
  div = app2 Div
  rem = app2 Rem


app2 f x y = do
  x' <- x
  y' <- y
  instr $ f x' y'
--}

