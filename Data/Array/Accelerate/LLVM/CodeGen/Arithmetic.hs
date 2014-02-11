{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--


module Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
  where

-- llvm-general
import LLVM.General.AST                                         hiding ( nuw, nsw )
import LLVM.General.AST.Attribute
import LLVM.General.AST.Constant                                ( Constant )
import qualified LLVM.General.AST.Constant                      as C
import qualified LLVM.General.AST.IntegerPredicate              as IP
import qualified LLVM.General.AST.FloatingPointPredicate        as FP

-- accelerate
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Util
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

-- standard library
import Prelude                                                  ( ($), (++), error, undefined, map )
import Data.Bool
import Data.Char                                                ( Char )
import Control.Monad
import qualified Data.Ord                                       as Ord

#include "accelerate.h"


-- Configuration
-- =============

-- | nuw and nsw stand for "No Unsigned Wrap" and "No Signed Wrap",
-- respectively. If the nuw and/or nsw keywords are present, the result value of
-- the instruction is a poison value if unsigned and/or signed overflow,
-- respectively, occurs.
--
nuw :: Bool
nuw = False

nsw :: Bool
nsw = False


-- Primitive operations
-- ====================

-- Call a function from the standard C math library. This is a wrapper around
-- the standard function `call`, because:
--
--   (a) The parameter and return types are all the same; and
--   (b) If this is a floating point type, we add 'f' to the end of the name.
--
mathf :: Name -> FloatingType t -> [Operand] -> CodeGen Operand
mathf (UnName _) _ _    = INTERNAL_ERROR(error) "mathf" "attempt to call unnamed function"
mathf (Name n)   f args = call name t (toArgs args) [NoUnwind, ReadNone]
  where
    t      = typeOf f
    toArgs = map (t,)
    name   = Name $ case typeBits t of
                    32 -> n ++ "f"
                    64 -> n ++ "f"
                    _  -> INTERNAL_ERROR(error) "mathf" "unsupported floating point size"


-- Operations from Num
-- -------------------

add :: NumType a -> Operand -> Operand -> CodeGen Operand
add (IntegralNumType _) x y = instr $ Add nuw nsw x y []
add (FloatingNumType _) x y = instr $ FAdd x y []

sub :: NumType a -> Operand -> Operand -> CodeGen Operand
sub (IntegralNumType _) x y = instr $ Sub nuw nsw x y []
sub (FloatingNumType _) x y = instr $ FSub x y []

mul :: NumType a -> Operand -> Operand -> CodeGen Operand
mul (IntegralNumType _) x y = instr $ Mul nuw nsw x y []
mul (FloatingNumType _) x y = instr $ FMul x y []

negate :: forall a. NumType a -> Operand -> CodeGen Operand
negate t x =
  case t of
    IntegralNumType i | unsignedIntegralNum i          -> return x
    IntegralNumType i | IntegralDict <- integralDict i -> mul t x (constOp (num t (-1)))
    FloatingNumType f | FloatingDict <- floatingDict f -> mul t x (constOp (num t (-1)))

abs :: forall a. NumType a -> Operand -> CodeGen Operand
abs t x =
  case t of
    FloatingNumType f                  -> mathf "fabs" f [x]
    IntegralNumType i
      | unsignedIntegralNum i          -> return x
      | IntegralDict <- integralDict i -> case bitSize (undefined :: a) of
                                            64 -> call "llabs" (typeOf i) [(typeOf i, x)] [NoUnwind, ReadNone]
                                            _  -> call "abs"   (typeOf i) [(typeOf i, x)] [NoUnwind, ReadNone]

signum :: NumType a -> Operand -> CodeGen Operand
signum = error "signum"


-- Operations from Integral and Bits
-- ---------------------------------

quot :: IntegralType a -> Operand -> Operand -> CodeGen Operand
quot t x y
  | signedIntegralNum t = instr $ SDiv False x y []
  | otherwise           = instr $ UDiv False x y []

rem :: IntegralType a -> Operand -> Operand -> CodeGen Operand
rem t x y
  | signedIntegralNum t = instr $ SRem x y []
  | otherwise           = instr $ URem x y []

idiv :: IntegralType a -> Operand -> Operand -> CodeGen Operand
idiv = error "todo: idiv"

mod :: IntegralType a -> Operand -> Operand -> CodeGen Operand
mod = error "todo: mod"

band :: IntegralType a -> Operand -> Operand -> CodeGen Operand
band _ x y = instr $ And x y []

bor :: IntegralType a -> Operand -> Operand -> CodeGen Operand
bor _ x y = instr $ Or x y []

xor :: IntegralType a -> Operand -> Operand -> CodeGen Operand
xor _ x y = instr $ Xor x y []

complement :: IntegralType a -> Operand -> CodeGen Operand
complement t x | IntegralDict <- integralDict t = xor t x (constOp (integral t (-1)))

shiftL :: IntegralType a -> Operand -> Operand -> CodeGen Operand
shiftL _ x i = instr $ Shl nsw nuw x i []

shiftR :: IntegralType a -> Operand -> Operand -> CodeGen Operand
shiftR _ x i = instr $ LShr False x i []

rotateL :: IntegralType a -> Operand -> Operand -> CodeGen Operand
rotateL = error "todo: rotateL"

rotateR :: IntegralType a -> Operand -> Operand -> CodeGen Operand
rotateR = error "todo: rotateR"


-- Operators from Fractional, Floating, RealFrac & RealFloat
-- ---------------------------------------------------------

fdiv :: FloatingType a -> Operand -> Operand -> CodeGen Operand
fdiv _ x y = instr $ FDiv x y []

recip :: FloatingType a -> Operand -> CodeGen Operand
recip t x | FloatingDict <- floatingDict t = fdiv t (constOp (floating t (-1))) x

sin :: FloatingType a -> Operand -> CodeGen Operand
sin t x = mathf "sin" t [x]

cos :: FloatingType a -> Operand -> CodeGen Operand
cos t x = mathf "cos" t [x]

tan :: FloatingType a -> Operand -> CodeGen Operand
tan = error "todo: tan"

asin :: FloatingType a -> Operand -> CodeGen Operand
asin = error "todo: asin"

acos :: FloatingType a -> Operand -> CodeGen Operand
acos = error "todo: acos"

atan :: FloatingType a -> Operand -> CodeGen Operand
atan = error "todo: atan"

asinh :: FloatingType a -> Operand -> CodeGen Operand
asinh = error "todo: asinh"

acosh :: FloatingType a -> Operand -> CodeGen Operand
acosh = error "todo: acosh"

atanh :: FloatingType a -> Operand -> CodeGen Operand
atanh = error "todo: atanh"

atan2 :: FloatingType a -> Operand -> Operand -> CodeGen Operand
atan2 = error "todo: atan2"

exp :: FloatingType a -> Operand -> CodeGen Operand
exp t x = mathf "exp" t [x]

fpow :: FloatingType a -> Operand -> Operand -> CodeGen Operand
fpow t x y = mathf "pow" t [x,y]

sqrt :: FloatingType a -> Operand -> CodeGen Operand
sqrt t x = mathf "sqrt" t [x]

log :: FloatingType a -> Operand -> CodeGen Operand
log t x = mathf "log" t [x]

logBase :: FloatingType a -> Operand -> Operand -> CodeGen Operand
logBase t x y = do
  x' <- log t x
  y' <- log t y
  fdiv t x' y'

truncate :: FloatingType a -> IntegralType b -> Operand -> CodeGen Operand
truncate _ i x
  | signedIntegralNum i = instr $ FPToSI x (typeOf i) []
  | otherwise           = instr $ FPToUI x (typeOf i) []

round :: FloatingType a -> IntegralType b -> Operand -> CodeGen Operand
round = error "todo: round"

floor :: FloatingType a -> IntegralType b -> Operand -> CodeGen Operand
floor = error "todo: floor"

ceiling :: FloatingType a -> IntegralType b -> Operand -> CodeGen Operand
ceiling = error "todo: ceiling"


-- Relational and equality operators
-- ---------------------------------

data Predicate = EQ | NE | LT | LE | GT | GE

lt :: ScalarType a -> Operand -> Operand -> CodeGen Operand
lt = cmp LT

gt :: ScalarType a -> Operand -> Operand -> CodeGen Operand
gt = cmp GT

lte :: ScalarType a -> Operand -> Operand -> CodeGen Operand
lte = cmp LE

gte :: ScalarType a -> Operand -> Operand -> CodeGen Operand
gte = cmp GE

eq :: ScalarType a -> Operand -> Operand -> CodeGen Operand
eq = cmp EQ

neq :: ScalarType a -> Operand -> Operand -> CodeGen Operand
neq = cmp NE

max :: ScalarType a -> Operand -> Operand -> CodeGen Operand
max = error "todo: max"

min :: ScalarType a -> Operand -> Operand -> CodeGen Operand
min = error "todo: min"

-- This might also need to insert a `zext .. to` instruction, as the result of
-- `icmp` and `fcmp` are of type `i1'.
--
cmp :: Predicate -> ScalarType t -> Operand -> Operand -> CodeGen Operand
cmp op ty x y =
  case ty of
    NumScalarType (IntegralNumType i)
      | signedIntegralNum i           -> instr $ ICmp (signedP op) x y []
      | otherwise                     -> instr $ ICmp (unsignedP op) x y []
    NumScalarType (FloatingNumType _) -> instr $ FCmp (floatingP op) x y []
    NonNumScalarType _                -> error "todo: non-numeric scalar comparison"
  where
    signedP :: Predicate -> IP.IntegerPredicate
    signedP EQ = IP.EQ
    signedP NE = IP.NE
    signedP LT = IP.SLT
    signedP LE = IP.SLE
    signedP GT = IP.SGT
    signedP GE = IP.SGE

    unsignedP :: Predicate -> IP.IntegerPredicate
    unsignedP EQ = IP.EQ
    unsignedP NE = IP.NE
    unsignedP LT = IP.ULT
    unsignedP LE = IP.ULE
    unsignedP GT = IP.UGT
    unsignedP GE = IP.UGE

    floatingP :: Predicate -> FP.FloatingPointPredicate
    floatingP EQ = FP.OEQ
    floatingP NE = FP.ONE
    floatingP LT = FP.OLT
    floatingP LE = FP.OLE
    floatingP GT = FP.OGT
    floatingP GE = FP.OGE

-- Logical operators
-- -----------------

-- Here we make use of the fact that Bool is represented as a Word8
--

land :: Operand -> Operand -> CodeGen Operand
land x y = do
  let i8    = scalarType :: ScalarType Word8
      false = constOp (scalar i8 0)
  --
  u <- neq i8 x false
  v <- neq i8 y false
  band (integralType :: IntegralType Word8) u v

lor  :: Operand -> Operand -> CodeGen Operand
lor x y = do
  let i8   = scalarType :: ScalarType Int8
      zero = constOp (scalar i8 0)
  --
  u <- bor (integralType :: IntegralType Word8) x y
  neq i8 u zero

lnot :: Operand -> CodeGen Operand
lnot x =
  let i8   = scalarType :: ScalarType Int8
      zero = constOp (scalar i8 0)
  in  eq i8 x zero


-- Type conversions
-- ----------------

-- Truncate the operand to the given type. The bit size of the value must be
-- larger than the bit size of the destination. Equal sized types are not
-- allowed
--
trunc :: ScalarType a -> Operand -> CodeGen Operand
trunc t x = instr $ Trunc x (typeOf t) []

-- Sign extend a value to the given type. The bit size of the value must be
-- smaller than the bit size of the destination type.
--
sext :: ScalarType a -> Operand -> CodeGen Operand
sext t x = instr $ SExt x (typeOf t) []

-- Zero extend a value to the given type. The bit size of the value must be
-- smaller than the bit size of the destination type.
--
zext :: ScalarType a -> Operand -> CodeGen Operand
zext t x = instr $ ZExt x (typeOf t) []


ord :: Operand -> CodeGen Operand
ord x =
  case bitSize (undefined :: Int) of
    32 -> return x
    64 -> instr $ SExt x (typeOf (integralType :: IntegralType Int)) []
    _  -> error "I don't know what architecture I am"

chr :: Operand -> CodeGen Operand
chr x =
  case bitSize (undefined :: Int) of
    32 -> return x
    64 -> trunc (scalarType :: ScalarType Char) x
    _  -> error "I don't know what architecture I am"

boolToInt :: Operand -> CodeGen Operand
boolToInt x = zext (scalarType :: ScalarType Int) x

fromIntegral :: forall a b. IntegralType a -> NumType b -> Operand -> CodeGen Operand
fromIntegral i1 t x =
  case t of
    -- Integral to floating point conversions have appropriate instructions
    FloatingNumType f
      | signedIntegralNum i1 -> instr $ SIToFP x (typeOf f) []
      | otherwise            -> instr $ UIToFP x (typeOf f) []

    -- Conversion between integral types requires either a truncation (if the
    -- destination bitsize is smaller) or signed/unsigned extension.
    IntegralNumType i2       ->
      let b1 = typeBits (typeOf i1)
          b2 = typeBits (typeOf i2)
          r  = NumScalarType t
      in
      case Ord.compare b1 b2 of
        Ord.EQ                        -> return x
        Ord.GT                        -> trunc r x
        Ord.LT | signedIntegralNum i1 -> sext r x
               | otherwise            -> zext r x


-- Constant operands
-- =================

cadd :: NumType a -> Constant -> Constant -> CodeGen Operand
cadd (IntegralNumType _) x y = return $ ConstantOperand $ C.Add nuw nsw x y
cadd (FloatingNumType _) x y = return $ ConstantOperand $ C.FAdd x y

