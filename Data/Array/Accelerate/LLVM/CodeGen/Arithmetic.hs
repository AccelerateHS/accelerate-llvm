{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Prelude                                                  ( ($), error, undefined )
import Data.Bool
import Data.Char                                                ( Char )
import Control.Monad
import qualified Data.Ord                                       as Ord


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

-- TODO: NVVM does not support all LLVM intrinsic functions. In that case we
--       should either:
--         (a) call out to the standard C name in all cases; or
--         (b) move those functions into separate CUDA/Native arithmetic modules
--
-- The first option would be better if possible.
--

-- Operations from Num
-- -------------------

add :: NumType a -> Operand -> Operand -> LLVM Operand
add (IntegralNumType _) x y = instr $ Add nuw nsw x y []
add (FloatingNumType _) x y = instr $ FAdd x y []

sub :: NumType a -> Operand -> Operand -> LLVM Operand
sub (IntegralNumType _) x y = instr $ Sub nuw nsw x y []
sub (FloatingNumType _) x y = instr $ FSub x y []

mul :: NumType a -> Operand -> Operand -> LLVM Operand
mul (IntegralNumType _) x y = instr $ Mul nuw nsw x y []
mul (FloatingNumType _) x y = instr $ FMul x y []

negate :: forall a. NumType a -> Operand -> LLVM Operand
negate t x =
  case t of
    IntegralNumType i | unsignedIntegralNum i          -> return x
    IntegralNumType i | IntegralDict <- integralDict i -> mul t x (constOp (num t (-1)))
    FloatingNumType f | FloatingDict <- floatingDict f -> mul t x (constOp (num t (-1)))

abs :: forall a. NumType a -> Operand -> LLVM Operand
abs t x =
  case t of
    FloatingNumType f                  -> intrinsic "llvm.fabs" f [x]
    IntegralNumType i
      | unsignedIntegralNum i          -> return x
      | IntegralDict <- integralDict i -> case bitSize (undefined :: a) of
                                            64 -> call "llabs" [x]
                                            _  -> call "abs"   [x]

signum :: NumType a -> Operand -> LLVM Operand
signum = error "signum"


-- Operations from Integral and Bits
-- ---------------------------------

quot :: IntegralType a -> Operand -> Operand -> LLVM Operand
quot t x y
  | signedIntegralNum t = instr $ SDiv False x y []
  | otherwise           = instr $ UDiv False x y []

rem :: IntegralType a -> Operand -> Operand -> LLVM Operand
rem t x y
  | signedIntegralNum t = instr $ SRem x y []
  | otherwise           = instr $ URem x y []

idiv :: IntegralType a -> Operand -> Operand -> LLVM Operand
idiv = error "todo: idiv"

mod :: IntegralType a -> Operand -> Operand -> LLVM Operand
mod = error "todo: mod"

band :: IntegralType a -> Operand -> Operand -> LLVM Operand
band _ x y = instr $ And x y []

bor :: IntegralType a -> Operand -> Operand -> LLVM Operand
bor _ x y = instr $ Or x y []

xor :: IntegralType a -> Operand -> Operand -> LLVM Operand
xor _ x y = instr $ Xor x y []

complement :: IntegralType a -> Operand -> LLVM Operand
complement t x | IntegralDict <- integralDict t = xor t x (constOp (integral t (-1)))

shiftL :: IntegralType a -> Operand -> Operand -> LLVM Operand
shiftL _ x i = instr $ Shl nsw nuw x i []

shiftR :: IntegralType a -> Operand -> Operand -> LLVM Operand
shiftR _ x i = instr $ LShr False x i []

rotateL :: IntegralType a -> Operand -> Operand -> LLVM Operand
rotateL = error "todo: rotateL"

rotateR :: IntegralType a -> Operand -> Operand -> LLVM Operand
rotateR = error "todo: rotateR"


-- Operators from Fractional, Floating, RealFrac & RealFloat
-- ---------------------------------------------------------

fdiv :: FloatingType a -> Operand -> Operand -> LLVM Operand
fdiv _ x y = instr $ FDiv x y []

recip :: FloatingType a -> Operand -> LLVM Operand
recip t x | FloatingDict <- floatingDict t = fdiv t (constOp (floating t (-1))) x

sin :: FloatingType a -> Operand -> LLVM Operand
sin t x = intrinsic "llvm.sin" t [x]

cos :: FloatingType a -> Operand -> LLVM Operand
cos t x = intrinsic "llvm.cos" t [x]

tan :: FloatingType a -> Operand -> LLVM Operand
tan = error "todo: tan"

asin :: FloatingType a -> Operand -> LLVM Operand
asin = error "todo: asin"

acos :: FloatingType a -> Operand -> LLVM Operand
acos = error "todo: acos"

atan :: FloatingType a -> Operand -> LLVM Operand
atan = error "todo: atan"

asinh :: FloatingType a -> Operand -> LLVM Operand
asinh = error "todo: asinh"

acosh :: FloatingType a -> Operand -> LLVM Operand
acosh = error "todo: acosh"

atanh :: FloatingType a -> Operand -> LLVM Operand
atanh = error "todo: atanh"

atan2 :: FloatingType a -> Operand -> Operand -> LLVM Operand
atan2 = error "todo: atan2"

exp :: FloatingType a -> Operand -> LLVM Operand
exp t x = intrinsic "llvm.exp" t [x]

fpow :: FloatingType a -> Operand -> Operand -> LLVM Operand
fpow t x y = intrinsic "llvm.pow" t [x,y]

sqrt :: FloatingType a -> Operand -> LLVM Operand
sqrt t x = intrinsic "llvm.sqrt" t [x]

log :: FloatingType a -> Operand -> LLVM Operand
log t x = intrinsic "llvm.log" t [x]

logBase :: FloatingType a -> Operand -> Operand -> LLVM Operand
logBase t x y = do
  x' <- log t x
  y' <- log t y
  fdiv t x' y'

truncate :: FloatingType a -> IntegralType b -> Operand -> LLVM Operand
truncate _ i x
  | signedIntegralNum i = instr $ FPToSI x (llvmOfIntegralType i) []
  | otherwise           = instr $ FPToUI x (llvmOfIntegralType i) []

round :: FloatingType a -> IntegralType b -> Operand -> LLVM Operand
round = error "todo: round"

floor :: FloatingType a -> IntegralType b -> Operand -> LLVM Operand
floor = error "todo: floor"

ceiling :: FloatingType a -> IntegralType b -> Operand -> LLVM Operand
ceiling = error "todo: ceiling"


-- Relational and equality operators
-- ---------------------------------

data Predicate = EQ | NE | LT | LE | GT | GE

lt :: ScalarType a -> Operand -> Operand -> LLVM Operand
lt = cmp LT

gt :: ScalarType a -> Operand -> Operand -> LLVM Operand
gt = cmp GT

lte :: ScalarType a -> Operand -> Operand -> LLVM Operand
lte = cmp LE

gte :: ScalarType a -> Operand -> Operand -> LLVM Operand
gte = cmp GE

eq :: ScalarType a -> Operand -> Operand -> LLVM Operand
eq = cmp EQ

neq :: ScalarType a -> Operand -> Operand -> LLVM Operand
neq = cmp NE

max :: ScalarType a -> Operand -> Operand -> LLVM Operand
max = error "todo: max"

min :: ScalarType a -> Operand -> Operand -> LLVM Operand
min = error "todo: min"

-- This might also need to insert a `zext .. to` instruction, as the result of
-- `icmp` and `fcmp` are of type `i1'.
--
cmp :: Predicate -> ScalarType t -> Operand -> Operand -> LLVM Operand
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

land :: Operand -> Operand -> LLVM Operand
land x y = do
  let i8    = scalarType :: ScalarType Word8
      false = constOp (scalar i8 0)
  --
  u <- neq i8 x false
  v <- neq i8 y false
  band (integralType :: IntegralType Word8) u v

lor  :: Operand -> Operand -> LLVM Operand
lor x y = do
  let i8   = scalarType :: ScalarType Int8
      zero = constOp (scalar i8 0)
  --
  u <- bor (integralType :: IntegralType Word8) x y
  neq i8 u zero

lnot :: Operand -> LLVM Operand
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
trunc :: ScalarType a -> Operand -> LLVM Operand
trunc t x = instr $ Trunc x (llvmOfScalarType t) []

-- Sign extend a value to the given type. The bit size of the value must be
-- smaller than the bit size of the destination type.
--
sext :: ScalarType a -> Operand -> LLVM Operand
sext t x = instr $ SExt x (llvmOfScalarType t) []

-- Zero extend a value to the given type. The bit size of the value must be
-- smaller than the bit size of the destination type.
--
zext :: ScalarType a -> Operand -> LLVM Operand
zext t x = instr $ ZExt x (llvmOfScalarType t) []


ord :: Operand -> LLVM Operand
ord x =
  case bitSize (undefined :: Int) of
    32 -> return x
    64 -> instr $ SExt x (llvmOfIntegralType (integralType :: IntegralType Int)) []
    _  -> error "I don't know what architecture I am"

chr :: Operand -> LLVM Operand
chr x =
  case bitSize (undefined :: Int) of
    32 -> return x
    64 -> trunc (scalarType :: ScalarType Char) x
    _  -> error "I don't know what architecture I am"

boolToInt :: Operand -> LLVM Operand
boolToInt x = zext (scalarType :: ScalarType Int) x

fromIntegral :: forall a b. IntegralType a -> NumType b -> Operand -> LLVM Operand
fromIntegral i1 t x =
  case t of
    -- Integral to floating point conversions have appropriate instructions
    FloatingNumType f
      | signedIntegralNum i1 -> instr $ SIToFP x (llvmOfFloatingType f) []
      | otherwise            -> instr $ UIToFP x (llvmOfFloatingType f) []

    -- Conversion between integral types requires either a truncation (if the
    -- destination bitsize is smaller) or signed/unsigned extension.
    IntegralNumType i2       ->
      let b1 = typeBits (llvmOfIntegralType i1)
          b2 = typeBits (llvmOfIntegralType i2)
          r  = NumScalarType t
      in
      case Ord.compare b1 b2 of
        Ord.EQ                        -> return x
        Ord.GT                        -> trunc r x
        Ord.LT | signedIntegralNum i1 -> sext r x
               | otherwise            -> zext r x


-- Constant operands
-- =================

cadd :: NumType a -> Constant -> Constant -> LLVM Operand
cadd (IntegralNumType _) x y = return $ ConstantOperand $ C.Add nuw nsw x y
cadd (FloatingNumType _) x y = return $ ConstantOperand $ C.FAdd x y

