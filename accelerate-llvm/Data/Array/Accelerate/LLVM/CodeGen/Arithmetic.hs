{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
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
import Prelude                                                  ( Num, ($), (++), (-), undefined, map )
import Data.Bool
import Data.Char                                                ( Char )
import Control.Applicative
import Control.Monad
import qualified Prelude                                        as P
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
                    32  -> n ++ "f"
                    64  -> n
                    128 -> n ++ "l"     -- long double
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
signum t x =
  case t of
    FloatingNumType f | FloatingDict <- floatingDict f -> signum' t x
    IntegralNumType i | IntegralDict <- integralDict i ->
      if signedIntegralNum i
         then signum' t x
         else let t' = NumScalarType t
              in  zext t' =<< neq t' x (constOp (num t 0))

signum' :: Num a => NumType a -> Operand -> CodeGen Operand
signum' t x = do
  let t'    = NumScalarType t
      zero  = constOp (num t 0)
      mone  = constOp (num t (-1))
  --
  ifFalse       <- newBlock "signum.false"
  ifEnd         <- newBlock "signum.end"

  c1            <- lt t' x zero
  top           <- cbr c1 ifEnd ifFalse

  setBlock ifFalse
  c2            <- neq t' x zero
  s             <- zext t' c2
  bot           <- br ifEnd

  setBlock ifEnd
  phi' (typeOf t) [(mone,top), (s,bot)]


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

-- Integer division, truncated towards negative infinity
--
idiv :: IntegralType a -> Operand -> Operand -> CodeGen Operand
idiv t x y
  | unsignedIntegralNum t          = quot t x y
  | IntegralDict <- integralDict t = do
      let tn   = IntegralNumType t
          ts   = NumScalarType tn
          mone = constOp (integral t (-1))
          zero = constOp (integral t 0)
          one  = constOp (integral t 1)

      ifThen    <- newBlock "idiv.then"
      ifOr      <- newBlock "idiv.or"
      ifElse    <- newBlock "idiv.else"
      ifFalse   <- newBlock "idiv.false"
      ifEnd     <- newBlock "idiv.end"

      c1        <- join $ land <$> gt ts x zero <*> lt ts y zero
      _         <- cbr c1 ifThen ifOr

      setBlock ifThen
      t1        <- add tn x mone
      t2        <- quot t t1 y
      v1        <- add tn t2 mone
      bthen     <- br ifEnd

      setBlock ifOr
      c2        <- join $ land <$> lt ts x zero <*> gt ts y zero
      _         <- cbr c2 ifElse ifFalse

      setBlock ifElse
      e1        <- add tn x one
      e2        <- quot t e1 y
      v2        <- add tn e2 mone
      belse     <- br ifEnd

      setBlock ifFalse
      v3        <- quot t x y
      bfalse    <- br ifEnd

      setBlock ifEnd
      phi' (typeOf t) [(v1,bthen), (v2,belse), (v3,bfalse)]


-- Integer modules, satisfying: (x `div` y)*y + (x `mod` y) == x
--
-- > mod x y =
-- >   if ((x > 0 && y < 0) || (x < 0 && y > 0))
-- >      then if (r /= 0)
-- >              then r + y
-- >              else 0
-- >      else r
-- >   where r = x `rem` y
--
mod :: forall a. IntegralType a -> Operand -> Operand -> CodeGen Operand
mod t x y
  | unsignedIntegralNum t          = rem t x y
  | IntegralDict <- integralDict t = do
      let t'   = NumScalarType (IntegralNumType t)
          zero = constOp (scalar t' 0)
      ifOr      <- newBlock "mod.or"
      ifTrue    <- newBlock "mod.true"
      ifEnd     <- newBlock "mod.end"

      r         <- rem t x y
      c1        <- join $ land <$> gt t' x zero <*> lt t' y zero
      _         <- cbr c1 ifTrue ifOr

      setBlock ifOr
      c2        <- join $ land <$> lt t' x zero <*> gt t' y zero
      false     <- cbr c2 ifTrue ifEnd

      setBlock ifTrue
      c3        <- eq t' r zero
      v'        <- add (IntegralNumType t) r y
      v         <- instr $ Select c3 zero v' []
      true      <- br ifEnd

      setBlock ifEnd
      phi' (typeOf t) [(v,true), (r,false)]


band :: IntegralType a -> Operand -> Operand -> CodeGen Operand
band _ x y = instr $ And x y []

bor :: IntegralType a -> Operand -> Operand -> CodeGen Operand
bor _ x y = instr $ Or x y []

xor :: IntegralType a -> Operand -> Operand -> CodeGen Operand
xor _ x y = instr $ Xor x y []

complement :: IntegralType a -> Operand -> CodeGen Operand
complement t x | IntegralDict <- integralDict t = xor t x (constOp (integral t (-1)))

shiftL :: IntegralType a -> Operand -> Operand -> CodeGen Operand
shiftL t x i = do
  i'   <- fromIntegral int (IntegralNumType t) i
  instr $ Shl nsw nuw x i' []

shiftR :: IntegralType a -> Operand -> Operand -> CodeGen Operand
shiftR t x i = do
  i'   <- fromIntegral int (IntegralNumType t) i
  instr $ if signedIntegralNum t
             then AShr False x i' []
             else LShr False x i' []

rotateL :: IntegralType a -> Operand -> Operand -> CodeGen Operand
rotateL t x i | IntegralDict <- integralDict t = do
  let bits = P.fromIntegral (typeBits (typeOf t))
  --
  i'    <- fromIntegral int (IntegralNumType t) i
  v1    <- band t i' (constOp $ integral t (bits - 1))
  v2    <- shiftL t x v1
  v3    <- sub (IntegralNumType t) (constOp $ integral t bits) v1
  v4    <- instr $ LShr False x v3 []           -- require unsigned shift here
  bor t v4 v2


rotateR :: forall a. IntegralType a -> Operand -> Operand -> CodeGen Operand
rotateR t x i | IntegralDict <- integralDict t = do
  let bits = P.fromIntegral (typeBits (typeOf t))
  --
  i'    <- fromIntegral int (IntegralNumType t) i
  v1    <- band t i' (constOp $ integral t (bits - 1))
  v2    <- instr $ LShr False x v1 []           -- require unsigned shift here
  v3    <- sub (IntegralNumType t) (constOp $ integral t bits) v1
  v4    <- shiftL t x v3
  bor t v4 v2


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
tan t x = mathf "tan" t [x]

asin :: FloatingType a -> Operand -> CodeGen Operand
asin t x = mathf "asin" t [x]

acos :: FloatingType a -> Operand -> CodeGen Operand
acos t x = mathf "acos" t [x]

atan :: FloatingType a -> Operand -> CodeGen Operand
atan t x = mathf "atan" t [x]

asinh :: FloatingType a -> Operand -> CodeGen Operand
asinh t x = mathf "asinh" t [x]

acosh :: FloatingType a -> Operand -> CodeGen Operand
acosh t x = mathf "acosh" t [x]

atanh :: FloatingType a -> Operand -> CodeGen Operand
atanh t x = mathf "atanh" t [x]

atan2 :: FloatingType a -> Operand -> Operand -> CodeGen Operand
atan2 t x y = mathf "atan2" t [x,y]

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
round tf ti x = do
  i <- mathf "round" tf [x]
  truncate tf ti i

floor :: FloatingType a -> IntegralType b -> Operand -> CodeGen Operand
floor tf ti x = do
  i <- mathf "floor" tf [x]
  truncate tf ti i

ceiling :: FloatingType a -> IntegralType b -> Operand -> CodeGen Operand
ceiling tf ti x = do
  i <- mathf "ceil" tf [x]
  truncate tf ti i


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
max ty x y =
  case ty of
    NumScalarType (FloatingNumType f)   -> mathf "fmax" f [x,y]
    _                                   -> do c <- gte ty x y
                                              instr $ Select c x y []

min :: ScalarType a -> Operand -> Operand -> CodeGen Operand
min ty x y =
  case ty of
    NumScalarType (FloatingNumType f)   -> mathf "fmin" f [x,y]
    _                                   -> do c <- lte ty x y
                                              instr $ Select c x y []

cmp :: Predicate -> ScalarType t -> Operand -> Operand -> CodeGen Operand
cmp op ty x y =
  case ty of
    NumScalarType (FloatingNumType _) -> instr $ FCmp (floatingP op) x y []

    NumScalarType (IntegralNumType i)
      | signedIntegralNum i           -> instr $ ICmp (signedP op) x y []
      | otherwise                     -> instr $ ICmp (unsignedP op) x y []

    NonNumScalarType (TypeBool _)     -> instr $ ICmp (unsignedP op) x y []
    NonNumScalarType (TypeChar _)     -> instr $ ICmp (unsignedP op) x y []
    NonNumScalarType (TypeCUChar _)   -> instr $ ICmp (unsignedP op) x y []
    NonNumScalarType (TypeCSChar _)   -> instr $ ICmp (signedP op) x y []
    NonNumScalarType (TypeCChar _)    -> instr $ ICmp (signedP op) x y []
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

land :: Operand -> Operand -> CodeGen Operand
land x y = do
  let i1    = scalarType :: ScalarType Bool
      false = constOp (scalar i1 False)
  --
  u    <- neq i1 x false
  v    <- neq i1 y false
  instr $ And u v []

lor  :: Operand -> Operand -> CodeGen Operand
lor x y = do
  let i1   = scalarType :: ScalarType Bool
      false = constOp (scalar i1 False)
  --
  u <- instr $ Or x y []
  neq i1 u false

lnot :: Operand -> CodeGen Operand
lnot x =
  let i1    = scalarType :: ScalarType Bool
      false = constOp (scalar i1 False)
  in  eq i1 x false


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
    _  -> INTERNAL_ERROR(error) "ord" "I don't know what architecture I am"

chr :: Operand -> CodeGen Operand
chr x =
  case bitSize (undefined :: Int) of
    32 -> return x
    64 -> trunc (scalarType :: ScalarType Char) x
    _  -> INTERNAL_ERROR(error) "ord" "I don't know what architecture I am"

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

