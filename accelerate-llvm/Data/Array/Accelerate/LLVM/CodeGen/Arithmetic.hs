{-# LANGUAGE GADTs               #-}
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

-- standard/external libraries
import Prelude                                                  ( Eq, Num, Char, String, Bool(..), ($), (++), (==), error, undefined, return, otherwise, flip )
import Data.Bits                                                ( finiteBitSize )
import Control.Applicative
import qualified Data.Ord                                       as Ord

-- accelerate
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar

-- accelerate-llvm
import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Global
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad                 hiding ( cbr, phi, intrinsic )
import Data.Array.Accelerate.LLVM.CodeGen.Type
import qualified Data.Array.Accelerate.LLVM.CodeGen.Monad       as M


-- Operations from Num
-- -------------------

add :: NumType a -> IR a -> IR a -> CodeGen (IR a)
add = binop Add

sub :: NumType a -> IR a -> IR a -> CodeGen (IR a)
sub = binop Sub

mul :: NumType a -> IR a -> IR a -> CodeGen (IR a)
mul = binop Mul

negate :: NumType a -> IR a -> CodeGen (IR a)
negate t x =
  case t of
    IntegralNumType i | unsigned i                     -> return x
    IntegralNumType i | IntegralDict <- integralDict i -> mul t x (ir t (num t (-1)))
    FloatingNumType f | FloatingDict <- floatingDict f -> mul t x (ir t (num t (-1)))

abs :: NumType a -> IR a -> CodeGen (IR a)
abs = error "abs"

signum :: NumType a -> IR a -> CodeGen (IR a)
signum = error "signum"


-- Operations from Integral and Bits
-- ---------------------------------

quot :: IntegralType a -> IR a -> IR a -> CodeGen (IR a)
quot = binop Quot

rem :: IntegralType a -> IR a -> IR a -> CodeGen (IR a)
rem = binop Rem

quotRem :: IntegralType a -> IR a -> IR a -> CodeGen (IR (a,a))
quotRem = error "quotRem"

idiv :: IntegralType a -> IR a -> IR a -> CodeGen (IR a)
idiv = error "idiv"

mod :: IntegralType a -> IR a -> IR a -> CodeGen (IR a)
mod = error "mod"

divMod :: IntegralType a -> IR a -> IR a -> CodeGen (IR (a,a))
divMod = error "divMod"

band :: IntegralType a -> IR a -> IR a -> CodeGen (IR a)
band = binop BAnd

bor :: IntegralType a -> IR a -> IR a -> CodeGen (IR a)
bor = binop BOr

xor :: IntegralType a -> IR a -> IR a -> CodeGen (IR a)
xor = binop BXor

complement :: IntegralType a -> IR a -> CodeGen (IR a)
complement t x | IntegralDict <- integralDict t = xor t x (ir t (integral t (-1)))

shiftL :: IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
shiftL t x i = do
  i' <- fromIntegral integralType (IntegralNumType t) i
  binop ShiftL t x i'

shiftR :: IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
shiftR t x i = do
  i' <- fromIntegral integralType (IntegralNumType t) i
  if signed t
     then binop ShiftRA t x i'
     else binop ShiftRL t x i'

rotateL :: IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
rotateL = error "rotateL"

rotateR :: forall a. IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
rotateR = error "rotateR"


-- Operators from Fractional and Floating
-- --------------------------------------

fdiv :: FloatingType a -> IR a -> IR a -> CodeGen (IR a)
fdiv = binop Div

recip :: FloatingType a -> IR a -> CodeGen (IR a)
recip t x | FloatingDict <- floatingDict t = fdiv t (ir t (floating t 1)) x

sin :: FloatingType a -> IR a -> CodeGen (IR a)
sin = mathf "sin"

cos :: FloatingType a -> IR a -> CodeGen (IR a)
cos = mathf "cos"

tan :: FloatingType a -> IR a -> CodeGen (IR a)
tan = mathf "tan"

asin :: FloatingType a -> IR a -> CodeGen (IR a)
asin = mathf "asin"

acos :: FloatingType a -> IR a -> CodeGen (IR a)
acos = mathf "acos"

atan :: FloatingType a -> IR a -> CodeGen (IR a)
atan = mathf "atan"

asinh :: FloatingType a -> IR a -> CodeGen (IR a)
asinh = mathf "asinh"

acosh :: FloatingType a -> IR a -> CodeGen (IR a)
acosh = mathf "acosh"

atanh :: FloatingType a -> IR a -> CodeGen (IR a)
atanh = mathf "atanh"

atan2 :: FloatingType a -> IR a -> IR a -> CodeGen (IR a)
atan2 = mathf' "atan2"

exp :: FloatingType a -> IR a -> CodeGen (IR a)
exp = mathf "exp"

fpow :: FloatingType a -> IR a -> IR a -> CodeGen (IR a)
fpow = mathf' "pow"

sqrt :: FloatingType a -> IR a -> CodeGen (IR a)
sqrt = mathf "sqrt"

log :: FloatingType a -> IR a -> CodeGen (IR a)
log = mathf "log"

logBase :: forall a. FloatingType a -> IR a -> IR a -> CodeGen (IR a)
logBase t x@(op t -> base) y | FloatingDict <- floatingDict t = logBase'
  where
    match :: Eq t => Operand t -> Operand t -> Bool
    match (ConstantOperand (ScalarConstant _ u))
          (ConstantOperand (ScalarConstant _ v)) = u == v
    match _ _                                    = False

    logBase' :: (Num a, Eq a) => CodeGen (IR a)
    logBase' | match base (floating t 2)  = mathf "log2"  t y
             | match base (floating t 10) = mathf "log10" t y
             | otherwise
             = do x' <- log t x
                  y' <- log t y
                  fdiv t y' x'


-- Operators from RealFloat
-- ------------------------

isNaN :: FloatingType a -> IR a -> CodeGen (IR Bool)
isNaN t (op t -> x) = do
  name <- M.intrinsic "isnan"
  r    <- call (Lam (NumScalarType (FloatingNumType t)) x (Body scalarType name)) [NoUnwind, ReadOnly]
  return $ ir scalarType r


-- Operators from RealFrac
-- -----------------------

truncate :: FloatingType a -> IntegralType b -> IR a -> CodeGen (IR b)
truncate tf ti (op tf -> x) = ir ti <$> instr (FPToInt tf ti x)

round :: FloatingType a -> IntegralType b -> IR a -> CodeGen (IR b)
round tf ti x = do
  i <- mathf "round" tf x
  truncate tf ti i

floor :: FloatingType a -> IntegralType b -> IR a -> CodeGen (IR b)
floor tf ti x = do
  i <- mathf "floor" tf x
  truncate tf ti i

ceiling :: FloatingType a -> IntegralType b -> IR a -> CodeGen (IR b)
ceiling tf ti x = do
  i <- mathf "ceil" tf x
  truncate tf ti i


-- Relational and Equality operators
-- ---------------------------------

cmp :: Predicate -> ScalarType a -> IR a -> IR a -> CodeGen (IR Bool)
cmp p dict (op dict -> x) (op dict -> y) = ir scalarType <$> instr (Cmp dict p x y)

lt :: ScalarType a -> IR a -> IR a -> CodeGen (IR Bool)
lt = cmp LT

gt :: ScalarType a -> IR a -> IR a -> CodeGen (IR Bool)
gt = cmp GT

lte :: ScalarType a -> IR a -> IR a -> CodeGen (IR Bool)
lte = cmp LE

gte :: ScalarType a -> IR a -> IR a -> CodeGen (IR Bool)
gte = cmp GE

eq :: ScalarType a -> IR a -> IR a -> CodeGen (IR Bool)
eq = cmp EQ

neq :: ScalarType a -> IR a -> IR a -> CodeGen (IR Bool)
neq = cmp NE

max :: ScalarType a -> IR a -> IR a -> CodeGen (IR a)
max ty x y
  | NumScalarType (FloatingNumType f) <- ty = mathf' "fmax" f x y
  | otherwise                               = do c <- op scalarType <$> gte ty x y
                                                 binop (flip Select c) ty x y

min :: ScalarType a -> IR a -> IR a -> CodeGen (IR a)
min ty x y
  | NumScalarType (FloatingNumType f) <- ty = mathf' "fmin" f x y
  | otherwise                               = do c <- op scalarType <$> lte ty x y
                                                 binop (flip Select c) ty x y


-- Logical operators
-- -----------------

land :: IR Bool -> IR Bool -> CodeGen (IR Bool)
land (op scalarType -> x) (op scalarType -> y)
  = ir scalarType <$> instr (LAnd x y)

lor  :: IR Bool -> IR Bool -> CodeGen (IR Bool)
lor (op scalarType -> x) (op scalarType -> y)
  = ir scalarType <$> instr (LOr x y)

lnot :: IR Bool -> CodeGen (IR Bool)
lnot (op scalarType -> x) = ir scalarType <$> instr (LNot x)


-- Type conversions
-- ----------------

ord :: IR Char -> CodeGen (IR Int)
ord = error "ord"

chr :: IR Int -> CodeGen (IR Char)
chr = error "chr"

boolToInt :: IR Bool -> CodeGen (IR Int)
boolToInt = error "boolToInt"

fromIntegral :: forall a b. IntegralType a -> NumType b -> IR a -> CodeGen (IR b)
fromIntegral i1 n (op i1 -> x) =
  case n of
    FloatingNumType f -> ir n <$> instr (IntToFP i1 f x)

    IntegralNumType (i2 :: IntegralType b)
      | IntegralDict <- integralDict i1
      , IntegralDict <- integralDict i2
      -> let
             bits_a = finiteBitSize (undefined::a)
             bits_b = finiteBitSize (undefined::b)
         in
         case Ord.compare bits_a bits_b of
           Ord.EQ -> ir n <$> instr (BitCast (NumScalarType n) x)
           Ord.GT -> ir n <$> instr (Trunc i1 i2 x)
           Ord.LT -> ir n <$> instr (Ext i1 i2 x)


-- Utility functions
-- -----------------

binop :: IROP dict => (dict a -> Operand a -> Operand a -> Instruction a) -> dict a -> IR a -> IR a -> CodeGen (IR a)
binop f dict (op dict -> x) (op dict -> y) = ir dict <$> instr (f dict x y)

cbr :: IR Bool -> Block -> Block -> CodeGen Block
cbr p = M.cbr (op nonNumType p)

phi :: forall a. Elt a => [(IR a, Block)] -> CodeGen (IR a)
phi incoming = IR <$> go (eltType (undefined::a)) [ (o, b) | (IR o, b) <- incoming ]
  where
    go :: TupleType t -> [(Operands t, Block)] -> CodeGen (Operands t)
    go UnitTuple         _   = return OP_Unit
    go (SingleTuple _)   ops = OP_Scalar <$> M.phi [ (x, b) | (OP_Scalar x, b) <- ops ]
    go (PairTuple t1 t2) ops = OP_Pair   <$> go t1 [ (x, b) | (OP_Pair x _, b) <- ops ]
                                         <*> go t2 [ (y, b) | (OP_Pair _ y, b) <- ops ]


ifThenElse
    :: Elt a
    => CodeGen (IR Bool)
    -> CodeGen (IR a)
    -> CodeGen (IR a)
    -> CodeGen (IR a)
ifThenElse test yes no = do
  ifThen <- newBlock "if.then"
  ifElse <- newBlock "if.else"
  ifExit <- newBlock "if.exit"

  _  <- beginBlock "if.entry"
  p  <- test
  _  <- cbr p ifThen ifElse

  setBlock ifThen
  tv <- yes
  tb <- br ifExit

  setBlock ifElse
  fv <- no
  fb <- br ifExit

  setBlock ifExit
  phi [(tv, tb), (fv, fb)]


-- Call a function from the standard C math library. This is a wrapper around
-- the 'call' function from CodeGen.Base since:
--
--   (1) The parameter and return types are all the same; and
--   (2) We check if there is an intrinsic implementation of this function
--
-- TLM: We should really be able to construct functions of any arity.
--
mathf :: String -> FloatingType t -> IR t -> CodeGen (IR t)
mathf n t (op t -> x) = do
  let st  = NumScalarType (FloatingNumType t)
  --
  name <- intrinsic t n
  r    <- call (Lam st x (Body st name)) [NoUnwind, ReadOnly]
  return $ ir t r


mathf' :: String -> FloatingType t -> IR t -> IR t -> CodeGen (IR t)
mathf' n t (op t -> x) (op t -> y) = do
  let st = NumScalarType (FloatingNumType t)
  --
  name <- intrinsic t n
  r    <- call (Lam st x (Lam st y (Body st name))) [NoUnwind, ReadOnly]
  return $ ir t r

intrinsic :: FloatingType t -> String -> CodeGen Label
intrinsic t n =
  M.intrinsic $ case t of
                  TypeFloat{}   -> n++"f"
                  TypeCFloat{}  -> n++"f"
                  TypeDouble{}  -> n
                  TypeCDouble{} -> n

