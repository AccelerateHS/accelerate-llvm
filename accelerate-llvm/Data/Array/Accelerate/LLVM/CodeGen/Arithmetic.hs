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
import Prelude                                                  ( Num, Char, Bool, String, ($), error, undefined, return, otherwise )
import Control.Applicative

-- accelerate
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar

-- accelerate-llvm
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Operand

import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad                 hiding ( cbr, phi )
import Data.Array.Accelerate.LLVM.CodeGen.Type
import qualified Data.Array.Accelerate.LLVM.CodeGen.Monad       as M


-- Operations from Num
-- -------------------

add :: NumType a -> IR a -> IR a -> CodeGen (IR a)
add = binop Add

sub :: NumType a -> IR a -> IR a -> CodeGen (IR a)
sub = binop Sub

mul :: NumType a -> IR a -> IR a -> CodeGen (IR a)
mul = binop Sub

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
shiftL t (op t -> x) (op integralType -> i) = ir t <$> instr (ShiftL t x i)

shiftR :: IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
shiftR t (op t -> x) (op integralType -> i)
  | signed t  = ir t <$> instr (ShiftRA t x i)
  | otherwise = ir t <$> instr (ShiftRL t x i)

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
sin t x = error "sin" -- mathf "sin" t [x]

cos :: FloatingType a -> IR a -> CodeGen (IR a)
cos t x = error "cos" -- mathf "cos" t [x]

tan :: FloatingType a -> IR a -> CodeGen (IR a)
tan t x = error "tan" -- mathf "tan" t [x]

asin :: FloatingType a -> IR a -> CodeGen (IR a)
asin t x = error "asin" -- mathf "asin" t [x]

acos :: FloatingType a -> IR a -> CodeGen (IR a)
acos t x = error "acos" -- mathf "acos" t [x]

atan :: FloatingType a -> IR a -> CodeGen (IR a)
atan t x = error "atan" -- mathf "atan" t [x]

asinh :: FloatingType a -> IR a -> CodeGen (IR a)
asinh t x = error "asinh" -- mathf "asinh" t [x]

acosh :: FloatingType a -> IR a -> CodeGen (IR a)
acosh t x = error "acosh" -- mathf "acosh" t [x]

atanh :: FloatingType a -> IR a -> CodeGen (IR a)
atanh t x = error "atanh" -- mathf "atanh" t [x]

atan2 :: FloatingType a -> IR a -> IR a -> CodeGen (IR a)
atan2 t x y = error "atan2" -- mathf "atan2" t [x,y]

exp :: FloatingType a -> IR a -> CodeGen (IR a)
exp t x = error "exp" -- mathf "exp" t [x]

fpow :: FloatingType a -> IR a -> IR a -> CodeGen (IR a)
fpow t x y = error "pow" -- mathf "pow" t [x,y]

sqrt :: FloatingType a -> IR a -> CodeGen (IR a)
sqrt t x = error "sqrt" -- mathf "sqrt" t [x]

log :: FloatingType a -> IR a -> CodeGen (IR a)
log t x = error "log" -- mathf "log" t [x]

logBase :: FloatingType a -> IR a -> IR a -> CodeGen (IR a)
logBase t x y | FloatingDict <- floatingDict t = error "logBase" -- logBase' t x y


-- Operators from RealFloat
-- ------------------------

isNaN :: FloatingType a -> IR a -> CodeGen (IR Bool)
isNaN = error "isNaN"


-- Operators from RealFrac
-- -----------------------

truncate :: FloatingType a -> IntegralType b -> IR a -> CodeGen (IR b)
truncate _ i x = error "truncate"
--  | signedIntegralNum i = instr $ FPToSI x (typeOf i) []
--  | otherwise           = instr $ FPToUI x (typeOf i) []

round :: FloatingType a -> IntegralType b -> IR a -> CodeGen (IR b)
round tf ti x = error "round"
--  i <- mathf "round" tf [x]
--  truncate tf ti i

floor :: FloatingType a -> IntegralType b -> IR a -> CodeGen (IR b)
floor tf ti x = error "floor"
--  i <- mathf "floor" tf [x]
--  truncate tf ti i

ceiling :: FloatingType a -> IntegralType b -> IR a -> CodeGen (IR b)
ceiling tf ti x = error "ceiling"
--  i <- mathf "ceil" tf [x]
--  truncate tf ti i


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
max ty x y = error "max"

min :: ScalarType a -> IR a -> IR a -> CodeGen (IR a)
min ty x y = error "min"


-- Logical operators
-- -----------------

land :: IR Bool -> IR Bool -> CodeGen (IR Bool)
land = error "land"

lor  :: IR Bool -> IR Bool -> CodeGen (IR Bool)
lor = error "lor"

lnot :: IR Bool -> CodeGen (IR Bool)
lnot = error "lnot"


-- Type conversions
-- ----------------

ord :: IR Char -> CodeGen (IR Int)
ord = error "ord"

chr :: IR Int -> CodeGen (IR Char)
chr = error "chr"

boolToInt :: IR Bool -> CodeGen (IR Int)
boolToInt = error "boolToInt"

fromIntegral :: IntegralType a -> NumType b -> IR a -> CodeGen (IR b)
fromIntegral = error "fromIntegral"


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

