{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
-- Copyright   : [2015..2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
  where

-- standard/external libraries
import Prelude                                                  ( Eq, Num, Char, Bool(..), Maybe(..), ($), (++), (==), error, undefined, otherwise, flip, fromInteger )
import Data.Bits                                                ( finiteBitSize )
import Data.String
import Control.Applicative
import Control.Monad
import qualified Prelude                                        as P
import qualified Data.Ord                                       as Ord

-- accelerate
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Error
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
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type


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
    IntegralNumType i | IntegralDict <- integralDict i -> mul t x (ir t (num t (P.negate 1)))
    FloatingNumType f | FloatingDict <- floatingDict f -> mul t x (ir t (num t (P.negate 1)))

abs :: forall a. NumType a -> IR a -> CodeGen (IR a)
abs t x =
  case t of
    FloatingNumType f                  -> mathf "fabs" f x
    IntegralNumType i
      | unsigned i                     -> return x
      | IntegralDict <- integralDict i ->
          let t' = NumScalarType t in
          case finiteBitSize (undefined :: a) of
            64 -> call (Lam t' (op t x) (Body (Just t') "llabs")) [NoUnwind, ReadNone]
            _  -> call (Lam t' (op t x) (Body (Just t') "abs"))   [NoUnwind, ReadNone]

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

mod :: Elt a => IntegralType a -> IR a -> IR a -> CodeGen (IR a)
mod t x y
  | unsigned t                     = rem t x y
  | IntegralDict <- integralDict t =
    do
       let nt = IntegralNumType t
           st = NumScalarType nt
           _0 = ir t (integral t 0)
       --
       ifOr     <- newBlock "mod.or"
       ifTrue   <- newBlock "mod.true"
       ifEnd    <- newBlock "mod.end"

       _        <- beginBlock "mod.entry"
       r        <- rem t x y
       c1       <- join $ land <$> gt st x _0 <*> lt st y _0
       _        <- cbr c1 ifTrue ifOr

       setBlock ifOr
       c2       <- join $ land <$> lt st x _0 <*> gt st y _0
       false    <- cbr c2 ifTrue ifEnd

       setBlock ifTrue
       c3       <- neq st r _0
       s        <- add nt r y
       v        <- instr $ Select st (op scalarType c3) (op t s) (op t _0)
       true     <- br ifEnd

       setBlock ifEnd
       phi [(v,true), (r,false)]


divMod :: IntegralType a -> IR a -> IR a -> CodeGen (IR (a,a))
divMod = error "divMod"

band :: IntegralType a -> IR a -> IR a -> CodeGen (IR a)
band = binop BAnd

bor :: IntegralType a -> IR a -> IR a -> CodeGen (IR a)
bor = binop BOr

xor :: IntegralType a -> IR a -> IR a -> CodeGen (IR a)
xor = binop BXor

complement :: IntegralType a -> IR a -> CodeGen (IR a)
complement t x | IntegralDict <- integralDict t = xor t x (ir t (integral t (P.negate 1)))

shiftL :: IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
shiftL t x i = do
  i' <- fromIntegral integralType (IntegralNumType t) i
  binop ShiftL t x i'

shiftR :: IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
shiftR t
  | signed t  = shiftRA t
  | otherwise = shiftRL t

shiftRL :: IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
shiftRL t x i = do
  i' <- fromIntegral integralType (IntegralNumType t) i
  r  <- binop ShiftRL t x i'
  return r

shiftRA :: IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
shiftRA t x i = do
  i' <- fromIntegral integralType (IntegralNumType t) i
  r  <- binop ShiftRA t x i'
  return r

rotateL :: forall a. IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
rotateL t x i
  | IntegralDict <- integralDict t
  = do let wsib = finiteBitSize (undefined::a)
       i1 <- band integralType i (ir integralType (integral integralType (wsib P.- 1)))
       i2 <- sub numType (ir numType (integral integralType wsib)) i1
       --
       a  <- shiftL t x i1
       b  <- shiftRL t x i2
       c  <- bor t a b
       return c

rotateR :: forall a. IntegralType a -> IR a -> IR Int -> CodeGen (IR a)
rotateR t x i = do
  i' <- negate numType i
  r  <- rotateL t x i'
  return r


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

sinh :: FloatingType a -> IR a -> CodeGen (IR a)
sinh = mathf "sinh"

cosh :: FloatingType a -> IR a -> CodeGen (IR a)
cosh = mathf "cosh"

tanh :: FloatingType a -> IR a -> CodeGen (IR a)
tanh = mathf "tanh"

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
  name <- intrinsic "isnan"
  r    <- call (Lam (NumScalarType (FloatingNumType t)) x (Body (Just scalarType) name)) [NoUnwind, ReadOnly]
  return r


-- Operators from RealFrac
-- -----------------------

truncate :: FloatingType a -> IntegralType b -> IR a -> CodeGen (IR b)
truncate tf ti (op tf -> x) = instr (FPToInt tf ti x)

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
cmp p dict (op dict -> x) (op dict -> y) = instr (Cmp dict p x y)

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
  = instr (LAnd x y)

lor  :: IR Bool -> IR Bool -> CodeGen (IR Bool)
lor (op scalarType -> x) (op scalarType -> y)
  = instr (LOr x y)

lnot :: IR Bool -> CodeGen (IR Bool)
lnot (op scalarType -> x) = instr (LNot x)


-- Type conversions
-- ----------------

ord :: IR Char -> CodeGen (IR Int)
ord (op scalarType -> x) =
  case finiteBitSize (undefined :: Int) of
    32 -> instr (BitCast scalarType x)
    64 -> instr (Trunc boundedType boundedType x)
    _  -> $internalError "ord" "I don't know what architecture I am"

chr :: IR Int -> CodeGen (IR Char)
chr (op integralType -> x) =
  case finiteBitSize (undefined :: Int) of
    32 -> instr (BitCast scalarType x)
    64 -> instr (Ext boundedType boundedType x)
    _  -> $internalError "chr" "I don't know what architecture I am"

boolToInt :: IR Bool -> CodeGen (IR Int)
boolToInt x = instr (Ext boundedType boundedType (op scalarType x))

fromIntegral :: forall a b. IntegralType a -> NumType b -> IR a -> CodeGen (IR b)
fromIntegral i1 n (op i1 -> x) =
  case n of
    FloatingNumType f -> instr (IntToFP i1 f x)

    IntegralNumType (i2 :: IntegralType b)
      | IntegralDict <- integralDict i1
      , IntegralDict <- integralDict i2
      -> let
             bits_a = finiteBitSize (undefined::a)
             bits_b = finiteBitSize (undefined::b)
         in
         case Ord.compare bits_a bits_b of
           Ord.EQ -> instr (BitCast (NumScalarType n) x)
           Ord.GT -> instr (Trunc (IntegralBoundedType i1) (IntegralBoundedType i2) x)
           Ord.LT -> instr (Ext (IntegralBoundedType i1) (IntegralBoundedType i2) x)


-- Utility functions
-- -----------------

fst :: IR (a, b) -> IR a
fst (IR (OP_Pair (OP_Pair OP_Unit x) _)) = IR x

snd :: IR (a, b) -> IR b
snd (IR (OP_Pair _ y)) = IR y

pair :: IR a -> IR b -> IR (a, b)
pair (IR x) (IR y) = IR $ OP_Pair (OP_Pair OP_Unit x) y

unpair :: IR (a, b) -> (IR a, IR b)
unpair x = (fst x, snd x)

uncurry :: (IR a -> IR b -> c) -> IR (a, b) -> c
uncurry f (unpair -> (x,y)) = f x y


binop :: IROP dict => (dict a -> Operand a -> Operand a -> Instruction a) -> dict a -> IR a -> IR a -> CodeGen (IR a)
binop f dict (op dict -> x) (op dict -> y) = instr (f dict x y)


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
  name <- lm t n
  r    <- call (Lam st x (Body (Just st) name)) [NoUnwind, ReadOnly]
  return r


mathf' :: String -> FloatingType t -> IR t -> IR t -> CodeGen (IR t)
mathf' n t (op t -> x) (op t -> y) = do
  let st = NumScalarType (FloatingNumType t)
  --
  name <- lm t n
  r    <- call (Lam st x (Lam st y (Body (Just st) name))) [NoUnwind, ReadOnly]
  return r

lm :: FloatingType t -> String -> CodeGen Label
lm t n
  = intrinsic
  $ case t of
      TypeFloat{}   -> n++"f"
      TypeCFloat{}  -> n++"f"
      TypeDouble{}  -> n
      TypeCDouble{} -> n

