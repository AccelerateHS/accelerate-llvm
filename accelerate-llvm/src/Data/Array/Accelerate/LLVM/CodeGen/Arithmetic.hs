{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
  where

import Data.Array.Accelerate.AST                                    ( PrimMaybe )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Function
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Compare
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Control.Applicative
import Control.Monad
import Data.Bits                                                    ( finiteBitSize )
import Data.Bool                                                    ( Bool(..), otherwise )
import Data.ByteString.Short                                        ( ShortByteString )
import Data.Constraint                                              ( Dict(..) )
import Data.Monoid
import Data.String
import Foreign.Storable                                             ( sizeOf )
import Prelude                                                      ( Eq, Num, Maybe(..), ($), (==), (/), undefined, flip, fromInteger )
import Text.Printf
import qualified Data.Ord                                           as Ord
import qualified Prelude                                            as P


-- Operations from Num
-- -------------------

add :: NumType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
add = binop Add

sub :: NumType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
sub = binop Sub

mul :: NumType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
mul = binop Mul

negate :: NumType a -> Operands a -> CodeGen arch (Operands a)
negate t x =
  case t of
    IntegralNumType i | IntegralDict <- integralDict i -> mul t x (ir t (num t (P.negate 1)))
    FloatingNumType f | FloatingDict <- floatingDict f -> mul t x (ir t (num t (P.negate 1)))

abs :: forall arch a. NumType a -> Operands a -> CodeGen arch (Operands a)
abs n x =
  case n of
    FloatingNumType f                  -> mathf "fabs" f x
    IntegralNumType i
      | unsigned i                     -> return x
      | IntegralDict <- integralDict i ->
          let p = ScalarPrimType (SingleScalarType (NumSingleType n))
              t = PrimType p
          in
          case finiteBitSize (undefined :: a) of
            64 -> call (Lam p (op n x) (Body t Nothing "llabs")) [NoUnwind, ReadNone]
            _  -> call (Lam p (op n x) (Body t Nothing "abs"))   [NoUnwind, ReadNone]

signum :: forall arch a. NumType a -> Operands a -> CodeGen arch (Operands a)
signum t x =
  case t of
    IntegralNumType i
      | IntegralDict <- integralDict i
      , unsigned i
      -> do z <- neq (NumSingleType t) x (ir t (num t 0))
            s <- instr (BoolToInt i (unbool z))
            return s
      --
      -- http://graphics.stanford.edu/~seander/bithacks.html#CopyIntegerSign
      | IntegralDict <- integralDict i
      -> do let wsib = finiteBitSize (undefined::a)
            z <- neq (NumSingleType t) x (ir t (num t 0))
            l <- instr (BoolToInt i (unbool z))
            r <- shiftRA i x (ir integralType (integral integralType (wsib P.- 1)))
            s <- bor i l r
            return s
    --
    -- http://graphics.stanford.edu/~seander/bithacks.html#CopyIntegerSign
    FloatingNumType f
      | FloatingDict <- floatingDict f
      -> do
            l <- gt (NumSingleType t) x (ir f (floating f 0))
            r <- lt (NumSingleType t) x (ir f (floating f 0))
            u <- instr (BoolToFP f (unbool l))
            v <- instr (BoolToFP f (unbool r))
            s <- sub t u v
            return s

-- Operations from Integral and Bits
-- ---------------------------------

quot :: IntegralType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
quot = binop Quot

rem :: IntegralType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
rem = binop Rem

quotRem :: IntegralType a -> Operands a -> Operands a -> CodeGen arch (Operands (a,a))
quotRem t x y = do
  q <- quot t x y
  r <- rem  t x y
  -- TLM: On x86 we can compute quotRem with a single [i]divq instruction. This
  -- is not evident from the generated LLVM IR, which will still list both
  -- div/rem operations. Note that this may not be true for other instruction
  -- sets, for example the NVPTX assembly contains both operations. It might be
  -- worthwhile to allow backends to specialise Exp code generation in the same
  -- way the other phases of compilation are handled.
  --
  -- The following can be used to compute the remainder, and _may_ be better for
  -- architectures without a combined quotRem instruction.
  --
  -- z <- mul (IntegralNumType t) y q
  -- r <- sub (IntegralNumType t) x z
  return $ pair q r

idiv :: IntegralType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
idiv i x y
  | unsigned i
  = quot i x y
  --
  | IntegralDict <- integralDict i
  , Dict         <- integralElt i
  , zero         <- ir i (integral i 0)
  , one          <- ir i (integral i 1)
  , n            <- IntegralNumType i
  , s            <- NumSingleType n
  = if (tp, gt s x zero `land'` lt s y zero)
       then do
         a <- sub n x one
         b <- quot i a y
         c <- sub n b one
         return c
       else
    if (tp, lt s x zero `land'` gt s y zero)
       then do
         a <- add n x one
         b <- quot i a y
         c <- sub n b one
         return c
    else
         quot i x y
  where
    tp = TupRsingle $ SingleScalarType $ NumSingleType $ IntegralNumType i

mod :: IntegralType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
mod i x y
  | unsigned i
  = rem i x y
  --
  | IntegralDict <- integralDict i
  , Dict         <- integralElt i
  , zero         <- ir i (integral i 0)
  , n            <- IntegralNumType i
  , s            <- NumSingleType n
  = do r <- rem i x y
       if (tp, (gt s x zero `land'` lt s y zero) `lor'` (lt s x zero `land'` gt s y zero))
          then if (tp, neq s r zero)
                  then add n r y
                  else return zero
          else return r
  where
    tp = TupRsingle $ SingleScalarType $ NumSingleType $ IntegralNumType i

divMod :: IntegralType a -> Operands a -> Operands a -> CodeGen arch (Operands (a,a))
divMod i x y
  | unsigned i
  = quotRem i x y
  --
  | IntegralDict <- integralDict i
  , Dict         <- integralElt i
  , zero         <- ir i (integral i 0)
  , one          <- ir i (integral i 1)
  , n            <- IntegralNumType i
  , s            <- NumSingleType n
  = if (TupRpair tp tp, gt s x zero `land'` lt s y zero)
       then do
         a <- sub n x one
         b <- quotRem i a y
         c <- sub n (fst b) one
         d <- add n (snd b) y
         e <- add n d one
         return $ pair c e
       else
    if (TupRpair tp tp, lt s x zero `land'` gt s y zero)
       then do
         a <- add n x one
         b <- quotRem i a y
         c <- sub n (fst b) one
         d <- add n (snd b) y
         e <- sub n d one
         return $ pair c e
    else
         quotRem i x y
  where
    tp = TupRsingle $ SingleScalarType $ NumSingleType $ IntegralNumType i


band :: IntegralType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
band = binop BAnd

bor :: IntegralType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
bor = binop BOr

xor :: IntegralType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
xor = binop BXor

complement :: IntegralType a -> Operands a -> CodeGen arch (Operands a)
complement t x | IntegralDict <- integralDict t = xor t x (ir t (integral t (P.negate 1)))

shiftL :: IntegralType a -> Operands a -> Operands Int -> CodeGen arch (Operands a)
shiftL t x i = do
  i' <- fromIntegral integralType (IntegralNumType t) i
  binop ShiftL t x i'

shiftR :: IntegralType a -> Operands a -> Operands Int -> CodeGen arch (Operands a)
shiftR t
  | signed t  = shiftRA t
  | otherwise = shiftRL t

shiftRL :: IntegralType a -> Operands a -> Operands Int -> CodeGen arch (Operands a)
shiftRL t x i = do
  i' <- fromIntegral integralType (IntegralNumType t) i
  r  <- binop ShiftRL t x i'
  return r

shiftRA :: IntegralType a -> Operands a -> Operands Int -> CodeGen arch (Operands a)
shiftRA t x i = do
  i' <- fromIntegral integralType (IntegralNumType t) i
  r  <- binop ShiftRA t x i'
  return r

rotateL :: forall arch a. IntegralType a -> Operands a -> Operands Int -> CodeGen arch (Operands a)
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

rotateR :: IntegralType a -> Operands a -> Operands Int -> CodeGen arch (Operands a)
rotateR t x i = do
  i' <- negate numType i
  r  <- rotateL t x i'
  return r

popCount :: forall arch a. IntegralType a -> Operands a -> CodeGen arch (Operands Int)
popCount i x
  | IntegralDict <- integralDict i
  = do let ctpop = fromString $ printf "llvm.ctpop.i%d" (finiteBitSize (undefined::a))
           p     = ScalarPrimType (SingleScalarType (NumSingleType (IntegralNumType i)))
           t     = PrimType p
       --
       c <- call (Lam p (op i x) (Body t Nothing ctpop)) [NoUnwind, ReadNone]
       r <- fromIntegral i numType c
       return r

countLeadingZeros :: forall arch a. IntegralType a -> Operands a -> CodeGen arch (Operands Int)
countLeadingZeros i x
  | IntegralDict <- integralDict i
  = do let clz = fromString $ printf "llvm.ctlz.i%d" (finiteBitSize (undefined::a))
           p   = ScalarPrimType (SingleScalarType (NumSingleType (IntegralNumType i)))
           t   = PrimType p
       --
       c <- call (Lam p (op i x) (Lam primType (boolean False) (Body t Nothing clz))) [NoUnwind, ReadNone]
       r <- fromIntegral i numType c
       return r

countTrailingZeros :: forall arch a. IntegralType a -> Operands a -> CodeGen arch (Operands Int)
countTrailingZeros i x
  | IntegralDict <- integralDict i
  = do let clz = fromString $ printf "llvm.cttz.i%d" (finiteBitSize (undefined::a))
           p   = ScalarPrimType (SingleScalarType (NumSingleType (IntegralNumType i)))
           t   = PrimType p
       --
       c <- call (Lam p (op i x) (Lam primType (boolean False) (Body t Nothing clz))) [NoUnwind, ReadNone]
       r <- fromIntegral i numType c
       return r


-- Operators from Fractional and Floating
-- --------------------------------------

fdiv :: FloatingType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
fdiv = binop Div

recip :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
recip t x | FloatingDict <- floatingDict t = fdiv t (ir t (floating t 1)) x

sin :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
sin = mathf "sin"

cos :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
cos = mathf "cos"

tan :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
tan = mathf "tan"

sinh :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
sinh = mathf "sinh"

cosh :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
cosh = mathf "cosh"

tanh :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
tanh = mathf "tanh"

asin :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
asin = mathf "asin"

acos :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
acos = mathf "acos"

atan :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
atan = mathf "atan"

asinh :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
asinh = mathf "asinh"

acosh :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
acosh = mathf "acosh"

atanh :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
atanh = mathf "atanh"

atan2 :: FloatingType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
atan2 = mathf2 "atan2"

exp :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
exp = mathf "exp"

fpow :: FloatingType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
fpow = mathf2 "pow"

sqrt :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
sqrt = mathf "sqrt"

log :: FloatingType a -> Operands a -> CodeGen arch (Operands a)
log = mathf "log"

logBase :: forall arch a. FloatingType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
logBase t x@(op t -> base) y | FloatingDict <- floatingDict t = logBase'
  where
    match :: Eq t => Operand t -> Operand t -> Bool
    match (ConstantOperand (ScalarConstant _ u))
          (ConstantOperand (ScalarConstant _ v)) = u == v
    match _ _                                    = False

    logBase' :: (Num a, Eq a) => CodeGen arch (Operands a)
    logBase' | match base (floating t 2)  = mathf "log2"  t y
             | match base (floating t 10) = mathf "log10" t y
             | otherwise
             = do x' <- log t x
                  y' <- log t y
                  fdiv t y' x'


-- Operators from RealFloat
-- ------------------------

isNaN :: FloatingType a -> Operands a -> CodeGen arch (Operands Bool)
isNaN f (op f -> x) = instr (IsNaN f x)

isInfinite :: forall arch a. FloatingType a -> Operands a -> CodeGen arch (Operands Bool)
isInfinite f x = do
  x' <- abs n x
  eq (NumSingleType n) infinity x'
  where
    n :: NumType a
    n = FloatingNumType f

    infinity :: Operands a
    infinity | FloatingDict <- floatingDict f = ir f (floating f (1/0))


-- Operators from RealFrac
-- -----------------------

truncate :: FloatingType a -> IntegralType b -> Operands a -> CodeGen arch (Operands b)
truncate tf ti (op tf -> x) = instr (FPToInt tf ti x)

round :: FloatingType a -> IntegralType b -> Operands a -> CodeGen arch (Operands b)
round tf ti x = do
  i <- mathf "round" tf x
  truncate tf ti i

floor :: FloatingType a -> IntegralType b -> Operands a -> CodeGen arch (Operands b)
floor tf ti x = do
  i <- mathf "floor" tf x
  truncate tf ti i

ceiling :: FloatingType a -> IntegralType b -> Operands a -> CodeGen arch (Operands b)
ceiling tf ti x = do
  i <- mathf "ceil" tf x
  truncate tf ti i


-- Relational and Equality operators
-- ---------------------------------

cmp :: Ordering -> SingleType a -> Operands a -> Operands a -> CodeGen arch (Operands Bool)
cmp p dict (op dict -> x) (op dict -> y) = instr (Cmp dict p x y)

lt :: SingleType a -> Operands a -> Operands a -> CodeGen arch (Operands Bool)
lt = cmp LT

gt :: SingleType a -> Operands a -> Operands a -> CodeGen arch (Operands Bool)
gt = cmp GT

lte :: SingleType a -> Operands a -> Operands a -> CodeGen arch (Operands Bool)
lte = cmp LE

gte :: SingleType a -> Operands a -> Operands a -> CodeGen arch (Operands Bool)
gte = cmp GE

eq :: SingleType a -> Operands a -> Operands a -> CodeGen arch (Operands Bool)
eq = cmp EQ

neq :: SingleType a -> Operands a -> Operands a -> CodeGen arch (Operands Bool)
neq = cmp NE

max :: SingleType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
max ty x y
  | NumSingleType (FloatingNumType f) <- ty = mathf2 "fmax" f x y
  | otherwise                               = do c <- unbool <$> gte ty x y
                                                 binop (flip Select c) ty x y

min :: SingleType a -> Operands a -> Operands a -> CodeGen arch (Operands a)
min ty x y
  | NumSingleType (FloatingNumType f) <- ty = mathf2 "fmin" f x y
  | otherwise                               = do c <- unbool <$> lte ty x y
                                                 binop (flip Select c) ty x y


-- Logical operators
-- -----------------
--
-- Note that these implementations are strict in both arguments. The short
-- circuiting (&&) and (||) operators in the language are not evaluated
-- using these functions, but defined in terms of if-then-else.
--
land :: Operands Bool -> Operands Bool -> CodeGen arch (Operands Bool)
land (OP_Bool x) (OP_Bool y) = instr (LAnd x y)

lor :: Operands Bool -> Operands Bool -> CodeGen arch (Operands Bool)
lor (OP_Bool x) (OP_Bool y) = instr (LOr x y)

lnot :: Operands Bool -> CodeGen arch (Operands Bool)
lnot (OP_Bool x) = instr (LNot x)

-- Utilities for implementing bounds checks
land' :: CodeGen arch (Operands Bool) -> CodeGen arch (Operands Bool) -> CodeGen arch (Operands Bool)
land' x y = do
  a <- x
  b <- y
  land a b

lor' :: CodeGen arch (Operands Bool) -> CodeGen arch (Operands Bool) -> CodeGen arch (Operands Bool)
lor' x y = do
  a <- x
  b <- y
  lor a b

-- Type conversions
-- ----------------

fromIntegral :: forall arch a b. IntegralType a -> NumType b -> Operands a -> CodeGen arch (Operands b)
fromIntegral i1 n (op i1 -> x) =
  case n of
    FloatingNumType f
      -> instr (IntToFP i1 f x)

    IntegralNumType (i2 :: IntegralType b)
      | IntegralDict <- integralDict i1
      , IntegralDict <- integralDict i2
      -> let
             bits_a = finiteBitSize (undefined::a)
             bits_b = finiteBitSize (undefined::b)
         in
         case Ord.compare bits_a bits_b of
           Ord.EQ -> instr (BitCast (SingleScalarType (NumSingleType n)) x)
           Ord.GT -> instr (Trunc (IntegralBoundedType i1) (IntegralBoundedType i2) x)
           Ord.LT -> instr (Ext   (IntegralBoundedType i1) (IntegralBoundedType i2) x)

toFloating :: forall arch a b. NumType a -> FloatingType b -> Operands a -> CodeGen arch (Operands b)
toFloating n1 f2 (op n1 -> x) =
  case n1 of
    IntegralNumType i1
      -> instr (IntToFP i1 f2 x)

    FloatingNumType (f1 :: FloatingType a)
      | FloatingDict <- floatingDict f1
      , FloatingDict <- floatingDict f2
      -> let
             bytes_a = sizeOf (undefined::a)
             bytes_b = sizeOf (undefined::b)
         in
         case Ord.compare bytes_a bytes_b of
           Ord.EQ -> instr (BitCast (SingleScalarType (NumSingleType (FloatingNumType f2))) x)
           Ord.GT -> instr (FTrunc f1 f2 x)
           Ord.LT -> instr (FExt   f1 f2 x)

bitcast :: ScalarType a -> ScalarType b -> Operands a -> CodeGen arch (Operands b)
bitcast ta tb x
  | Just Refl <- matchScalarType ta tb = return x
  | otherwise                          = instr (BitCast tb (op ta x))


-- Utility functions
-- -----------------

fst :: Operands (a, b) -> Operands a
fst (OP_Pair x _) = x

snd :: Operands (a, b) -> Operands b
snd (OP_Pair _ y) = y

pair :: Operands a -> Operands b -> Operands (a, b)
pair x y = OP_Pair x y

unpair :: Operands (a, b) -> (Operands a, Operands b)
unpair (OP_Pair x y) = (x, y)

uncurry :: (Operands a -> Operands b -> c) -> Operands (a, b) -> c
uncurry f (OP_Pair x y) = f x y

unbool :: Operands Bool -> Operand Bool
unbool (OP_Bool x) = x

binop :: IROP dict => (dict a -> Operand a -> Operand a -> Instruction a) -> dict a -> Operands a -> Operands a -> CodeGen arch (Operands a)
binop f dict (op dict -> x) (op dict -> y) = instr (f dict x y)


fst3 :: Operands (Tup3 a b c) -> Operands a
fst3 (OP_Pair (OP_Pair (OP_Pair OP_Unit x) _) _) = x

snd3 :: Operands (Tup3 a b c) -> Operands b
snd3 (OP_Pair (OP_Pair _ y) _) = y

thd3 :: Operands (Tup3 a b c) -> Operands c
thd3 (OP_Pair _ z) = z

trip :: Operands a -> Operands b -> Operands c -> Operands (Tup3 a b c)
trip x y z = OP_Pair (OP_Pair (OP_Pair OP_Unit x) y) z

untrip :: Operands (Tup3 a b c) -> (Operands a, Operands b, Operands c)
untrip t = (fst3 t, snd3 t, thd3 t)


-- | Lift a constant value into an constant in the intermediate representation.
--
{-# INLINABLE lift #-}
lift :: TypeR a -> a -> Operands a
lift tp v = constant tp v

{-# INLINE liftInt #-}
liftInt :: Int -> Operands Int
liftInt = lift $ TupRsingle scalarTypeInt

{-# INLINE liftInt32 #-}
liftInt32 :: Int32 -> Operands Int32
liftInt32 = lift $ TupRsingle scalarTypeInt32

{-# INLINE liftWord32 #-}
liftWord32 :: Word32 -> Operands Word32
liftWord32 = lift $ TupRsingle scalarTypeWord32

{-# INLINE liftBool #-}
liftBool :: Bool -> Operands Bool
liftBool x = OP_Bool (boolean x)

-- | Standard if-then-else expression
--
ifThenElse
    :: (TypeR a, CodeGen arch (Operands Bool))
    -> CodeGen arch (Operands a)
    -> CodeGen arch (Operands a)
    -> CodeGen arch (Operands a)
ifThenElse (tp, test) yes no = do
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
  phi tp [(tv, tb), (fv, fb)]


caseof
    :: TypeR a
    -> CodeGen arch (Operands TAG)
    -> [(TAG, CodeGen arch (Operands a))]
    -> Maybe (CodeGen arch (Operands a))
    -> CodeGen arch (Operands a)
caseof tR tag xs x = do
  exit   <- newBlock "switch.exit"
  def    <- newBlock "switch.default"
  cases  <- forM xs (\(t,e) -> (t,e,) <$> newBlock (printf "switch.l%d" t))

  _    <- beginBlock "switch.entry"
  p    <- tag
  _    <- switch p def [(t,b) | (t,_,b) <- cases]

  -- Generate basic blocks for each equation
  vs   <- forM cases $ \(_,body,label) -> do
    setBlock label
    r <- body
    b <- br exit
    return (r,b)

  -- Basic block for the default case
  v    <- do
    setBlock def
    r <- case x of
        Nothing ->
          let go :: TypeR a -> Operands a
              go TupRunit       = OP_Unit
              go (TupRsingle t) = ir t (undef t)
              go (TupRpair a b) = OP_Pair (go a) (go b)
          in return (go tR)
        Just default_ -> default_
    b <- br exit
    return (r,b)

  setBlock exit
  phi tR (v:vs)


-- Execute the body only if the first argument evaluates to True
--
when :: CodeGen arch (Operands Bool) -> CodeGen arch () -> CodeGen arch ()
when test doit = do
  body <- newBlock "when.body"
  exit <- newBlock "when.exit"

  p <- test
  _ <- cbr p body exit

  setBlock body
  doit
  _ <- br exit

  setBlock exit


-- Execute the body only if the first argument evaluates to False
--
unless :: CodeGen arch (Operands Bool) -> CodeGen arch () -> CodeGen arch ()
unless test doit = do
  body <- newBlock "unless.body"
  exit <- newBlock "unless.exit"

  p <- test
  _ <- cbr p exit body

  setBlock body
  doit
  _ <- br exit

  setBlock exit


-- Call a function from the standard C math library. This is a wrapper around
-- the 'call' function from CodeGen.Base since:
--
--   (1) The parameter and return types are all the same; and
--   (2) We check if there is an intrinsic implementation of this function
--
-- TLM: We should really be able to construct functions of any arity.
--
mathf :: ShortByteString -> FloatingType t -> Operands t -> CodeGen arch (Operands t)
mathf n f (op f -> x) = do
  let s = ScalarPrimType (SingleScalarType (NumSingleType (FloatingNumType f)))
      t = PrimType s
  --
  name <- lm f n
  r    <- call (Lam s x (Body t Nothing name)) [NoUnwind, ReadOnly]
  return r


mathf2 :: ShortByteString -> FloatingType t -> Operands t -> Operands t -> CodeGen arch (Operands t)
mathf2 n f (op f -> x) (op f -> y) = do
  let s = ScalarPrimType (SingleScalarType (NumSingleType (FloatingNumType f)))
      t = PrimType s
  --
  name <- lm f n
  r    <- call (Lam s x (Lam s y (Body t Nothing name))) [NoUnwind, ReadOnly]
  return r

lm :: FloatingType t -> ShortByteString -> CodeGen arch Label
lm t n
  = intrinsic
  $ case t of
      TypeHalf{}    -> n<>"f"   -- XXX: check
      TypeFloat{}   -> n<>"f"
      TypeDouble{}  -> n

isJust :: Operands (PrimMaybe a) -> CodeGen arch (Operands Bool)
isJust (OP_Pair l _) = instr (IntToBool integralType (op integralType l))

fromJust :: Operands (PrimMaybe a) -> CodeGen arch (Operands a)
fromJust (OP_Pair _ (OP_Pair OP_Unit r)) = return r

