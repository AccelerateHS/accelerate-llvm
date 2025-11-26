{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Instruction
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Instruction
  where

import LLVM.AST.Type.Constant                             ( Constant(ScalarConstant) )
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Function
import LLVM.AST.Type.GetElementPtr
import LLVM.AST.Type.InlineAssembly
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import LLVM.AST.Type.Instruction.Atomic                   ( Atomicity, MemoryOrdering )
import LLVM.AST.Type.Instruction.Compare                  ( Ordering(..) )
import LLVM.AST.Type.Instruction.RMW                      ( RMWOperation )
import LLVM.AST.Type.Instruction.Volatile                 ( Volatility(..) )

import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty as LP

import Data.Array.Accelerate.AST                          ( PrimBool )
import Data.Array.Accelerate.AST.Idx
import qualified Data.Array.Accelerate.Debug.Internal     as Debug
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Type
import Data.Primitive.Vec

import Prelude                                            hiding ( Ordering(..), quot, rem, div, isNaN, tail )
import Data.Bifunctor                                     ( bimap )
import Data.Maybe                                         ( fromMaybe )
import System.IO.Unsafe                                   ( unsafePerformIO )


-- | Non-terminating instructions
--
--  * <http://llvm.org/docs/LangRef.html#binary-operations>
--
--  * <http://llvm.org/docs/LangRef.html#bitwise-binary-operations>
--
--  * <http://llvm.org/docs/LangRef.html#vector-operations>
--
--  * <http://llvm.org/docs/LangRef.html#aggregate-operations>
--
--  * <http://llvm.org/docs/LangRef.html#memory-access-and-addressing-operations>
--
--  * <http://llvm.org/docs/LangRef.html#other-operations>
--
data Instruction a where
  -- Binary Operations
  -- -----------------

  -- <http://llvm.org/docs/LangRef.html#add-instruction>
  -- <http://llvm.org/docs/LangRef.html#fadd-instruction>
  --
  Add             :: NumType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#sub-instruction>
  -- <http://llvm.org/docs/LangRef.html#fsub-instruction>
  --
  Sub             :: NumType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#mul-instruction>
  -- <http://llvm.org/docs/LangRef.html#fmul-instruction>
  --
  Mul             :: NumType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#udiv-instruction>
  -- <http://llvm.org/docs/LangRef.html#sdiv-instruction>
  --
  Quot            :: IntegralType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#urem-instruction>
  -- <http://llvm.org/docs/LangRef.html#srem-instruction>
  --
  Rem             :: IntegralType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#fdiv-instruction>
  --
  Div             :: FloatingType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#shl-instruction>
  --
  ShiftL          :: IntegralType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#lshr-instruction>
  --
  ShiftRL         :: IntegralType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#ashr-instruction>
  --
  ShiftRA         :: IntegralType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  -- Bitwise Binary Operations
  -- -------------------------

  -- <http://llvm.org/docs/LangRef.html#and-instruction>
  --
  BAnd            :: IntegralType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  LAnd            :: Operand Bool
                  -> Operand Bool
                  -> Instruction Bool

  -- <http://llvm.org/docs/LangRef.html#or-instruction>
  --
  BOr             :: IntegralType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  LOr             :: Operand Bool
                  -> Operand Bool
                  -> Instruction Bool

  -- <http://llvm.org/docs/LangRef.html#xor-instruction>
  --
  BXor            :: IntegralType a
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  LNot            :: Operand Bool
                  -> Instruction Bool

  -- Vector Operations
  -- -----------------

  -- <http://llvm.org/docs/LangRef.html#extractelement-instruction>
  --
  ExtractElement  :: Int32  -- TupleIdx (ProdRepr (Vec n a)) a
                  -> Operand (Vec n a)
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#insertelement-instruction>
  --
  InsertElement   :: Int32  -- TupleIdx (ProdRepr (Vec n a)) a
                  -> Operand (Vec n a)
                  -> Operand a
                  -> Instruction (Vec n a)

  -- ShuffleVector

  -- Aggregate Operations
  -- --------------------

  -- <http://llvm.org/docs/LangRef.html#extractvalue-instruction>
  -- ExtractValue is currently restricted to pairs, we might want
  -- to allow larger structures. It is currently however only used
  -- for CmpXchg, which returns a pair so we don't need this for
  -- other structures.
  --
  ExtractValue    :: ScalarType t
                  -> PairIdx tup t
                  -> Operand tup
                  -> Instruction t

  -- <http://llvm.org/docs/LangRef.html#insertvalue-instruction>
  -- InsertValue

  -- Memory Access and Addressing Operations
  -- ---------------------------------------

  -- <http://llvm.org/docs/LangRef.html#alloca-instruction>
  -- Alloca

  -- <http://llvm.org/docs/LangRef.html#load-instruction>
  --
  Load            :: ScalarType a
                  -> Volatility
                  -> Operand (Ptr a)
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#store-instruction>
  --
  Store           :: Volatility
                  -> Operand (Ptr a)
                  -> Operand a
                  -> Instruction ()

  -- <http://llvm.org/docs/LangRef.html#getelementptr-instruction>
  --
  GetElementPtr   :: GetElementPtr Operand (Ptr a) (Ptr b)
                  -> Instruction (Ptr b)

  -- <http://llvm.org/docs/LangRef.html#i-fence>
  --
  Fence           :: Atomicity
                  -> Instruction ()

  -- <http://llvm.org/docs/LangRef.html#cmpxchg-instruction>
  --
  CmpXchg         :: IntegralType a
                  -> Volatility
                  -> Operand (Ptr a)
                  -> Operand a                  -- expected value
                  -> Operand a                  -- replacement value
                  -> Atomicity                  -- on success
                  -> MemoryOrdering             -- on failure (see docs for restrictions)
                  -> Instruction (a, PrimBool)  -- should be (a, Bool)

  -- <http://llvm.org/docs/LangRef.html#atomicrmw-instruction>
  --
  AtomicRMW       :: NumType a
                  -> Volatility
                  -> RMWOperation
                  -> Operand (Ptr a)
                  -> Operand a
                  -> Atomicity
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#trunc-to-instruction>
  --
  Trunc           :: BoundedType a        -- precondition: BitSize a > BitSize b
                  -> BoundedType b
                  -> Operand a
                  -> Instruction b

  IntToBool       :: IntegralType a
                  -> Operand a
                  -> Instruction Bool

  -- <http://llvm.org/docs/LangRef.html#fptrunc-to-instruction>
  --
  FTrunc          :: FloatingType a       -- precondition: BitSize a > BitSize b
                  -> FloatingType b
                  -> Operand a
                  -> Instruction b

  -- <http://llvm.org/docs/LangRef.html#zext-to-instruction>
  -- <http://llvm.org/docs/LangRef.html#sext-to-instruction>
  --
  Ext             :: BoundedType a        -- precondition: BitSize a < BitSize b
                  -> BoundedType b
                  -> Operand a
                  -> Instruction b

  BoolToInt       :: IntegralType a
                  -> Operand Bool
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#fpext-to-instruction>
  --
  FExt            :: FloatingType a       -- precondition: BitSize a < BitSize b
                  -> FloatingType b
                  -> Operand a
                  -> Instruction b

  BoolToFP        :: FloatingType a
                  -> Operand Bool
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#fptoui-to-instruction>
  -- <http://llvm.org/docs/LangRef.html#fptosi-to-instruction>
  --
  FPToInt         :: FloatingType a
                  -> IntegralType b
                  -> Operand a
                  -> Instruction b

  -- <http://llvm.org/docs/LangRef.html#uitofp-to-instruction>
  -- <http://llvm.org/docs/LangRef.html#sitofp-to-instruction>
  --
  IntToFP         :: IntegralType a
                  -> FloatingType b
                  -> Operand a
                  -> Instruction b

  -- <http://llvm.org/docs/LangRef.html#bitcast-to-instruction>
  --
  BitCast         :: ScalarType b         -- precondition: BitSize a == BitSize b
                  -> Operand a
                  -> Instruction b

  PtrCast         :: PrimType (Ptr b)     -- precondition: same address space
                  -> Operand (Ptr a)
                  -> Instruction (Ptr b)

  -- PtrToInt
  -- IntToPtr
  -- AddrSpaceCast

  -- Other Operations
  -- ----------------

  -- <http://llvm.org/docs/LangRef.html#icmp-instruction>
  -- <http://llvm.org/docs/LangRef.html#fcmp-instruction>
  --
  -- We treat non-scalar types as signed/unsigned integer values.
  --
  Cmp             :: SingleType a
                  -> Ordering
                  -> Operand a
                  -> Operand a
                  -> Instruction Bool

  IsNaN           :: FloatingType a
                  -> Operand a
                  -> Instruction Bool

  -- <http://llvm.org/docs/LangRef.html#phi-instruction>
  --
  Phi             :: PrimType a
                  -> [(Operand a, Label)]
                  -> Instruction a

  -- <http://llvm.org/docs/LangRef.html#call-instruction>
  --
  Call            :: Function (Either InlineAssembly Label) args t
                  -> Instruction t

  -- <http://llvm.org/docs/LangRef.html#select-instruction>
  --
  Select          :: SingleType a
                  -> Operand Bool
                  -> Operand a
                  -> Operand a
                  -> Instruction a

  -- VAArg
  -- LandingPad


-- | Instances of instructions may be given a name, allowing their results to be
-- referenced as Operands. Instructions returning void (e.g. function calls)
-- don't need names.
--
data Named ins a where
  (:=) :: Name a -> ins a -> Named ins a
  Do   :: ins ()          -> Named ins ()


-- | Convert to llvm-pretty
--
instance Downcast (Instruction a) LP.Instr where
  downcast = \case
    Add t x y             -> add t (downcast x) (downcast y)
    Sub t x y             -> sub t (downcast x) (downcast y)
    Mul t x y             -> mul t (downcast x) (downcast y)
    Quot t x y            -> quot t (downcast x) (downcast y)
    Rem t x y             -> rem t (downcast x) (downcast y)
    Div _ x y             -> LP.Arith (LP.FDiv fmf) (downcast x) (LP.typedValue (downcast y))
    ShiftL _ x i          -> LP.Bit (LP.Shl nsw nuw) (downcast x) (LP.typedValue (downcast i))
    ShiftRL _ x i         -> LP.Bit (LP.Lshr exact) (downcast x) (LP.typedValue (downcast i))
    ShiftRA _ x i         -> LP.Bit (LP.Ashr exact) (downcast x) (LP.typedValue (downcast i))
    BAnd _ x y            -> LP.Bit LP.And (downcast x) (LP.typedValue (downcast y))
    LAnd x y              -> LP.Bit LP.And (downcast x) (LP.typedValue (downcast y))
    BOr _ x y             -> LP.Bit LP.Or (downcast x) (LP.typedValue (downcast y))
    LOr x y               -> LP.Bit LP.Or (downcast x) (LP.typedValue (downcast y))
    BXor _ x y            -> LP.Bit LP.Xor (downcast x) (LP.typedValue (downcast y))
    LNot x                -> LP.Bit LP.Xor (downcast x) (LP.ValInteger 1)
    InsertElement i v x   -> LP.InsertElt (downcast v) (downcast x) (constant i)
    ExtractElement i v    -> LP.ExtractElt (downcast v) (constant i)
    ExtractValue _ i s    -> extractStruct i (downcast s)
    Store vol p x         -> LP.Store (downcast vol) (downcast x) (downcast p) atomicity alignment
    Load t vol p          -> LP.Load (downcast vol) (downcast t) (downcast p) atomicity alignment
    GetElementPtr (GEP t n i1 path) ->
      LP.GEP inbounds (downcast t) (downcast n) (downcast i1 : downcast path)
    Fence a               -> LP.Fence (downcast (fst a)) (downcast (snd a))
    -- TODO: this is now a STRONG cmpxchg. Is that what was intended? I think llvm-hs defaulted to strong, but the LLVM source is very obtuse about this.
    CmpXchg _ v p x y a m -> LP.CmpXchg False (downcast v) (downcast p) (downcast x) (downcast y) (downcast (fst a)) (downcast (snd a)) (downcast m)
    AtomicRMW t v f p x a -> LP.AtomicRW (downcast v) (downcast (t,f)) (downcast p) (downcast x) (downcast (fst a)) (downcast (snd a))
    Trunc _ t x           -> LP.Conv (LP.Trunc False False) (downcast x) (downcast t)
    IntToBool _ x         -> LP.Conv (LP.Trunc False False) (downcast x) (LP.PrimType (LP.Integer 1))
    FTrunc _ t x          -> LP.Conv LP.FpTrunc (downcast x) (downcast t)
    Ext a b x             -> ext a b (downcast x)
    BoolToInt a x         -> LP.Conv (LP.ZExt False) (downcast x) (downcast a)
    BoolToFP x a          -> LP.Conv (LP.UiToFp False) (downcast a) (downcast x)
    FExt _ t x            -> LP.Conv LP.FpExt (downcast x) (downcast t)
    FPToInt _ b x         -> float2int b (downcast x)
    IntToFP a b x         -> int2float a b (downcast x)
    BitCast t x           -> LP.Conv LP.BitCast (downcast x) (downcast t)
    PtrCast t x           -> LP.Conv LP.BitCast (downcast x) (downcast t)
    Phi t e               -> LP.Phi (fmfFor t) (downcast t) (map (bimap (LP.typedValue . downcast) (LP.Named . labelToPrettyI)) e)
    Select t p x y        -> LP.Select (fmfFor t) (downcast p) (downcast x) (LP.typedValue (downcast y))
    IsNaN _ x             -> isNaN (downcast x)
    Cmp t p x y           -> cmp t p (downcast x) (downcast y)
    Call f                -> call f

    where
      nsw :: Bool       -- no signed wrap
      nsw = False

      nuw :: Bool       -- no unsigned wrap
      nuw = False

      exact :: Bool     -- does not lose any information
      exact = False

      fmf :: [LP.FMF]
      fmf = fastmathFlags

      inbounds :: [LP.GEPAttr]
      inbounds = [LP.GEP_Inbounds]

      atomicity :: Maybe LP.AtomicOrdering
      atomicity = Nothing

      alignment :: Maybe LP.Align
      alignment = Nothing  -- was: 0

      -- fmf :: LLVM.FastMathFlags
      -- fmf = LLVM.FastMathFlags
      --         { LLVM.allowReassoc    = True
      --         , LLVM.noNaNs          = True
      --         , LLVM.noInfs          = True
      --         , LLVM.noSignedZeros   = True
      --         , LLVM.allowReciprocal = True
      --         , LLVM.allowContract   = True
      --         , LLVM.approxFunc      = True
      --         }

      constant :: IsScalar a => a -> LP.Value
      constant x = LP.typedValue $ downcast (ConstantOperand (ScalarConstant scalarType x))

      add :: NumType a -> LP.Typed LP.Value -> LP.Typed LP.Value -> LP.Instr
      add IntegralNumType{} x (LP.Typed _ y) = LP.Arith (LP.Add nsw nuw) x y
      add FloatingNumType{} x (LP.Typed _ y) = LP.Arith (LP.FAdd fmf)    x y

      sub :: NumType a -> LP.Typed LP.Value -> LP.Typed LP.Value -> LP.Instr
      sub IntegralNumType{} x (LP.Typed _ y) = LP.Arith (LP.Sub nsw nuw) x y
      sub FloatingNumType{} x (LP.Typed _ y) = LP.Arith (LP.FSub fmf)    x y

      mul :: NumType a -> LP.Typed LP.Value -> LP.Typed LP.Value -> LP.Instr
      mul IntegralNumType{} x (LP.Typed _ y) = LP.Arith (LP.Mul nsw nuw) x y
      mul FloatingNumType{} x (LP.Typed _ y) = LP.Arith (LP.FMul fmf)    x y

      quot :: IntegralType a -> LP.Typed LP.Value -> LP.Typed LP.Value -> LP.Instr
      quot t x (LP.Typed _ y)
        | signed t  = LP.Arith (LP.SDiv exact) x y
        | otherwise = LP.Arith (LP.UDiv exact) x y

      rem :: IntegralType a -> LP.Typed LP.Value -> LP.Typed LP.Value -> LP.Instr
      rem t x (LP.Typed _ y)
        | signed t  = LP.Arith LP.SRem x y
        | otherwise = LP.Arith LP.URem x y

      extractStruct :: PairIdx s t -> LP.Typed LP.Value -> LP.Instr
      extractStruct i s = LP.ExtractValue s ix
        where
          ix = case i of
            PairIdxLeft  -> [0]
            PairIdxRight -> [1]

      ext :: BoundedType a -> BoundedType b -> LP.Typed LP.Value -> LP.Instr
      ext a (downcast -> b) x
        | signed a  = LP.Conv LP.SExt x b
        | otherwise = LP.Conv (LP.ZExt False) x b

      float2int :: IntegralType b -> LP.Typed LP.Value -> LP.Instr
      float2int t@(downcast -> t') x
        | signed t  = LP.Conv LP.FpToSi x t'
        | otherwise = LP.Conv LP.FpToUi x t'

      int2float :: IntegralType a -> FloatingType b -> LP.Typed LP.Value -> LP.Instr
      int2float a (downcast -> b) x
        | signed a  = LP.Conv LP.SiToFp x b
        | otherwise = LP.Conv (LP.UiToFp False) x b

      isNaN :: LP.Typed LP.Value -> LP.Instr
      isNaN x = LP.FCmp fmf LP.Funo x (LP.typedValue x)

      cmp :: SingleType a -> Ordering -> LP.Typed LP.Value -> LP.Typed LP.Value -> LP.Instr
      cmp t p x (LP.Typed _ y) =
        case t of
          NumSingleType FloatingNumType{} -> LP.FCmp fastmathFlags (fp p) x y
          _ | signed t                    -> LP.ICmp False (si p) x y
            | otherwise                   -> LP.ICmp False (ui p) x y
        where
          fp :: Ordering -> LP.FCmpOp
          fp EQ = LP.Foeq
          fp NE = LP.Fone
          fp LT = LP.Folt
          fp LE = LP.Fole
          fp GT = LP.Fogt
          fp GE = LP.Foge

          si :: Ordering -> LP.ICmpOp
          si EQ = LP.Ieq
          si NE = LP.Ine
          si LT = LP.Islt
          si LE = LP.Isle
          si GT = LP.Isgt
          si GE = LP.Isge

          ui :: Ordering -> LP.ICmpOp
          ui EQ = LP.Ieq
          ui NE = LP.Ine
          ui LT = LP.Iult
          ui LE = LP.Iule
          ui GT = LP.Iugt
          ui GE = LP.Iuge

      call :: Function (Either InlineAssembly Label) args t -> LP.Instr
      call f = LP.Call tail fmFlags fun_ty target argv
        where
          trav :: Function (Either InlineAssembly Label) args t
               -> ( [LP.Type]           -- argument types
                  , [LP.Typed LP.Value] -- argument operands
                  , Bool                -- tail call?
                  , LP.Type             -- return type
                  , [LP.FMF]            -- fast-math flags for this return type
                  , LP.Value            -- target: function name or inline assembly
                  )
          trav (Body u k o) =
            case o of
              Left _asm ->
                internalError
                  "Inline assembly should not be used as llvm-pretty does not \
                  \support it. For a workaround, see the solution for nanosleep \
                  \in Data.Array.Accelerate.LLVM.PTX.Compile."
              -- Left asm -> ([], [], downcast k, downcast u, Left  (downcast (LLVM.FunctionType ret argt False, asm)))
              Right n  -> ([], [], fromMaybe False (downcast k), downcast u, fmfFor u, LP.ValSymbol (labelToPrettyS n))
          trav (Lam t x l)  =
            let (ts, xs, k, r, fm, n) = trav l
            in  (downcast t : ts, downcast x : xs, k, r, fm, n)

          (argt, argv, tail, ret, fmFlags, target) = trav f
          fun_ty                                   = LP.FunTy ret argt False


-- instance Downcast (i a) i' => Downcast (Named i a) (LLVM.Named i') where
--   downcast (x := op) = downcast x LLVM.:= downcast op
--   downcast (Do op)   = LLVM.Do (downcast op)


instance TypeOf Instruction where
  typeOf = \case
    Add _ x _             -> typeOf x
    Sub _ x _             -> typeOf x
    Mul _ x _             -> typeOf x
    Quot _ x _            -> typeOf x
    Rem _ x _             -> typeOf x
    Div _ x _             -> typeOf x
    ShiftL _ x _          -> typeOf x
    ShiftRL _ x _         -> typeOf x
    ShiftRA _ x _         -> typeOf x
    BAnd _ x _            -> typeOf x
    BOr _ x _             -> typeOf x
    BXor _ x _            -> typeOf x
    LAnd x _              -> typeOf x
    LOr x _               -> typeOf x
    LNot x                -> typeOf x
    ExtractElement _ x    -> typeOfVec x
    InsertElement _ x _   -> typeOf x
    ExtractValue t _ _    -> scalar t
    Load t _ _            -> scalar t
    Store{}               -> VoidType
    GetElementPtr gep     -> typeOf gep
    Fence{}               -> VoidType
    CmpXchg t _ _ _ _ _ _ -> PrimType . StructPrimType False $ ScalarPrimType (SingleScalarType (NumSingleType (IntegralNumType t))) `pair` primType
    AtomicRMW _ _ _ _ x _ -> typeOf x
    FTrunc _ t _          -> floating t
    FExt _ t _            -> floating t
    Trunc _ t _           -> bounded t
    Ext _ t _             -> bounded t
    FPToInt _ t _         -> integral t
    IntToFP _ t _         -> floating t
    IntToBool _ _         -> type'
    BoolToInt t _         -> integral t
    BoolToFP t _          -> floating t
    BitCast t _           -> scalar t
    PtrCast t _           -> PrimType t
    Cmp{}                 -> type'
    IsNaN{}               -> type'
    Phi t _               -> PrimType t
    Select _ _ x _        -> typeOf x
    Call f                -> fun f
    where
      typeOfVec :: HasCallStack => Operand (Vec n a) -> Type a
      typeOfVec x
        | PrimType p          <- typeOf x
        , ScalarPrimType s    <- p
        , VectorScalarType v  <- s
        , VectorType _ t      <- v
        = PrimType (ScalarPrimType (SingleScalarType t))
        --
        | otherwise
        = internalError "unexpected evaluation"

      scalar :: ScalarType a -> Type a
      scalar = PrimType . ScalarPrimType

      single :: SingleType a -> Type a
      single = scalar . SingleScalarType

      floating :: FloatingType a -> Type a
      floating = single . NumSingleType . FloatingNumType

      integral :: IntegralType a -> Type a
      integral = single . NumSingleType . IntegralNumType

      pair :: PrimType a -> PrimType b -> TupR PrimType (a, b)
      pair a b = TupRsingle a `TupRpair` TupRsingle b

      bounded :: BoundedType a -> Type a
      bounded (IntegralBoundedType t) = integral t

      fun :: Function kind args a -> Type a
      fun (Lam _ _ l)  = fun l
      fun (Body t _ _) = t


{-# NOINLINE fmfEnabled #-}
fmfEnabled :: Bool
fmfEnabled = unsafePerformIO $ Debug.getFlag Debug.fast_math

fastmathFlags :: [LP.FMF]
fastmathFlags
  -- We explicitly exclude 'nnan' and 'ninf' from this list, because apparently
  -- we care about NaN and Inf values:
  --   <https://github.com/AccelerateHS/accelerate/issues/407>
  | fmfEnabled = [LP.Fnsz, LP.Farcp, LP.Fcontract, LP.Fafn, LP.Freassoc]
  | otherwise  = []

-- | Some LLVM instructions allow fast math flags only if their return type is
-- a "compatible floating-point type". This class returns fast-math flags given
-- such a return type.
class FmfFor s where
  fmfFor :: s a -> [LP.FMF]

instance FmfFor PrimType where
  fmfFor BoolPrimType = []
  fmfFor (ScalarPrimType t) = fmfFor t
  fmfFor PtrPrimType{} = []
  fmfFor (ArrayPrimType _ t) = fmfFor t
  fmfFor StructPrimType{} = []  -- TODO: homogeneous float structs are allowed by LLVM
  fmfFor NamedPrimType{} = []

instance FmfFor ScalarType where
  fmfFor (SingleScalarType st) = fmfFor st
  fmfFor (VectorScalarType (VectorType _ st)) = fmfFor st

instance FmfFor SingleType where
  fmfFor (NumSingleType (IntegralNumType _)) = []
  fmfFor (NumSingleType (FloatingNumType _)) = fastmathFlags

instance FmfFor Type where
  fmfFor VoidType = []
  fmfFor (PrimType t) = fmfFor t

