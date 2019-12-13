{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Instruction
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Instruction
  where

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Global
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import LLVM.AST.Type.Instruction.Atomic                   ( Atomicity, MemoryOrdering )
import LLVM.AST.Type.Instruction.Compare                  ( Ordering(..) )
import LLVM.AST.Type.Instruction.RMW                      ( RMWOperation )
import LLVM.AST.Type.Instruction.Volatile                 ( Volatility )

import qualified LLVM.AST.Constant                        as LLVM ( Constant(GlobalReference) )
import qualified LLVM.AST.AddrSpace                       as LLVM
import qualified LLVM.AST.CallingConvention               as LLVM
import qualified LLVM.AST.FloatingPointPredicate          as FP
import qualified LLVM.AST.Instruction                     as LLVM
import qualified LLVM.AST.IntegerPredicate                as IP
import qualified LLVM.AST.Name                            as LLVM
import qualified LLVM.AST.Operand                         as LLVM ( Operand(..) )
import qualified LLVM.AST.ParameterAttribute              as LLVM ( ParameterAttribute )
import qualified LLVM.AST.Type                            as LLVM ( Type(..) )

import Data.Array.Accelerate.AST                          ( tupleIdxToInt )
import Data.Array.Accelerate.Product                      ( ProdRepr, TupleIdx )
import Data.Array.Accelerate.Error

import Prelude                                            hiding ( Ordering(..), quot, rem, div, isNaN )


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
  --
  ExtractValue    :: ScalarType t
                  -> TupleIdx (ProdRepr tup) t
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
  GetElementPtr   :: Operand (Ptr a)
                  -> [Operand i]
                  -> Instruction (Ptr a)

  -- <http://llvm.org/docs/LangRef.html#i-fence>
  --
  Fence           :: Atomicity
                  -> Instruction ()

  -- <http://llvm.org/docs/LangRef.html#cmpxchg-instruction>
  --
  CmpXchg         :: IntegralType a
                  -> Volatility
                  -> Operand (Ptr a)
                  -> Operand a              -- expected value
                  -> Operand a              -- replacement value
                  -> Atomicity              -- on success
                  -> MemoryOrdering         -- on failure (see docs for restrictions)
                  -> Instruction (a, Bool)

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

  -- <http://llvm.org/docs/LangRef.html#fpext-to-instruction>
  --
  FExt            :: FloatingType a       -- precondition: BitSize a < BitSize b
                  -> FloatingType b
                  -> Operand a
                  -> Instruction b

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
  IntToFP         :: Either (IntegralType a) (NonNumType a)
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
  Call            :: GlobalFunction args t
                  -> [Either GroupID FunctionAttribute]
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


-- | Convert to llvm-hs
--
instance Downcast (Instruction a) LLVM.Instruction where
  downcast = \case
    Add t x y             -> add t (downcast x) (downcast y)
    Sub t x y             -> sub t (downcast x) (downcast y)
    Mul t x y             -> mul t (downcast x) (downcast y)
    Quot t x y            -> quot t (downcast x) (downcast y)
    Rem t x y             -> rem t (downcast x) (downcast y)
    Div _ x y             -> LLVM.FDiv fmf (downcast x) (downcast y) md
    ShiftL _ x i          -> LLVM.Shl nsw nuw (downcast x) (downcast i) md
    ShiftRL _ x i         -> LLVM.LShr exact (downcast x) (downcast i) md
    ShiftRA _ x i         -> LLVM.AShr exact (downcast x) (downcast i) md
    BAnd _ x y            -> LLVM.And (downcast x) (downcast y) md
    LAnd x y              -> LLVM.And (downcast x) (downcast y) md
    BOr _ x y             -> LLVM.Or (downcast x) (downcast y) md
    LOr x y               -> LLVM.Or (downcast x) (downcast y) md
    BXor _ x y            -> LLVM.Xor (downcast x) (downcast y) md
    LNot x                -> LLVM.Xor (downcast x) (constant True) md
    InsertElement i v x   -> LLVM.InsertElement (downcast v) (downcast x) (constant i) md
    ExtractElement i v    -> LLVM.ExtractElement (downcast v) (constant i) md
    ExtractValue _ i s    -> extractStruct (typeOf s) i (downcast s)
    Load _ v p            -> LLVM.Load (downcast v) (downcast p) atomicity alignment md
    Store v p x           -> LLVM.Store (downcast v) (downcast p) (downcast x) atomicity alignment md
    GetElementPtr n i     -> LLVM.GetElementPtr inbounds (downcast n) (downcast i) md
    Fence a               -> LLVM.Fence (downcast a) md
    CmpXchg _ v p x y a m -> LLVM.CmpXchg (downcast v) (downcast p) (downcast x) (downcast y) (downcast a) (downcast m) md
    AtomicRMW t v f p x a -> LLVM.AtomicRMW (downcast v) (downcast (t,f)) (downcast p) (downcast x) (downcast a) md
    Trunc _ t x           -> LLVM.Trunc (downcast x) (downcast t) md
    FTrunc _ t x          -> LLVM.FPTrunc (downcast x) (downcast t) md
    Ext a b x             -> ext a b (downcast x)
    FExt _ t x            -> LLVM.FPExt (downcast x) (downcast t) md
    FPToInt _ b x         -> float2int b (downcast x)
    IntToFP a b x         -> int2float a b (downcast x)
    BitCast t x           -> LLVM.BitCast (downcast x) (downcast t) md
    PtrCast t x           -> LLVM.BitCast (downcast x) (downcast t) md
    Phi t e               -> LLVM.Phi (downcast t) (downcast e) md
    Select _ p x y        -> LLVM.Select (downcast p) (downcast x) (downcast y) md
    IsNaN _ x             -> isNaN (downcast x)
    Cmp t p x y           -> cmp t p (downcast x) (downcast y)
    Call f a              -> call f a

    where
      nsw :: Bool       -- no signed wrap
      nsw = False

      nuw :: Bool       -- no unsigned wrap
      nuw = False

      exact :: Bool     -- does not lose any information
      exact = False

      inbounds :: Bool
      inbounds = True

      atomicity :: Maybe LLVM.Atomicity
      atomicity = Nothing

      alignment :: Word32
      alignment = 0

      fmf :: LLVM.FastMathFlags
#if MIN_VERSION_llvm_hs_pure(6,0,0)
      fmf = LLVM.FastMathFlags
              { LLVM.allowReassoc    = True
              , LLVM.noNaNs          = True
              , LLVM.noInfs          = True
              , LLVM.noSignedZeros   = True
              , LLVM.allowReciprocal = True
              , LLVM.allowContract   = True
              , LLVM.approxFunc      = True
              }
#else
      fmf = LLVM.UnsafeAlgebra -- allow everything
#endif

      md :: LLVM.InstructionMetadata
      md = []

      constant :: IsScalar a => a -> LLVM.Operand
      constant x = downcast (ConstantOperand (ScalarConstant scalarType x))

      add :: NumType a -> LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
      add IntegralNumType{} x y = LLVM.Add nsw nuw x y md
      add FloatingNumType{} x y = LLVM.FAdd fmf    x y md

      sub :: NumType a -> LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
      sub IntegralNumType{} x y = LLVM.Sub nsw nuw x y md
      sub FloatingNumType{} x y = LLVM.FSub fmf    x y md

      mul :: NumType a -> LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
      mul IntegralNumType{} x y = LLVM.Mul nsw nuw x y md
      mul FloatingNumType{} x y = LLVM.FMul fmf    x y md

      quot :: IntegralType a -> LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
      quot t x y
        | signed t  = LLVM.SDiv exact x y md
        | otherwise = LLVM.UDiv exact x y md

      rem :: IntegralType a -> LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
      rem t x y
        | signed t  = LLVM.SRem x y md
        | otherwise = LLVM.URem x y md

      extractStruct :: Type s -> TupleIdx (ProdRepr s) t -> LLVM.Operand -> LLVM.Instruction
      extractStruct t i s = LLVM.ExtractValue s [fromIntegral (size t - tupleIdxToInt i - 1)] md
        where
          size (PrimType (StructPrimType u)) = go u
          size _                             = $internalError "downcast" "unexpected operand type to ExtractValue"

          go :: TupleType t -> Int
          go (TypeRpair u _) = 1 + go u
          go _               = 0

      ext :: BoundedType a -> BoundedType b -> LLVM.Operand -> LLVM.Instruction
      ext a (downcast -> b) x
        | signed a  = LLVM.SExt x b md
        | otherwise = LLVM.ZExt x b md

      float2int :: IntegralType b -> LLVM.Operand -> LLVM.Instruction
      float2int t@(downcast -> t') x
        | signed t  = LLVM.FPToSI x t' md
        | otherwise = LLVM.FPToUI x t' md

      int2float :: Either (IntegralType a) (NonNumType a) -> FloatingType b -> LLVM.Operand -> LLVM.Instruction
      int2float a (downcast -> b) x
        | either signed signed a = LLVM.SIToFP x b md
        | otherwise              = LLVM.UIToFP x b md

      isNaN :: LLVM.Operand -> LLVM.Instruction
      isNaN x = LLVM.FCmp FP.UNO x x md

      cmp :: SingleType a -> Ordering -> LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
      cmp t p x y =
        case t of
          NumSingleType FloatingNumType{} -> LLVM.FCmp (fp p) x y md
          _ | signed t                    -> LLVM.ICmp (si p) x y md
            | otherwise                   -> LLVM.ICmp (ui p) x y md
        where
          fp :: Ordering -> FP.FloatingPointPredicate
          fp EQ = FP.OEQ
          fp NE = FP.ONE
          fp LT = FP.OLT
          fp LE = FP.OLE
          fp GT = FP.OGT
          fp GE = FP.OGE

          si :: Ordering -> IP.IntegerPredicate
          si EQ = IP.EQ
          si NE = IP.NE
          si LT = IP.SLT
          si LE = IP.SLE
          si GT = IP.SGT
          si GE = IP.SGE

          ui :: Ordering -> IP.IntegerPredicate
          ui EQ = IP.EQ
          ui NE = IP.NE
          ui LT = IP.ULT
          ui LE = IP.ULE
          ui GT = IP.UGT
          ui GE = IP.UGE

      call :: GlobalFunction args t -> [Either GroupID FunctionAttribute] -> LLVM.Instruction
      call f as = LLVM.Call Nothing LLVM.C [] (Right (LLVM.ConstantOperand (LLVM.GlobalReference funt name))) argv (downcast as) md
        where
          trav :: GlobalFunction args t
               -> ( [LLVM.Type]                                 -- argument types
                  , [(LLVM.Operand, [LLVM.ParameterAttribute])] -- argument operands
                  , LLVM.Type                                   -- return type
                  , LLVM.Name                                   -- function name
                  )
          trav (Body u n)   = ([], [], downcast u, downcast n)
          trav (Lam t x l)  =
            let (ts, xs, r, n)  = trav l
            in  (downcast t : ts, (downcast x, []) : xs, r, n)

          (argt, argv, ret, name) = trav f
          funt                    = LLVM.PointerType (LLVM.FunctionType ret argt False) (LLVM.AddrSpace 0)


instance Downcast (i a) i' => Downcast (Named i a) (LLVM.Named i') where
  downcast (x := op) = downcast x LLVM.:= downcast op
  downcast (Do op)   = LLVM.Do (downcast op)


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
    GetElementPtr x _     -> typeOf x
    Fence{}               -> VoidType
    CmpXchg t _ _ _ _ _ _ -> PrimType . StructPrimType $ SingleScalarType (NumSingleType (IntegralNumType t)) `pair` scalarType
    AtomicRMW _ _ _ _ x _ -> typeOf x
    FTrunc _ t _          -> floating t
    FExt _ t _            -> floating t
    Trunc _ t _           -> bounded t
    Ext _ t _             -> bounded t
    FPToInt _ t _         -> integral t
    IntToFP _ t _         -> floating t
    BitCast t _           -> scalar t
    PtrCast t _           -> PrimType t
    Cmp{}                 -> type'
    IsNaN{}               -> type'
    Phi t _               -> PrimType t
    Select _ _ x _        -> typeOf x
    Call f _              -> fun f
    where
      typeOfVec :: Operand (Vec n a) -> Type a
      typeOfVec x
        | PrimType p          <- typeOf x
        , ScalarPrimType s    <- p
        , VectorScalarType v  <- s
        , VectorType _ t      <- v
        = PrimType (ScalarPrimType (SingleScalarType t))
        --
        | otherwise
        = $internalError "typeOfVec" "unexpected evaluation"

      scalar :: ScalarType a -> Type a
      scalar = PrimType . ScalarPrimType

      single :: SingleType a -> Type a
      single = scalar . SingleScalarType

      floating :: FloatingType a -> Type a
      floating = single . NumSingleType . FloatingNumType

      integral :: IntegralType a -> Type a
      integral = single . NumSingleType . IntegralNumType

      nonnum :: NonNumType a -> Type a
      nonnum = single . NonNumSingleType

      pair :: ScalarType a -> ScalarType b -> TupleType (((), a), b)
      pair a b = TypeRunit `TypeRpair` TypeRscalar a `TypeRpair` TypeRscalar b

      bounded :: BoundedType a -> Type a
      bounded (IntegralBoundedType t) = integral t
      bounded (NonNumBoundedType   t) = nonnum t

      fun :: GlobalFunction args a -> Type a
      fun (Lam _ _ l) = fun l
      fun (Body t _)  = t

