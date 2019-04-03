{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Downcast
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Downcast (

  Downcast(..)

) where

import Prelude                                                      hiding ( Ordering(..), const )
import Data.Bits
import Foreign.C.Types

import Data.Array.Accelerate.AST                                    ( tupleIdxToInt )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Type
import qualified Data.Array.Accelerate.LLVM.CodeGen.Constant        as Const

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Flags
import LLVM.AST.Type.Global
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Atomic
import LLVM.AST.Type.Instruction.Compare
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Metadata
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation
import LLVM.AST.Type.Terminator
import qualified LLVM.AST.Type.Instruction.RMW                      as RMW

import qualified LLVM.AST.Attribute                                 as L
import qualified LLVM.AST.AddrSpace                                 as L
import qualified LLVM.AST.CallingConvention                         as L
import qualified LLVM.AST.Constant                                  as LC
import qualified LLVM.AST.Float                                     as L
import qualified LLVM.AST.FloatingPointPredicate                    as FP
import qualified LLVM.AST.Global                                    as L
import qualified LLVM.AST.Instruction                               as L
import qualified LLVM.AST.IntegerPredicate                          as IP
import qualified LLVM.AST.Name                                      as L
import qualified LLVM.AST.Operand                                   as LO
import qualified LLVM.AST.RMWOperation                              as LA
import qualified LLVM.AST.Type                                      as L
#if MIN_VERSION_llvm_hs_pure(6,1,0)
#else
import qualified LLVM.AST.Type.Metadata                             as LM
#endif


-- | Convert a value from our representation of the LLVM AST which uses
-- Haskell-level types, into the llvm-general representation where types are
-- represented only at the value level. We use the type-level information to
-- generate the appropriate value-level types.
--
class Downcast typed untyped where
  downcast :: typed -> untyped

instance Downcast a a' => Downcast [a] [a'] where
  downcast = map downcast

instance Downcast a a' => Downcast (Maybe a) (Maybe a') where
  downcast Nothing  = Nothing
  downcast (Just x) = Just (downcast x)

instance (Downcast a a', Downcast b b') => Downcast (a,b) (a',b') where
  downcast (a,b) = (downcast a, downcast b)

instance (Downcast a a', Downcast b b') =>  Downcast (Either a b) (Either a' b') where
  downcast (Left a)  = Left (downcast a)
  downcast (Right b) = Right (downcast b)


-- LLVM.General.AST.Type.Flags
-- ---------------------------

nsw :: Bool
nsw = False

nuw :: Bool
nuw = False

fmf :: FastMathFlags
#if MIN_VERSION_llvm_hs_pure(6,0,0)
fmf = FastMathFlags
        { allowReassoc    = True
        , noNaNs          = True
        , noInfs          = True
        , noSignedZeros   = True
        , allowReciprocal = True
        , allowContract   = True
        , approxFunc      = True
        }
#else
fmf = UnsafeAlgebra -- allow everything
#endif

md :: L.InstructionMetadata
md = []


instance Downcast NUW Bool where
  downcast NoUnsignedWrap = True
  downcast UnsignedWrap   = False

instance Downcast NSW Bool where
  downcast NoSignedWrap = True
  downcast SignedWrap   = False

instance Downcast FastMathFlags L.FastMathFlags where
  downcast = id


-- LLVM.General.AST.Type.Name
-- --------------------------

instance Downcast (Name a) L.Name where
  downcast (Name s)   = L.Name s
  downcast (UnName n) = L.UnName n


-- LLVM.General.AST.Type.Instruction
-- ---------------------------------

tailcall :: Maybe L.TailCallKind
tailcall = Nothing

-- Instructions

instance Downcast (Instruction a) L.Instruction where
  downcast (Add t x y) =
    case t of
      IntegralNumType{}              -> L.Add nsw nuw (downcast x) (downcast y) md
      FloatingNumType{}              -> L.FAdd fmf    (downcast x) (downcast y) md
  downcast (Sub t x y) =
    case t of
      IntegralNumType{}              -> L.Sub nsw nuw (downcast x) (downcast y) md
      FloatingNumType{}              -> L.FSub fmf    (downcast x) (downcast y) md
  downcast (Mul t x y) =
    case t of
      IntegralNumType{}              -> L.Mul nsw nuw (downcast x) (downcast y) md
      FloatingNumType{}              -> L.FMul fmf    (downcast x) (downcast y) md
  downcast (Quot t x y)
    | signed t                        = L.SDiv False (downcast x) (downcast y) md
    | otherwise                       = L.UDiv False (downcast x) (downcast y) md
  downcast (Rem t x y)
    | signed t                        = L.SRem (downcast x) (downcast y) md
    | otherwise                       = L.URem (downcast x) (downcast y) md
  downcast (Div _ x y)                = L.FDiv fmf (downcast x) (downcast y) md
  downcast (ShiftL _ x i)             = L.Shl nsw nuw (downcast x) (downcast i) md
  downcast (ShiftRL _ x i)            = L.LShr False (downcast x) (downcast i) md
  downcast (ShiftRA _ x i)            = L.AShr False (downcast x) (downcast i) md
  downcast (BAnd _ x y)               = L.And (downcast x) (downcast y) md
  downcast (LAnd x y)                 = L.And (downcast x) (downcast y) md
  downcast (BOr _ x y)                = L.Or (downcast x) (downcast y) md
  downcast (LOr x y)                  = L.Or (downcast x) (downcast y) md
  downcast (BXor _ x y)               = L.Xor (downcast x) (downcast y) md
  downcast (LNot x)                   = L.Xor (downcast x) (downcast (Const.scalar scalarType True)) md

  downcast (ExtractElement v tix vec) = L.ExtractElement (downcast vec) (downcast tix') md
    where
      tix' :: Operand Int32
      tix' = Const.integral integralType $ sizeOfVec v - fromIntegral (tupleIdxToInt tix) - 1

      sizeOfVec :: VectorType v -> Int32
      sizeOfVec Vector2Type{}  = 2
      sizeOfVec Vector3Type{}  = 3
      sizeOfVec Vector4Type{}  = 4
      sizeOfVec Vector8Type{}  = 8
      sizeOfVec Vector16Type{} = 16

  downcast (InsertElement tix vec x)  = L.InsertElement (downcast vec) (downcast x) (downcast tix') md
    where
      tix' :: Operand Int32
      tix' = Const.integral integralType tix

  downcast (ExtractValue _ tix tup)   = L.ExtractValue (downcast tup) [fromIntegral $ sizeOfTuple - tupleIdxToInt tix - 1] md
    where
      sizeOfTuple
        | PrimType p       <- typeOf tup
        , StructPrimType t <- p
        = go t
        | otherwise
        = $internalError "downcast" "unexpected operand type to ExtractValue"
      --
      go :: TupleType t -> Int
      go (TypeRpair t _) = 1 + go t
      go _               = 0

  downcast (Load _ v p)             = L.Load (downcast v) (downcast p) Nothing 0 md
  downcast (Store v p x)            = L.Store (downcast v) (downcast p) (downcast x) Nothing 0 md
  downcast (GetElementPtr n i)      = L.GetElementPtr False (downcast n) (downcast i) md            -- TLM: in bounds??
  downcast (Fence a)                = L.Fence (downcast a) md
  downcast (CmpXchg _ v p x y a m)  = L.CmpXchg (downcast v) (downcast p) (downcast x) (downcast y) (downcast a) (downcast m) md
  downcast (AtomicRMW t v op p x a) = L.AtomicRMW (downcast v) (downcast (t,op)) (downcast p) (downcast x) (downcast a) md
  downcast (Trunc _ t x)            = L.Trunc (downcast x) (downcast t) md
  downcast (FTrunc _ t x)           = L.FPTrunc (downcast x) (downcast t) md
  downcast (Ext t t' x)
    | signed t                      = L.SExt (downcast x) (downcast t') md
    | otherwise                     = L.ZExt (downcast x) (downcast t') md
  downcast (FExt _ t x)             = L.FPExt (downcast x) (downcast t) md
  downcast (FPToInt _ t x)
    | signed t                      = L.FPToSI (downcast x) (downcast t) md
    | otherwise                     = L.FPToUI (downcast x) (downcast t) md
  downcast (IntToFP t t' x)
    | either signed signed t        = L.SIToFP (downcast x) (downcast t') md
    | otherwise                     = L.UIToFP (downcast x) (downcast t') md
  downcast (BitCast t x)            = L.BitCast (downcast x) (downcast t) md
  downcast (PtrCast t x)            = L.BitCast (downcast x) (downcast t) md
  downcast (Phi t incoming)         = L.Phi (downcast t) (downcast incoming) md
  downcast (Select _ p x y)         = L.Select (downcast p) (downcast x) (downcast y) md
  downcast (Call f attrs)           = L.Call tailcall L.C [] (downcast f) (downcast f) (downcast attrs) md
  downcast (FCmp _ p x y)           =
    let
        fp UNO = FP.UNO
        fp OEQ = FP.OEQ
    in
    L.FCmp (fp p) (downcast x) (downcast y) md

  downcast (Cmp t p x y)            =
    let
        fp EQ = FP.OEQ
        fp NE = FP.ONE
        fp LT = FP.OLT
        fp LE = FP.OLE
        fp GT = FP.OGT
        fp GE = FP.OGE
        --
        si EQ = IP.EQ
        si NE = IP.NE
        si LT = IP.SLT
        si LE = IP.SLE
        si GT = IP.SGT
        si GE = IP.SGE
        --
        ui EQ = IP.EQ
        ui NE = IP.NE
        ui LT = IP.ULT
        ui LE = IP.ULE
        ui GT = IP.UGT
        ui GE = IP.UGE
    in
    case t of
      NumSingleType FloatingNumType{} -> L.FCmp (fp p) (downcast x) (downcast y) md
      _ | signed t                    -> L.ICmp (si p) (downcast x) (downcast y) md
        | otherwise                   -> L.ICmp (ui p) (downcast x) (downcast y) md

instance Downcast Volatility Bool where
  downcast Volatile    = True
  downcast NonVolatile = False

instance Downcast Synchronisation L.SynchronizationScope where
  downcast SingleThread = L.SingleThread
#if MIN_VERSION_llvm_hs_pure(5,0,0)
  downcast CrossThread  = L.System
#else
  downcast CrossThread  = L.CrossThread
#endif

instance Downcast MemoryOrdering L.MemoryOrdering where
  downcast Unordered              = L.Unordered
  downcast Monotonic              = L.Monotonic
  downcast Acquire                = L.Acquire
  downcast Release                = L.Release
  downcast AcquireRelease         = L.AcquireRelease
  downcast SequentiallyConsistent = L.SequentiallyConsistent

instance Downcast (IntegralType t, RMW.RMWOperation) LA.RMWOperation where
  downcast (_, RMW.Exchange)  = LA.Xchg
  downcast (_, RMW.Add)       = LA.Add
  downcast (_, RMW.Sub)       = LA.Sub
  downcast (_, RMW.And)       = LA.And
  downcast (_, RMW.Or)        = LA.Or
  downcast (_, RMW.Xor)       = LA.Xor
  downcast (_, RMW.Nand)      = LA.Nand
  downcast (t, RMW.Min)
    | signed t                = LA.Min
    | otherwise               = LA.UMin
  downcast (t, RMW.Max)
    | signed t                = LA.Max
    | otherwise               = LA.UMax

instance (Downcast (i a) i') => Downcast (Named i a) (L.Named i') where
  downcast (x := op) = downcast x L.:= downcast op
  downcast (Do op)   = L.Do (downcast op)

instance Downcast a b => Downcast (L.Named a) (L.Named b) where
  downcast (l L.:= r)   = l L.:= downcast r
  downcast (L.Do x)     = L.Do (downcast x)


-- LLVM.General.AST.Type.Constant
-- ------------------------------

instance Downcast (Constant a) LC.Constant where
  downcast (UndefConstant t)      = LC.Undef (downcast t)
  downcast (GlobalReference t n)  = LC.GlobalReference (downcast t) (downcast n)
  downcast (ScalarConstant t x)   = scalar t x
    where
      scalar :: ScalarType s -> s -> LC.Constant
      scalar (SingleScalarType s) = single s
      scalar (VectorScalarType s) = vector s

      single :: SingleType s -> s -> LC.Constant
      single (NumSingleType s)    = num s
      single (NonNumSingleType s) = nonnum s

      vector :: VectorType s -> s -> LC.Constant
      vector (Vector2Type s) (V2 a b)     = LC.Vector [single s a, single s b]
      vector (Vector3Type s) (V3 a b c)   = LC.Vector [single s a, single s b, single s c]
      vector (Vector4Type s) (V4 a b c d) = LC.Vector [single s a, single s b, single s c, single s d]
      vector (Vector8Type s) (V8 a b c d e f g h) =
        LC.Vector [ single s a, single s b, single s c, single s d
                  , single s e, single s f, single s g, single s h ]
      vector (Vector16Type s) (V16 a b c d e f g h i j k l m n o p) =
        LC.Vector [ single s a, single s b, single s c, single s d
                  , single s e, single s f, single s g, single s h
                  , single s i, single s j, single s k, single s l
                  , single s m, single s n, single s o, single s p ]

      num :: NumType s -> s -> LC.Constant
      num (IntegralNumType s) v
        | IntegralDict <- integralDict s
        = LC.Int (L.typeBits (downcast s)) (fromIntegral v)
      num (FloatingNumType s) v
        = LC.Float
        $ case s of
            TypeFloat{}                            -> L.Single v
            TypeDouble{}                           -> L.Double v
            TypeCDouble{} | CDouble u <- v         -> L.Double u
            TypeCFloat{}  | CFloat u <- v          -> L.Single u
            TypeHalf{}    | Half (CUShort u) <- v  -> L.Half u

      nonnum :: NonNumType s -> s -> LC.Constant
      nonnum s v
        = LC.Int (L.typeBits (downcast s))
        $ case s of
            TypeBool{}   -> fromIntegral (fromEnum v)
            TypeChar{}   -> fromIntegral (fromEnum v)
            TypeCChar{}  -> fromIntegral (fromEnum v)
            TypeCUChar{} -> fromIntegral (fromEnum v)
            TypeCSChar{} -> fromIntegral (fromEnum v)


-- LLVM.General.AST.Type.Operand
-- -----------------------------

instance Downcast (Operand a) LO.Operand where
  downcast (LocalReference t n) = LO.LocalReference (downcast t) (downcast n)
  downcast (ConstantOperand c)  = LO.ConstantOperand (downcast c)


-- LLVM.General.AST.Type.Metadata
-- ------------------------------

instance Downcast Metadata LO.Operand where
  downcast = LO.MetadataOperand . downcast

instance Downcast Metadata LO.Metadata where
  downcast (MetadataStringOperand s)   = LO.MDString s
  downcast (MetadataConstantOperand o) = LO.MDValue (LO.ConstantOperand o)
  downcast (MetadataNodeOperand n)     = LO.MDNode (downcast n)

#if MIN_VERSION_llvm_hs_pure(6,1,0)
instance Downcast MetadataNode (LO.MDRef LO.MDNode) where
  downcast (MetadataNode n)            = LO.MDInline (downcast n)
  downcast (MetadataNodeReference r)   = LO.MDRef r

instance Downcast [Maybe Metadata] LO.MDNode where
  downcast = LO.MDTuple . map downcast
#else
instance Downcast MetadataNode LM.MetadataNode where
  downcast (MetadataNode n)          = LM.MetadataNode (downcast n)
  downcast (MetadataNodeReference r) = LM.MetadataNodeReference r
#endif



-- LLVM.General.AST.Type.Terminator
-- --------------------------------

instance Downcast (Terminator a) L.Terminator where
  downcast Ret            = L.Ret Nothing md
  downcast (RetVal x)     = L.Ret (Just (downcast x)) md
  downcast (Br l)         = L.Br (downcast l) md
  downcast (CondBr p t f) = L.CondBr (downcast p) (downcast t) (downcast f) md
  downcast (Switch p d a) = L.Switch (downcast p) (downcast d) (downcast a) md


-- LLVM.General.AST.Type.Name
-- --------------------------

instance Downcast Label L.Name where
  downcast (Label l) = L.Name l


-- LLVM.General.AST.Type.Global
-- ----------------------------

instance Downcast (Parameter a) L.Parameter where
  downcast (Parameter t x) = L.Parameter (downcast t) (downcast x) attrs
    where
      attrs | PtrPrimType{} <- t = [L.NoAlias, L.NoCapture] -- TLM: alignment?
            | otherwise          = []

-- Function -> callable operands (for Call instruction)
--
instance Downcast (GlobalFunction args t) LO.CallableOperand where
  downcast f
    = let trav :: GlobalFunction args t -> ([L.Type], L.Type, L.Name)
          trav (Body t n)  = ([], downcast t, downcast n)
          trav (Lam t _ l) = let (t',r, n) = trav l
                             in  (downcast t : t', r, n)

          (args, result, name)  = trav f
          ty                    = L.PointerType (L.FunctionType result args False) (L.AddrSpace 0)
      in
      Right (LO.ConstantOperand (LC.GlobalReference ty name))

instance Downcast (GlobalFunction args t) [(LO.Operand, [L.ParameterAttribute])] where
  downcast Body{}      = []
  downcast (Lam _ x l) = (downcast x, []) : downcast l

-- Function -> global declaration
--
instance Downcast (GlobalFunction args t) L.Global where
  downcast f
    = let trav :: GlobalFunction args t -> ([L.Type], L.Type, L.Name)
          trav (Body t n)  = ([], downcast t, downcast n)
          trav (Lam t _ l) = let (t',r, n) = trav l
                             in  (downcast t : t', r, n)

          (args, result, name)  = trav f
          params                = [ L.Parameter t (L.UnName i) [] | t <- args | i <- [0..] ]
      in
      L.functionDefaults { L.name       = name
                         , L.returnType = result
                         , L.parameters = (params,False)
                         }

instance Downcast FunctionAttribute L.FunctionAttribute where
  downcast NoReturn     = L.NoReturn
  downcast NoUnwind     = L.NoUnwind
  downcast ReadOnly     = L.ReadOnly
  downcast ReadNone     = L.ReadNone
  downcast AlwaysInline = L.AlwaysInline
  downcast NoDuplicate  = L.NoDuplicate
  downcast Convergent   = L.Convergent

instance Downcast GroupID L.GroupID where
  downcast (GroupID n) = L.GroupID n


-- LLVM.General.AST.Type.Representation
-- ------------------------------------

instance Downcast (Type a) L.Type where
  downcast VoidType     = L.VoidType
  downcast (PrimType t) = downcast t

instance Downcast (PrimType a) L.Type where
  downcast (ScalarPrimType t)   = downcast t
  downcast (PtrPrimType t a)    = L.PointerType (downcast t) a
  downcast (ArrayPrimType n t)  = L.ArrayType n (downcast t)
  downcast (StructPrimType t)   = L.StructureType False (go t)
    where
      go :: TupleType t -> [L.Type]
      go TypeRunit         = []
      go (TypeRscalar s)   = [downcast s]
      go (TypeRpair ta tb) = go ta ++ go tb

-- Data.Array.Accelerate.Type
-- --------------------------

instance Downcast (ScalarType a) L.Type where
  downcast (SingleScalarType t) = downcast t
  downcast (VectorScalarType t) = downcast t

instance Downcast (SingleType a) L.Type where
  downcast (NumSingleType t)    = downcast t
  downcast (NonNumSingleType t) = downcast t

instance Downcast (VectorType a) L.Type where
  downcast (Vector2Type t)  = L.VectorType 2 (downcast t)
  downcast (Vector3Type t)  = L.VectorType 3 (downcast t)
  downcast (Vector4Type t)  = L.VectorType 4 (downcast t)
  downcast (Vector8Type t)  = L.VectorType 8 (downcast t)
  downcast (Vector16Type t) = L.VectorType 16 (downcast t)

instance Downcast (BoundedType t) L.Type where
  downcast (IntegralBoundedType t) = downcast t
  downcast (NonNumBoundedType t)   = downcast t

instance Downcast (NumType a) L.Type where
  downcast (IntegralNumType t) = downcast t
  downcast (FloatingNumType t) = downcast t

instance Downcast (IntegralType a) L.Type where
  downcast TypeInt{}     = L.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: Int)) |] )
  downcast TypeInt8{}    = L.IntegerType 8
  downcast TypeInt16{}   = L.IntegerType 16
  downcast TypeInt32{}   = L.IntegerType 32
  downcast TypeInt64{}   = L.IntegerType 64
  downcast TypeWord{}    = L.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: Word)) |] )
  downcast TypeWord8{}   = L.IntegerType 8
  downcast TypeWord16{}  = L.IntegerType 16
  downcast TypeWord32{}  = L.IntegerType 32
  downcast TypeWord64{}  = L.IntegerType 64
  downcast TypeCShort{}  = L.IntegerType 16
  downcast TypeCUShort{} = L.IntegerType 16
  downcast TypeCInt{}    = L.IntegerType 32
  downcast TypeCUInt{}   = L.IntegerType 32
  downcast TypeCLong{}   = L.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: CLong)) |] )
  downcast TypeCULong{}  = L.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: CULong)) |] )
  downcast TypeCLLong{}  = L.IntegerType 64
  downcast TypeCULLong{} = L.IntegerType 64

instance Downcast (FloatingType a) L.Type where
  downcast TypeHalf{}    = L.FloatingPointType L.HalfFP
  downcast TypeFloat{}   = L.FloatingPointType L.FloatFP
  downcast TypeDouble{}  = L.FloatingPointType L.DoubleFP
  downcast TypeCFloat{}  = L.FloatingPointType L.FloatFP
  downcast TypeCDouble{} = L.FloatingPointType L.DoubleFP

instance Downcast (NonNumType a) L.Type where
  downcast TypeBool{}   = L.IntegerType 1
  downcast TypeChar{}   = L.IntegerType 32
  downcast TypeCChar{}  = L.IntegerType 8
  downcast TypeCSChar{} = L.IntegerType 8
  downcast TypeCUChar{} = L.IntegerType 8

