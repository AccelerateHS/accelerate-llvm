{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Downcast
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Downcast (

  Downcast(..)

) where

import Prelude                                                  hiding ( Ordering(..) )
import Data.Bits
import Foreign.C.Types

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.LLVM.CodeGen.Type

import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Flags
import LLVM.General.AST.Type.Global
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Metadata
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Terminator

import qualified LLVM.General.AST.AddrSpace                     as L
import qualified LLVM.General.AST.Attribute                     as L
import qualified LLVM.General.AST.CallingConvention             as L
import qualified LLVM.General.AST.Constant                      as LC
import qualified LLVM.General.AST.Float                         as L
import qualified LLVM.General.AST.FloatingPointPredicate        as FP
import qualified LLVM.General.AST.Global                        as L
import qualified LLVM.General.AST.Instruction                   as L
import qualified LLVM.General.AST.IntegerPredicate              as IP
import qualified LLVM.General.AST.Name                          as L
import qualified LLVM.General.AST.Operand                       as L
import qualified LLVM.General.AST.Type                          as L


-- | Convert a value from our representation of the LLVM AST which uses
-- Haskell-level types, into the llvm-general representation where types are
-- represented only at the value level. We use the type-level information to
-- generate the appropriate value-level types.
--
class Downcast a b where
  downcast :: a -> b

instance Downcast a a' => Downcast [a] [a'] where
  downcast = map downcast

instance Downcast a a' => Downcast (Maybe a) (Maybe a') where
  downcast Nothing  = Nothing
  downcast (Just x) = Just (downcast x)

instance (Downcast a a', Downcast b b') => Downcast (a,b) (a',b') where
  downcast (a,b) = (downcast a, downcast b)

nsw :: Bool
nsw = False

nuw :: Bool
nuw = False

fmf :: FastMathFlags
fmf = UnsafeAlgebra

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

instance Downcast (Name a) L.Name where
  downcast (Name s)   = L.Name s
  downcast (UnName n) = L.UnName n

instance (Downcast (i a) i') => Downcast (Named i a) (L.Named i') where
  downcast (x := op) = downcast x L.:= downcast op
  downcast (Do op)   = L.Do (downcast op)

instance Downcast (Instruction a) L.Instruction where
  downcast (Add t x y) =
    case t of
      IntegralNumType{}        -> L.Add nsw nuw (downcast x) (downcast y) md
      FloatingNumType{}        -> L.FAdd fmf    (downcast x) (downcast y) md
  downcast (Sub t x y) =
    case t of
      IntegralNumType{}        -> L.Sub nsw nuw (downcast x) (downcast y) md
      FloatingNumType{}        -> L.FSub fmf    (downcast x) (downcast y) md
  downcast (Mul t x y) =
    case t of
      IntegralNumType{}        -> L.Mul nsw nuw (downcast x) (downcast y) md
      FloatingNumType{}        -> L.FMul fmf    (downcast x) (downcast y) md
  downcast (Quot t x y)
    | signed t                  = L.SDiv False (downcast x) (downcast y) md
    | otherwise                 = L.UDiv False (downcast x) (downcast y) md
  downcast (Rem t x y)
    | signed t                  = L.SRem (downcast x) (downcast y) md
    | otherwise                 = L.URem (downcast x) (downcast y) md
  downcast (Div _ x y)          = L.FDiv fmf (downcast x) (downcast y) md
  downcast (ShiftL _ x i)       = L.Shl nsw nuw (downcast x) (downcast i) md
  downcast (ShiftRL _ x i)      = L.LShr False (downcast x) (downcast i) md
  downcast (ShiftRA _ x i)      = L.AShr False (downcast x) (downcast i) md
  downcast (BAnd _ x y)         = L.And (downcast x) (downcast y) md
  downcast (LAnd x y)           = L.And (downcast x) (downcast y) md
  downcast (BOr _ x y)          = L.Or (downcast x) (downcast y) md
  downcast (LOr x y)            = L.Or (downcast x) (downcast y) md
  downcast (BXor _ x y)         = L.Xor (downcast x) (downcast y) md
  downcast (LNot x)             = L.Xor (downcast x) (downcast (scalar scalarType True)) md
  downcast (Load _ v p)         = L.Load (downcast v) (downcast p) Nothing 0 md
  downcast (Store v p x)        = L.Store (downcast v) (downcast p) (downcast x) Nothing 0 md
  downcast (GetElementPtr n i)  = L.GetElementPtr False (downcast n) (downcast i) md            -- in bounds??
  downcast (Trunc _ t x)        = L.Trunc (downcast x) (downcast t) md
  downcast (FTrunc _ t x)       = L.FPTrunc (downcast x) (downcast t) md
  downcast (Ext _ t x)
    | signed t                  = L.SExt (downcast x) (downcast t) md
    | otherwise                 = L.ZExt (downcast x) (downcast t) md
  downcast (FExt _ t x)         = L.FPExt (downcast x) (downcast t) md
  downcast (FPToInt _ t x)
    | signed t                  = L.FPToSI (downcast x) (downcast t) md
    | otherwise                 = L.FPToUI (downcast x) (downcast t) md
  downcast (IntToFP t t' x)
    | signed t                  = L.SIToFP (downcast x) (downcast t') md
    | otherwise                 = L.UIToFP (downcast x) (downcast t') md
  downcast (BitCast t x)        = L.BitCast (downcast x) (downcast t) md
  downcast (Phi t incoming)     = L.Phi (downcast t) (downcast incoming) md
  downcast (Select _ p x y)     = L.Select (downcast p) (downcast x) (downcast y) md
  downcast (Call f attrs)       = L.Call False L.C [] (downcast f) (downcast f) (downcast attrs) md
  downcast (Cmp t p x y) =
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
      NumScalarType FloatingNumType{}   -> L.FCmp (fp p) (downcast x) (downcast y) md
      _ | signed t                      -> L.ICmp (si p) (downcast x) (downcast y) md
        | otherwise                     -> L.ICmp (ui p) (downcast x) (downcast y) md

instance Downcast (Constant a) LC.Constant where
  downcast (ScalarConstant (NumScalarType (IntegralNumType t)) x)
    | IntegralDict <- integralDict t
    = LC.Int (L.typeBits (downcast t)) (fromIntegral x)

  downcast (ScalarConstant (NumScalarType (FloatingNumType t)) x)
    = LC.Float
    $ case t of
        TypeFloat{}   -> L.Single x
        TypeDouble{}  -> L.Double x
        TypeCFloat{}  -> L.Single $ case x of CFloat x' -> x'
        TypeCDouble{} -> L.Double $ case x of CDouble x' -> x'

  downcast (ScalarConstant (NonNumScalarType t) x)
    = LC.Int (L.typeBits (downcast t))
    $ case t of
        TypeBool{}      -> fromIntegral (fromEnum x)
        TypeChar{}      -> fromIntegral (fromEnum x)
        TypeCChar{}     -> fromIntegral (fromEnum x)
        TypeCUChar{}    -> fromIntegral (fromEnum x)
        TypeCSChar{}    -> fromIntegral (fromEnum x)

  downcast (GlobalReference t n)
    = LC.GlobalReference (downcast t) (downcast n)

instance Downcast (Operand a) L.Operand where
  downcast (LocalReference t n)      = L.LocalReference (downcast t) (downcast n)
  downcast (ConstantOperand c)       = L.ConstantOperand (downcast c)

instance Downcast Metadata L.Operand where
  downcast (MetadataStringOperand s) = L.MetadataStringOperand s
  downcast (MetadataOperand o)       = downcast o
  downcast (MetadataNodeOperand n)   = L.MetadataNodeOperand (downcast n)

instance Downcast MetadataNode L.MetadataNode where
  downcast (MetadataNode n)          = L.MetadataNode (downcast n)
  downcast (MetadataNodeReference r) = L.MetadataNodeReference r

instance Downcast (Terminator a) L.Terminator where
  downcast Ret                  = L.Ret Nothing md
  downcast (RetVal x)           = L.Ret (Just (downcast x)) md
  downcast (Br l)               = L.Br (downcast l) md
  downcast (CondBr p t f)       = L.CondBr (downcast p) (downcast t) (downcast f) md
  downcast (Switch p d a)       = L.Switch (downcast p) (downcast d) (downcast a) md

instance Downcast a b => Downcast (L.Named a) (L.Named b) where
  downcast (l L.:= r)   = l L.:= downcast r
  downcast (L.Do x)     = L.Do (downcast x)

instance Downcast Label L.Name where
  downcast (Label l)    = L.Name l

instance Downcast (GlobalFunction args t) L.CallableOperand where
  downcast f
    = let trav :: GlobalFunction args t -> ([L.Type], L.Type, L.Name)
          trav (Body t n)  = ([], downcast t, downcast n)
          trav (Lam t _ l) = let (t',r, n) = trav l
                             in  (downcast t : t', r, n)

          (args, result, name)  = trav f
          ty                    = L.FunctionType result args False
      in
      Right (L.ConstantOperand (LC.GlobalReference ty name))

instance Downcast (GlobalFunction args t) [(L.Operand, [L.ParameterAttribute])] where
  downcast Body{}      = []
  downcast (Lam _ x l) = (downcast x, []) : downcast l

instance Downcast FunctionAttribute L.FunctionAttribute where
  downcast NoReturn     = L.NoReturn
  downcast NoUnwind     = L.NoUnwind
  downcast ReadOnly     = L.ReadOnly
  downcast ReadNone     = L.ReadNone
  downcast AlwaysInline = L.AlwaysInline

instance Downcast Volatile Bool where
  downcast Volatile    = True
  downcast NonVolatile = False

instance Downcast (Parameter a) L.Parameter where
  downcast (ScalarParameter t x) = L.Parameter (downcast t)                                 (downcast x) []
  downcast (PtrParameter t x)    = L.Parameter (L.PointerType (downcast t) (L.AddrSpace 0)) (downcast x) [L.NoAlias, L.NoCapture]       -- TLM: alignment!

instance Downcast (ScalarType a) L.Type where
  downcast (NumScalarType t)    = downcast t
  downcast (NonNumScalarType t) = downcast t

instance Downcast (NumType a) L.Type where
  downcast (IntegralNumType t) = downcast t
  downcast (FloatingNumType t) = downcast t

instance Downcast (IntegralType a) L.Type where
  downcast (TypeInt     _) = L.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: Int)) |] )
  downcast (TypeInt8    _) = L.IntegerType 8
  downcast (TypeInt16   _) = L.IntegerType 16
  downcast (TypeInt32   _) = L.IntegerType 32
  downcast (TypeInt64   _) = L.IntegerType 64
  downcast (TypeWord    _) = L.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: Word)) |] )
  downcast (TypeWord8   _) = L.IntegerType 8
  downcast (TypeWord16  _) = L.IntegerType 16
  downcast (TypeWord32  _) = L.IntegerType 32
  downcast (TypeWord64  _) = L.IntegerType 64
  downcast (TypeCShort  _) = L.IntegerType 16
  downcast (TypeCUShort _) = L.IntegerType 16
  downcast (TypeCInt    _) = L.IntegerType 32
  downcast (TypeCUInt   _) = L.IntegerType 32
  downcast (TypeCLong   _) = L.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: CLong)) |] )
  downcast (TypeCULong  _) = L.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: CULong)) |] )
  downcast (TypeCLLong  _) = L.IntegerType 64
  downcast (TypeCULLong _) = L.IntegerType 64

instance Downcast (FloatingType a) L.Type where
  downcast (TypeFloat   _) = L.FloatingPointType 32 L.IEEE
  downcast (TypeDouble  _) = L.FloatingPointType 64 L.IEEE
  downcast (TypeCFloat  _) = L.FloatingPointType 32 L.IEEE
  downcast (TypeCDouble _) = L.FloatingPointType 64 L.IEEE

instance Downcast (NonNumType a) L.Type where
  downcast (TypeBool   _) = L.IntegerType 1
  downcast (TypeChar   _) = L.IntegerType 32
  downcast (TypeCChar  _) = L.IntegerType 8
  downcast (TypeCSChar _) = L.IntegerType 8
  downcast (TypeCUChar _) = L.IntegerType 8

