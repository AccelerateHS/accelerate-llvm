{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : LLVM.General.AST.Type.Downcast
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.General.AST.Type.Downcast
  where

import Data.Array.Accelerate.Type

import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Flags
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Representation

import Foreign.C.Types

import qualified LLVM.General.AST.Constant                      as LC
import qualified LLVM.General.AST.Float                         as L
import qualified LLVM.General.AST.Instruction                   as L
import qualified LLVM.General.AST.Name                          as L
import qualified LLVM.General.AST.Operand                       as L
import qualified LLVM.General.AST.Type                          as L


class Downcast a b where
  downcast :: a -> b

instance Downcast NUW Bool where
  downcast NoUnsignedWrap = True
  downcast UnsignedWrap   = False

instance Downcast NSW Bool where
  downcast NoSignedWrap = True
  downcast SignedWrap   = False

instance Downcast FastMathFlags L.FastMathFlags where
  downcast = id

nsw :: Bool
nsw = False

nuw :: Bool
nuw = False

fmf :: FastMathFlags
fmf = UnsafeAlgebra

md :: L.InstructionMetadata
md = []


instance Downcast (Name a) L.Name where
  downcast (Name s)   = L.Name s
  downcast (UnName n) = L.UnName n

instance Downcast (Instruction a) L.Instruction where
  downcast (Add IntegralNumType{} x y) = L.Add nsw nuw (downcast x) (downcast y) md
  downcast (Add FloatingNumType{} x y) = L.FAdd fmf (downcast x) (downcast y) md

--  downcast (Mul x y)
--    | IntegralNumType{} <- numType :: NumType a = L.Mul nsw nuw (downcast x) (downcast y) md
--    | FloatingNumType{} <- numType :: NumType a = L.FMul fmf (downcast x) (downcast y) md


instance Downcast (Constant a) LC.Constant where
  downcast (IntegralConstant x)
    | IntegralDict <- integralDict (integralType :: IntegralType a)
    = LC.Int (L.typeBits (singleValueType (undefined :: a))) (toInteger x)

  downcast (FloatingConstant x) = LC.Float $
    case floatingType :: FloatingType a of
      TypeFloat{}   -> L.Single x
      TypeDouble{}  -> L.Double x
      TypeCFloat{}  -> L.Single $ case x of CFloat x' -> x'
      TypeCDouble{} -> L.Double $ case x of CDouble x' -> x'

  downcast (NonNumConstant x)   = LC.Int (L.typeBits (singleValueType (undefined :: a))) x

  downcast (GlobalReference n)  = LC.GlobalReference (llvmType (undefined::a)) (downcast n)
  downcast Undef                = LC.Undef (llvmType (undefined::a))

instance Downcast (Operand a) L.Operand where
  downcast (LocalReference t n)      = L.LocalReference (downcast t) (downcast n)
  downcast (ConstantOperand c)       = L.ConstantOperand (downcast c)
  downcast (MetadataStringOperand s) = L.MetadataStringOperand s

instance Downcast (Terminator a) L.Terminator where
  downcast Ret          = L.Ret Nothing []
  downcast (RetVal x)   = L.Ret (Just (downcast x)) []

instance Downcast a b => Downcast (L.Named a) (L.Named b) where
  downcast (l L.:= r)   = l L.:= downcast r
  downcast (L.Do x)     = L.Do (downcast x)

instance Downcast Label L.Name where
  downcast (Label l)    = L.Name l

instance Downcast (ScalarType a) L.Type where
  downcast (NumScalarType t)    = downcast t
  downcast (NonNumScalarType t) = downcast t

instance Downcast (NumType a) L.Type where
  downcast (IntegralNumType t) = downcast t
  downcast (FloatingNumType t) = downcast t

-- TODO: make instances concrete
--
instance Downcast (IntegralType a) L.Type where
  downcast (TypeInt     _) = llvmType (undefined::a)
  downcast (TypeInt8    _) = llvmType (undefined::a)
  downcast (TypeInt16   _) = llvmType (undefined::a)
  downcast (TypeInt32   _) = llvmType (undefined::a)
  downcast (TypeInt64   _) = llvmType (undefined::a)
  downcast (TypeWord    _) = llvmType (undefined::a)
  downcast (TypeWord8   _) = llvmType (undefined::a)
  downcast (TypeWord16  _) = llvmType (undefined::a)
  downcast (TypeWord32  _) = llvmType (undefined::a)
  downcast (TypeWord64  _) = llvmType (undefined::a)
  downcast (TypeCShort  _) = llvmType (undefined::a)
  downcast (TypeCUShort _) = llvmType (undefined::a)
  downcast (TypeCInt    _) = llvmType (undefined::a)
  downcast (TypeCUInt   _) = llvmType (undefined::a)
  downcast (TypeCLong   _) = llvmType (undefined::a)
  downcast (TypeCULong  _) = llvmType (undefined::a)
  downcast (TypeCLLong  _) = llvmType (undefined::a)
  downcast (TypeCULLong _) = llvmType (undefined::a)

instance Downcast (FloatingType a) L.Type where
  downcast (TypeFloat   _) = llvmType (undefined::a)
  downcast (TypeDouble  _) = llvmType (undefined::a)
  downcast (TypeCFloat  _) = llvmType (undefined::a)
  downcast (TypeCDouble _) = llvmType (undefined::a)

instance Downcast (NonNumType a) L.Type where
  downcast (TypeBool   _) = llvmType (undefined::a)
  downcast (TypeChar   _) = llvmType (undefined::a)
  downcast (TypeCChar  _) = llvmType (undefined::a)
  downcast (TypeCSChar _) = llvmType (undefined::a)
  downcast (TypeCUChar _) = llvmType (undefined::a)

