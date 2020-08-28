{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.IR
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.IR (

  Operands(..),
  IROP(..),

) where

import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import Data.Array.Accelerate.Error
import Data.Primitive.Vec

import qualified Data.ByteString.Short                              as B


-- We use a data family to represent sequences of LLVM (scalar) operands
-- representing a single Accelerate type. Using a data family rather than a type
-- family means that Operands is bijective.
--
data family Operands e :: *
data instance Operands ()         = OP_Unit
data instance Operands Int        = OP_Int     (Operand Int)
data instance Operands Int8       = OP_Int8    (Operand Int8)
data instance Operands Int16      = OP_Int16   (Operand Int16)
data instance Operands Int32      = OP_Int32   (Operand Int32)
data instance Operands Int64      = OP_Int64   (Operand Int64)
data instance Operands Word       = OP_Word    (Operand Word)
data instance Operands Word8      = OP_Word8   (Operand Word8)
data instance Operands Word16     = OP_Word16  (Operand Word16)
data instance Operands Word32     = OP_Word32  (Operand Word32)
data instance Operands Word64     = OP_Word64  (Operand Word64)
data instance Operands Half       = OP_Half    (Operand Half)
data instance Operands Float      = OP_Float   (Operand Float)
data instance Operands Double     = OP_Double  (Operand Double)
data instance Operands Bool       = OP_Bool    (Operand Bool)
data instance Operands (Vec n a)  = OP_Vec     (Operand (Vec n a))
data instance Operands (a,b)      = OP_Pair    (Operands a) (Operands b)


-- | Given some evidence that 'IR a' represents a scalar type, it can be
-- converted between the IR and Operand data types.
--
class IROP dict where
  op :: HasCallStack => dict a -> Operands a -> Operand a
  ir :: HasCallStack => dict a -> Operand a -> Operands a

instance IROP Type where
  ir VoidType     _ = OP_Unit
  ir (PrimType t) x = ir t x

  op VoidType     _ = LocalReference VoidType (Name B.empty)
  op (PrimType t) x = op t x

instance IROP PrimType where
  op (ScalarPrimType t) = op t
  op BoolPrimType       = \case OP_Bool x -> x
  op t                  = internalError ("unhandled type: " ++ show t)
  ir (ScalarPrimType t) = ir t
  ir BoolPrimType       = OP_Bool
  ir t                  = internalError ("unhandled type: " ++ show t)

instance IROP ScalarType where
  op (SingleScalarType t) = op t
  op (VectorScalarType t) = op t
  ir (SingleScalarType t) = ir t
  ir (VectorScalarType t) = ir t

instance IROP SingleType where
  op (NumSingleType t) = op t
  ir (NumSingleType t) = ir t

instance IROP VectorType where
  op (VectorType _ v) = single v
    where
      single :: SingleType t -> Operands (Vec n t) -> Operand (Vec n t)
      single (NumSingleType t) = num t

      num :: NumType t -> Operands (Vec n t) -> Operand (Vec n t)
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType t -> Operands (Vec n t) -> Operand (Vec n t)
      integral TypeInt    (OP_Vec x) = x
      integral TypeInt8   (OP_Vec x) = x
      integral TypeInt16  (OP_Vec x) = x
      integral TypeInt32  (OP_Vec x) = x
      integral TypeInt64  (OP_Vec x) = x
      integral TypeWord   (OP_Vec x) = x
      integral TypeWord8  (OP_Vec x) = x
      integral TypeWord16 (OP_Vec x) = x
      integral TypeWord32 (OP_Vec x) = x
      integral TypeWord64 (OP_Vec x) = x

      floating :: FloatingType t -> Operands (Vec n t) -> Operand (Vec n t)
      floating TypeHalf   (OP_Vec x) = x
      floating TypeFloat  (OP_Vec x) = x
      floating TypeDouble (OP_Vec x) = x

  ir (VectorType _ v) = single v
    where
      single :: SingleType t -> Operand (Vec n t) -> Operands (Vec n t)
      single (NumSingleType t) = num t

      num :: NumType t -> Operand (Vec n t) -> Operands (Vec n t)
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType t -> Operand (Vec n t) -> Operands (Vec n t)
      integral TypeInt    = OP_Vec
      integral TypeInt8   = OP_Vec
      integral TypeInt16  = OP_Vec
      integral TypeInt32  = OP_Vec
      integral TypeInt64  = OP_Vec
      integral TypeWord   = OP_Vec
      integral TypeWord8  = OP_Vec
      integral TypeWord16 = OP_Vec
      integral TypeWord32 = OP_Vec
      integral TypeWord64 = OP_Vec

      floating :: FloatingType t -> Operand (Vec n t) -> Operands (Vec n t)
      floating TypeHalf   = OP_Vec
      floating TypeFloat  = OP_Vec
      floating TypeDouble = OP_Vec

instance IROP NumType where
  op (IntegralNumType t) = op t
  op (FloatingNumType t) = op t
  ir (IntegralNumType t) = ir t
  ir (FloatingNumType t) = ir t

instance IROP IntegralType where
  op TypeInt     (OP_Int     x) = x
  op TypeInt8    (OP_Int8    x) = x
  op TypeInt16   (OP_Int16   x) = x
  op TypeInt32   (OP_Int32   x) = x
  op TypeInt64   (OP_Int64   x) = x
  op TypeWord    (OP_Word    x) = x
  op TypeWord8   (OP_Word8   x) = x
  op TypeWord16  (OP_Word16  x) = x
  op TypeWord32  (OP_Word32  x) = x
  op TypeWord64  (OP_Word64  x) = x
  --
  ir TypeInt     = OP_Int
  ir TypeInt8    = OP_Int8
  ir TypeInt16   = OP_Int16
  ir TypeInt32   = OP_Int32
  ir TypeInt64   = OP_Int64
  ir TypeWord    = OP_Word
  ir TypeWord8   = OP_Word8
  ir TypeWord16  = OP_Word16
  ir TypeWord32  = OP_Word32
  ir TypeWord64  = OP_Word64

instance IROP FloatingType where
  op TypeHalf   (OP_Half   x) = x
  op TypeFloat  (OP_Float  x) = x
  op TypeDouble (OP_Double x) = x
  --
  ir TypeHalf   = OP_Half
  ir TypeFloat  = OP_Float
  ir TypeDouble = OP_Double

