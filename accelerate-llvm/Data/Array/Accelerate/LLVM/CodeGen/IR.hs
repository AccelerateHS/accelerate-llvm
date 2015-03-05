{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.IR
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.IR (

  IR(..), Operands(..),
  IROP(..),

) where

import LLVM.General.AST.Type.Operand

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar


-- | The datatype 'IR' represents the LLVM IR producing a value of type 'a'.
-- Note that the operands comprising this value are stored in representation
-- type.
--
data IR t where
  IR :: Operands (EltRepr t)
     -> IR t

-- We use a data family to represent sequences of LLVM (scalar) operands
-- representing a single Accelerate type. Using a data family rather than a type
-- family means that Operands is bijective.
--
data family Operands e :: *
data instance Operands ()       = OP_Unit
data instance Operands Int      = OP_Int     (Operand Int)
data instance Operands Int8     = OP_Int8    (Operand Int8)
data instance Operands Int16    = OP_Int16   (Operand Int16)
data instance Operands Int32    = OP_Int32   (Operand Int32)
data instance Operands Int64    = OP_Int64   (Operand Int64)
data instance Operands Word     = OP_Word    (Operand Word)
data instance Operands Word8    = OP_Word8   (Operand Word8)
data instance Operands Word16   = OP_Word16  (Operand Word16)
data instance Operands Word32   = OP_Word32  (Operand Word32)
data instance Operands Word64   = OP_Word64  (Operand Word64)
data instance Operands CShort   = OP_CShort  (Operand CShort)
data instance Operands CUShort  = OP_CUShort (Operand CUShort)
data instance Operands CInt     = OP_CInt    (Operand CInt)
data instance Operands CUInt    = OP_CUInt   (Operand CUInt)
data instance Operands CLong    = OP_CLong   (Operand CLong)
data instance Operands CULong   = OP_CULong  (Operand CULong)
data instance Operands CLLong   = OP_CLLong  (Operand CLLong)
data instance Operands CULLong  = OP_CULLong (Operand CULLong)
data instance Operands Float    = OP_Float   (Operand Float)
data instance Operands Double   = OP_Double  (Operand Double)
data instance Operands CFloat   = OP_CFloat  (Operand CFloat)
data instance Operands CDouble  = OP_CDouble (Operand CDouble)
data instance Operands Bool     = OP_Bool    (Operand Bool)
data instance Operands Char     = OP_Char    (Operand Char)
data instance Operands CChar    = OP_CChar   (Operand CChar)
data instance Operands CSChar   = OP_CSChar  (Operand CSChar)
data instance Operands CUChar   = OP_CUChar  (Operand CUChar)
data instance Operands (a,b)    = OP_Pair    (Operands a) (Operands b)


-- | Given some evidence that 'IR a' represents a scalar type, it can be
-- converted between the IR and Operand data types.
--
class IROP dict where
  op :: dict a -> IR a -> Operand a
  ir :: dict a -> Operand a -> IR a
  --
  ir' :: dict a -> Operand a -> Operands a
  op' :: dict a -> Operands a -> Operand a

instance IROP ScalarType where
  op (NumScalarType t)    = op t
  op (NonNumScalarType t) = op t
  ir (NumScalarType t)    = ir t
  ir (NonNumScalarType t) = ir t
  --
  op' (NumScalarType t)    = op' t
  op' (NonNumScalarType t) = op' t
  ir' (NumScalarType t)    = ir' t
  ir' (NonNumScalarType t) = ir' t

instance IROP NumType where
  op (IntegralNumType t) = op t
  op (FloatingNumType t) = op t
  ir (IntegralNumType t) = ir t
  ir (FloatingNumType t) = ir t
  --
  op' (IntegralNumType t) = op' t
  op' (FloatingNumType t) = op' t
  ir' (IntegralNumType t) = ir' t
  ir' (FloatingNumType t) = ir' t

instance IROP IntegralType where
  op (TypeInt     _) (IR (OP_Int     x)) = x
  op (TypeInt8    _) (IR (OP_Int8    x)) = x
  op (TypeInt16   _) (IR (OP_Int16   x)) = x
  op (TypeInt32   _) (IR (OP_Int32   x)) = x
  op (TypeInt64   _) (IR (OP_Int64   x)) = x
  op (TypeWord    _) (IR (OP_Word    x)) = x
  op (TypeWord8   _) (IR (OP_Word8   x)) = x
  op (TypeWord16  _) (IR (OP_Word16  x)) = x
  op (TypeWord32  _) (IR (OP_Word32  x)) = x
  op (TypeWord64  _) (IR (OP_Word64  x)) = x
  op (TypeCShort  _) (IR (OP_CShort  x)) = x
  op (TypeCUShort _) (IR (OP_CUShort x)) = x
  op (TypeCInt    _) (IR (OP_CInt    x)) = x
  op (TypeCUInt   _) (IR (OP_CUInt   x)) = x
  op (TypeCLong   _) (IR (OP_CLong   x)) = x
  op (TypeCULong  _) (IR (OP_CULong  x)) = x
  op (TypeCLLong  _) (IR (OP_CLLong  x)) = x
  op (TypeCULLong _) (IR (OP_CULLong x)) = x
  --
  ir (TypeInt     _) = IR . OP_Int
  ir (TypeInt8    _) = IR . OP_Int8
  ir (TypeInt16   _) = IR . OP_Int16
  ir (TypeInt32   _) = IR . OP_Int32
  ir (TypeInt64   _) = IR . OP_Int64
  ir (TypeWord    _) = IR . OP_Word
  ir (TypeWord8   _) = IR . OP_Word8
  ir (TypeWord16  _) = IR . OP_Word16
  ir (TypeWord32  _) = IR . OP_Word32
  ir (TypeWord64  _) = IR . OP_Word64
  ir (TypeCShort  _) = IR . OP_CShort
  ir (TypeCUShort _) = IR . OP_CUShort
  ir (TypeCInt    _) = IR . OP_CInt
  ir (TypeCUInt   _) = IR . OP_CUInt
  ir (TypeCLong   _) = IR . OP_CLong
  ir (TypeCULong  _) = IR . OP_CULong
  ir (TypeCLLong  _) = IR . OP_CLLong
  ir (TypeCULLong _) = IR . OP_CULLong
  --
  op' (TypeInt     _) (OP_Int     x) = x
  op' (TypeInt8    _) (OP_Int8    x) = x
  op' (TypeInt16   _) (OP_Int16   x) = x
  op' (TypeInt32   _) (OP_Int32   x) = x
  op' (TypeInt64   _) (OP_Int64   x) = x
  op' (TypeWord    _) (OP_Word    x) = x
  op' (TypeWord8   _) (OP_Word8   x) = x
  op' (TypeWord16  _) (OP_Word16  x) = x
  op' (TypeWord32  _) (OP_Word32  x) = x
  op' (TypeWord64  _) (OP_Word64  x) = x
  op' (TypeCShort  _) (OP_CShort  x) = x
  op' (TypeCUShort _) (OP_CUShort x) = x
  op' (TypeCInt    _) (OP_CInt    x) = x
  op' (TypeCUInt   _) (OP_CUInt   x) = x
  op' (TypeCLong   _) (OP_CLong   x) = x
  op' (TypeCULong  _) (OP_CULong  x) = x
  op' (TypeCLLong  _) (OP_CLLong  x) = x
  op' (TypeCULLong _) (OP_CULLong x) = x
  --
  ir' (TypeInt     _) = OP_Int
  ir' (TypeInt8    _) = OP_Int8
  ir' (TypeInt16   _) = OP_Int16
  ir' (TypeInt32   _) = OP_Int32
  ir' (TypeInt64   _) = OP_Int64
  ir' (TypeWord    _) = OP_Word
  ir' (TypeWord8   _) = OP_Word8
  ir' (TypeWord16  _) = OP_Word16
  ir' (TypeWord32  _) = OP_Word32
  ir' (TypeWord64  _) = OP_Word64
  ir' (TypeCShort  _) = OP_CShort
  ir' (TypeCUShort _) = OP_CUShort
  ir' (TypeCInt    _) = OP_CInt
  ir' (TypeCUInt   _) = OP_CUInt
  ir' (TypeCLong   _) = OP_CLong
  ir' (TypeCULong  _) = OP_CULong
  ir' (TypeCLLong  _) = OP_CLLong
  ir' (TypeCULLong _) = OP_CULLong

instance IROP FloatingType where
  op (TypeFloat   _) (IR (OP_Float   x)) = x
  op (TypeDouble  _) (IR (OP_Double  x)) = x
  op (TypeCFloat  _) (IR (OP_CFloat  x)) = x
  op (TypeCDouble _) (IR (OP_CDouble x)) = x
  --
  ir (TypeFloat   _) = IR . OP_Float
  ir (TypeDouble  _) = IR . OP_Double
  ir (TypeCFloat  _) = IR . OP_CFloat
  ir (TypeCDouble _) = IR . OP_CDouble
  --
  op' (TypeFloat   _) (OP_Float   x) = x
  op' (TypeDouble  _) (OP_Double  x) = x
  op' (TypeCFloat  _) (OP_CFloat  x) = x
  op' (TypeCDouble _) (OP_CDouble x) = x
  --
  ir' (TypeFloat   _) = OP_Float
  ir' (TypeDouble  _) = OP_Double
  ir' (TypeCFloat  _) = OP_CFloat
  ir' (TypeCDouble _) = OP_CDouble

instance IROP NonNumType where
  op (TypeBool   _) (IR (OP_Bool   x)) = x
  op (TypeChar   _) (IR (OP_Char   x)) = x
  op (TypeCChar  _) (IR (OP_CChar  x)) = x
  op (TypeCSChar _) (IR (OP_CSChar x)) = x
  op (TypeCUChar _) (IR (OP_CUChar x)) = x
  --
  ir (TypeBool   _) = IR . OP_Bool
  ir (TypeChar   _) = IR . OP_Char
  ir (TypeCChar  _) = IR . OP_CChar
  ir (TypeCSChar _) = IR . OP_CSChar
  ir (TypeCUChar _) = IR . OP_CUChar
  --
  op' (TypeBool   _) (OP_Bool   x) = x
  op' (TypeChar   _) (OP_Char   x) = x
  op' (TypeCChar  _) (OP_CChar  x) = x
  op' (TypeCSChar _) (OP_CSChar x) = x
  op' (TypeCUChar _) (OP_CUChar x) = x
  --
  ir' (TypeBool   _) = OP_Bool
  ir' (TypeChar   _) = OP_Char
  ir' (TypeCChar  _) = OP_CChar
  ir' (TypeCSChar _) = OP_CSChar
  ir' (TypeCUChar _) = OP_CUChar

