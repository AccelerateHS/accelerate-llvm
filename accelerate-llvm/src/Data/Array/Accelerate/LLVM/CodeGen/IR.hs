{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.IR
-- Copyright   : [2015..2017] Trevor L. McDonell
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

import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error

import qualified Data.ByteString.Short                              as B


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
data instance Operands Half     = OP_Half    (Operand Half)
data instance Operands Float    = OP_Float   (Operand Float)
data instance Operands Double   = OP_Double  (Operand Double)
data instance Operands CFloat   = OP_CFloat  (Operand CFloat)
data instance Operands CDouble  = OP_CDouble (Operand CDouble)
data instance Operands Bool     = OP_Bool    (Operand Bool)
data instance Operands Char     = OP_Char    (Operand Char)
data instance Operands CChar    = OP_CChar   (Operand CChar)
data instance Operands CSChar   = OP_CSChar  (Operand CSChar)
data instance Operands CUChar   = OP_CUChar  (Operand CUChar)
data instance Operands (V2 a)   = OP_V2      (Operand (V2 a))
data instance Operands (V3 a)   = OP_V3      (Operand (V3 a))
data instance Operands (V4 a)   = OP_V4      (Operand (V4 a))
data instance Operands (V8 a)   = OP_V8      (Operand (V8 a))
data instance Operands (V16 a)  = OP_V16     (Operand (V16 a))
data instance Operands (a,b)    = OP_Pair    (Operands a) (Operands b)


-- | Given some evidence that 'IR a' represents a scalar type, it can be
-- converted between the IR and Operand data types.
--
class IROP dict where
  op :: dict a -> IR a -> Operand a
  ir :: dict a -> Operand a -> IR a
  --
  op' :: dict a -> Operands a -> Operand a
  ir' :: dict a -> Operand a -> Operands a

instance IROP Type where
  op VoidType     _  = LocalReference VoidType (Name B.empty) -- TLM: ???
  op (PrimType t) x  = op t x

  ir VoidType     _  = IR OP_Unit
  ir (PrimType t) x  = ir t x
  --
  ir' VoidType     _ = OP_Unit
  ir' (PrimType t) x = ir' t x

  op' VoidType     _ = LocalReference VoidType (Name B.empty)  -- TLM: ???
  op' (PrimType t) x = op' t x

instance IROP PrimType where
  op (ScalarPrimType t)  = op t
  op t                   = $internalError "op" ("unhandled type: " ++ show t)
  ir (ScalarPrimType t)  = ir t
  ir t                   = $internalError "ir" ("unhandeld type: " ++ show t)
  --
  op' (ScalarPrimType t) = op' t
  op' t                  = $internalError "op'" ("unhandled type: " ++ show t)
  ir' (ScalarPrimType t) = ir' t
  ir' t                  = $internalError "ir'" ("unhandled type: " ++ show t)

instance IROP ScalarType where
  op (SingleScalarType t) = op t
  op (VectorScalarType t) = op t
  ir (SingleScalarType t) = ir t
  ir (VectorScalarType t) = ir t

  op' (SingleScalarType t) = op' t
  op' (VectorScalarType t) = op' t

  ir' (SingleScalarType t) = ir' t
  ir' (VectorScalarType t) = ir' t

instance IROP SingleType where
  op (NumSingleType t)    = op t
  op (NonNumSingleType t) = op t
  ir (NumSingleType t)    = ir t
  ir (NonNumSingleType t) = ir t
  --
  op' (NumSingleType t)    = op' t
  op' (NonNumSingleType t) = op' t
  ir' (NumSingleType t)    = ir' t
  ir' (NonNumSingleType t) = ir' t

instance IROP VectorType where
  op Vector2Type{}  (IR (OP_V2 x))  = x
  op Vector3Type{}  (IR (OP_V3 x))  = x
  op Vector4Type{}  (IR (OP_V4 x))  = x
  op Vector8Type{}  (IR (OP_V8 x))  = x
  op Vector16Type{} (IR (OP_V16 x)) = x
  --
  ir Vector2Type{}  = IR . OP_V2
  ir Vector3Type{}  = IR . OP_V3
  ir Vector4Type{}  = IR . OP_V4
  ir Vector8Type{}  = IR . OP_V8
  ir Vector16Type{} = IR . OP_V16
  --
  op' Vector2Type{}  (OP_V2 x)  = x
  op' Vector3Type{}  (OP_V3 x)  = x
  op' Vector4Type{}  (OP_V4 x)  = x
  op' Vector8Type{}  (OP_V8 x)  = x
  op' Vector16Type{} (OP_V16 x) = x
  --
  ir' Vector2Type{}  = OP_V2
  ir' Vector3Type{}  = OP_V3
  ir' Vector4Type{}  = OP_V4
  ir' Vector8Type{}  = OP_V8
  ir' Vector16Type{} = OP_V16

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
  op TypeInt{}     (IR (OP_Int     x)) = x
  op TypeInt8{}    (IR (OP_Int8    x)) = x
  op TypeInt16{}   (IR (OP_Int16   x)) = x
  op TypeInt32{}   (IR (OP_Int32   x)) = x
  op TypeInt64{}   (IR (OP_Int64   x)) = x
  op TypeWord{}    (IR (OP_Word    x)) = x
  op TypeWord8{}   (IR (OP_Word8   x)) = x
  op TypeWord16{}  (IR (OP_Word16  x)) = x
  op TypeWord32{}  (IR (OP_Word32  x)) = x
  op TypeWord64{}  (IR (OP_Word64  x)) = x
  op TypeCShort{}  (IR (OP_CShort  x)) = x
  op TypeCUShort{} (IR (OP_CUShort x)) = x
  op TypeCInt{}    (IR (OP_CInt    x)) = x
  op TypeCUInt{}   (IR (OP_CUInt   x)) = x
  op TypeCLong{}   (IR (OP_CLong   x)) = x
  op TypeCULong{}  (IR (OP_CULong  x)) = x
  op TypeCLLong{}  (IR (OP_CLLong  x)) = x
  op TypeCULLong{} (IR (OP_CULLong x)) = x
  --
  ir TypeInt{}     = IR . OP_Int
  ir TypeInt8{}    = IR . OP_Int8
  ir TypeInt16{}   = IR . OP_Int16
  ir TypeInt32{}   = IR . OP_Int32
  ir TypeInt64{}   = IR . OP_Int64
  ir TypeWord{}    = IR . OP_Word
  ir TypeWord8{}   = IR . OP_Word8
  ir TypeWord16{}  = IR . OP_Word16
  ir TypeWord32{}  = IR . OP_Word32
  ir TypeWord64{}  = IR . OP_Word64
  ir TypeCShort{}  = IR . OP_CShort
  ir TypeCUShort{} = IR . OP_CUShort
  ir TypeCInt{}    = IR . OP_CInt
  ir TypeCUInt{}   = IR . OP_CUInt
  ir TypeCLong{}   = IR . OP_CLong
  ir TypeCULong{}  = IR . OP_CULong
  ir TypeCLLong{}  = IR . OP_CLLong
  ir TypeCULLong{} = IR . OP_CULLong
  --
  op' TypeInt{}     (OP_Int     x) = x
  op' TypeInt8{}    (OP_Int8    x) = x
  op' TypeInt16{}   (OP_Int16   x) = x
  op' TypeInt32{}   (OP_Int32   x) = x
  op' TypeInt64{}   (OP_Int64   x) = x
  op' TypeWord{}    (OP_Word    x) = x
  op' TypeWord8{}   (OP_Word8   x) = x
  op' TypeWord16{}  (OP_Word16  x) = x
  op' TypeWord32{}  (OP_Word32  x) = x
  op' TypeWord64{}  (OP_Word64  x) = x
  op' TypeCShort{}  (OP_CShort  x) = x
  op' TypeCUShort{} (OP_CUShort x) = x
  op' TypeCInt{}    (OP_CInt    x) = x
  op' TypeCUInt{}   (OP_CUInt   x) = x
  op' TypeCLong{}   (OP_CLong   x) = x
  op' TypeCULong{}  (OP_CULong  x) = x
  op' TypeCLLong{}  (OP_CLLong  x) = x
  op' TypeCULLong{} (OP_CULLong x) = x
  --
  ir' TypeInt{}     = OP_Int
  ir' TypeInt8{}    = OP_Int8
  ir' TypeInt16{}   = OP_Int16
  ir' TypeInt32{}   = OP_Int32
  ir' TypeInt64{}   = OP_Int64
  ir' TypeWord{}    = OP_Word
  ir' TypeWord8{}   = OP_Word8
  ir' TypeWord16{}  = OP_Word16
  ir' TypeWord32{}  = OP_Word32
  ir' TypeWord64{}  = OP_Word64
  ir' TypeCShort{}  = OP_CShort
  ir' TypeCUShort{} = OP_CUShort
  ir' TypeCInt{}    = OP_CInt
  ir' TypeCUInt{}   = OP_CUInt
  ir' TypeCLong{}   = OP_CLong
  ir' TypeCULong{}  = OP_CULong
  ir' TypeCLLong{}  = OP_CLLong
  ir' TypeCULLong{} = OP_CULLong

instance IROP FloatingType where
  op TypeHalf{}    (IR (OP_Half    x)) = x
  op TypeFloat{}   (IR (OP_Float   x)) = x
  op TypeDouble{}  (IR (OP_Double  x)) = x
  op TypeCFloat{}  (IR (OP_CFloat  x)) = x
  op TypeCDouble{} (IR (OP_CDouble x)) = x
  --
  ir TypeHalf{}    = IR . OP_Half
  ir TypeFloat{}   = IR . OP_Float
  ir TypeDouble{}  = IR . OP_Double
  ir TypeCFloat{}  = IR . OP_CFloat
  ir TypeCDouble{} = IR . OP_CDouble
  --
  op' TypeHalf{}    (OP_Half    x) = x
  op' TypeFloat{}   (OP_Float   x) = x
  op' TypeDouble{}  (OP_Double  x) = x
  op' TypeCFloat{}  (OP_CFloat  x) = x
  op' TypeCDouble{} (OP_CDouble x) = x
  --
  ir' TypeHalf{}    = OP_Half
  ir' TypeFloat{}   = OP_Float
  ir' TypeDouble{}  = OP_Double
  ir' TypeCFloat{}  = OP_CFloat
  ir' TypeCDouble{} = OP_CDouble

instance IROP NonNumType where
  op TypeBool{}   (IR (OP_Bool   x)) = x
  op TypeChar{}   (IR (OP_Char   x)) = x
  op TypeCChar{}  (IR (OP_CChar  x)) = x
  op TypeCSChar{} (IR (OP_CSChar x)) = x
  op TypeCUChar{} (IR (OP_CUChar x)) = x
  --
  ir TypeBool{}   = IR . OP_Bool
  ir TypeChar{}   = IR . OP_Char
  ir TypeCChar{}  = IR . OP_CChar
  ir TypeCSChar{} = IR . OP_CSChar
  ir TypeCUChar{} = IR . OP_CUChar
  --
  op' TypeBool{}   (OP_Bool   x) = x
  op' TypeChar{}   (OP_Char   x) = x
  op' TypeCChar{}  (OP_CChar  x) = x
  op' TypeCSChar{} (OP_CSChar x) = x
  op' TypeCUChar{} (OP_CUChar x) = x
  --
  ir' TypeBool{}   = OP_Bool
  ir' TypeChar{}   = OP_Char
  ir' TypeCChar{}  = OP_CChar
  ir' TypeCSChar{} = OP_CSChar
  ir' TypeCUChar{} = OP_CUChar

