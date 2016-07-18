{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
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

import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Representation

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

-- Extra instances to support operands of pointer type
--
data instance Operands (Ptr Int)      = OP_PtrInt     (Operand (Ptr Int))
data instance Operands (Ptr Int8)     = OP_PtrInt8    (Operand (Ptr Int8))
data instance Operands (Ptr Int16)    = OP_PtrInt16   (Operand (Ptr Int16))
data instance Operands (Ptr Int32)    = OP_PtrInt32   (Operand (Ptr Int32))
data instance Operands (Ptr Int64)    = OP_PtrInt64   (Operand (Ptr Int64))
data instance Operands (Ptr Word)     = OP_PtrWord    (Operand (Ptr Word))
data instance Operands (Ptr Word8)    = OP_PtrWord8   (Operand (Ptr Word8))
data instance Operands (Ptr Word16)   = OP_PtrWord16  (Operand (Ptr Word16))
data instance Operands (Ptr Word32)   = OP_PtrWord32  (Operand (Ptr Word32))
data instance Operands (Ptr Word64)   = OP_PtrWord64  (Operand (Ptr Word64))
data instance Operands (Ptr CShort)   = OP_PtrCShort  (Operand (Ptr CShort))
data instance Operands (Ptr CUShort)  = OP_PtrCUShort (Operand (Ptr CUShort))
data instance Operands (Ptr CInt)     = OP_PtrCInt    (Operand (Ptr CInt))
data instance Operands (Ptr CUInt)    = OP_PtrCUInt   (Operand (Ptr CUInt))
data instance Operands (Ptr CLong)    = OP_PtrCLong   (Operand (Ptr CLong))
data instance Operands (Ptr CULong)   = OP_PtrCULong  (Operand (Ptr CULong))
data instance Operands (Ptr CLLong)   = OP_PtrCLLong  (Operand (Ptr CLLong))
data instance Operands (Ptr CULLong)  = OP_PtrCULLong (Operand (Ptr CULLong))
data instance Operands (Ptr Float)    = OP_PtrFloat   (Operand (Ptr Float))
data instance Operands (Ptr Double)   = OP_PtrDouble  (Operand (Ptr Double))
data instance Operands (Ptr CFloat)   = OP_PtrCFloat  (Operand (Ptr CFloat))
data instance Operands (Ptr CDouble)  = OP_PtrCDouble (Operand (Ptr CDouble))
data instance Operands (Ptr Bool)     = OP_PtrBool    (Operand (Ptr Bool))
data instance Operands (Ptr Char)     = OP_PtrChar    (Operand (Ptr Char))
data instance Operands (Ptr CChar)    = OP_PtrCChar   (Operand (Ptr CChar))
data instance Operands (Ptr CSChar)   = OP_PtrCSChar  (Operand (Ptr CSChar))
data instance Operands (Ptr CUChar)   = OP_PtrCUChar  (Operand (Ptr CUChar))

type instance EltRepr (Ptr Int)       = Ptr Int
type instance EltRepr (Ptr Int8)      = Ptr Int8
type instance EltRepr (Ptr Int16)     = Ptr Int16
type instance EltRepr (Ptr Int32)     = Ptr Int32
type instance EltRepr (Ptr Int64)     = Ptr Int64
type instance EltRepr (Ptr Word)      = Ptr Word
type instance EltRepr (Ptr Word8)     = Ptr Word8
type instance EltRepr (Ptr Word16)    = Ptr Word16
type instance EltRepr (Ptr Word32)    = Ptr Word32
type instance EltRepr (Ptr Word64)    = Ptr Word64
type instance EltRepr (Ptr CShort)    = Ptr CShort
type instance EltRepr (Ptr CUShort)   = Ptr CUShort
type instance EltRepr (Ptr CInt)      = Ptr CInt
type instance EltRepr (Ptr CUInt)     = Ptr CUInt
type instance EltRepr (Ptr CLong)     = Ptr CLong
type instance EltRepr (Ptr CULong)    = Ptr CULong
type instance EltRepr (Ptr CLLong)    = Ptr CLLong
type instance EltRepr (Ptr CULLong)   = Ptr CULLong
type instance EltRepr (Ptr Float)     = Ptr Float
type instance EltRepr (Ptr Double)    = Ptr Double
type instance EltRepr (Ptr CFloat)    = Ptr CFloat
type instance EltRepr (Ptr CDouble)   = Ptr CDouble
type instance EltRepr (Ptr Bool)      = Ptr Bool
type instance EltRepr (Ptr Char)      = Ptr Char
type instance EltRepr (Ptr CChar)     = Ptr CChar
type instance EltRepr (Ptr CSChar)    = Ptr CSChar
type instance EltRepr (Ptr CUChar)    = Ptr CUChar
-- type instance EltRepr (Ptr (a,b))     = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b))
-- type instance EltRepr (Ptr (a,b,c))   = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c))
-- type instance EltRepr (Ptr (a,b,c,d)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d))
-- type instance EltRepr (Ptr (a,b,c,d,e)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e))
-- type instance EltRepr (Ptr (a,b,c,d,e,f)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e), EltRepr (Ptr f))
-- type instance EltRepr (Ptr (a,b,c,d,e,f,g)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e), EltRepr (Ptr f), EltRepr (Ptr g))
-- type instance EltRepr (Ptr (a,b,c,d,e,f,g,h)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e), EltRepr (Ptr f), EltRepr (Ptr g), EltRepr (Ptr h))
-- type instance EltRepr (Ptr (a,b,c,d,e,f,g,h,i)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e), EltRepr (Ptr f), EltRepr (Ptr g), EltRepr (Ptr h), EltRepr (Ptr i))
-- type instance EltRepr (Ptr (a,b,c,d,e,f,g,h,i,j)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e), EltRepr (Ptr f), EltRepr (Ptr g), EltRepr (Ptr h), EltRepr (Ptr i), EltRepr (Ptr j))
-- type instance EltRepr (Ptr (a,b,c,d,e,f,g,h,i,j,k)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e), EltRepr (Ptr f), EltRepr (Ptr g), EltRepr (Ptr h), EltRepr (Ptr i), EltRepr (Ptr j), EltRepr (Ptr k))
-- type instance EltRepr (Ptr (a,b,c,d,e,f,g,h,i,j,k,l)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e), EltRepr (Ptr f), EltRepr (Ptr g), EltRepr (Ptr h), EltRepr (Ptr i), EltRepr (Ptr j), EltRepr (Ptr k), EltRepr (Ptr l))
-- type instance EltRepr (Ptr (a,b,c,d,e,f,g,h,i,j,k,l,m)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e), EltRepr (Ptr f), EltRepr (Ptr g), EltRepr (Ptr h), EltRepr (Ptr i), EltRepr (Ptr j), EltRepr (Ptr k), EltRepr (Ptr l), EltRepr (Ptr m))
-- type instance EltRepr (Ptr (a,b,c,d,e,f,g,h,i,j,k,l,m,n)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e), EltRepr (Ptr f), EltRepr (Ptr g), EltRepr (Ptr h), EltRepr (Ptr i), EltRepr (Ptr j), EltRepr (Ptr k), EltRepr (Ptr l), EltRepr (Ptr m), EltRepr (Ptr n))
-- type instance EltRepr (Ptr (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)) = TupleRepr (EltRepr (Ptr a), EltRepr (Ptr b), EltRepr (Ptr c), EltRepr (Ptr d), EltRepr (Ptr e), EltRepr (Ptr f), EltRepr (Ptr g), EltRepr (Ptr h), EltRepr (Ptr i), EltRepr (Ptr j), EltRepr (Ptr k), EltRepr (Ptr l), EltRepr (Ptr m), EltRepr (Ptr n), EltRepr (Ptr o))

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
  op VoidType     _  = LocalReference VoidType (Name []) -- TLM: ???
  op (PrimType t) x  = op t x
  ir (PrimType t) x  = ir t x
  ir VoidType     _  = IR OP_Unit
  --
  ir' VoidType     _ = OP_Unit
  ir' (PrimType t) x = ir' t x
  op' (PrimType t) x = op' t x
  op' VoidType     _ = LocalReference VoidType (Name [])  -- TLM: ???

instance IROP PrimType where
  op (ScalarPrimType t) = op t
  op (PtrPrimType s _)  = scalar s
    where
      scalar :: ScalarType a -> IR (Ptr a) -> Operand (Ptr a)
      scalar (NonNumScalarType t) = nonnum t
      scalar (NumScalarType t)    = num t

      nonnum :: NonNumType a -> IR (Ptr a) -> Operand (Ptr a)
      nonnum TypeBool{}   (IR (OP_PtrBool   x)) = x
      nonnum TypeChar{}   (IR (OP_PtrChar   x)) = x
      nonnum TypeCChar{}  (IR (OP_PtrCChar  x)) = x
      nonnum TypeCSChar{} (IR (OP_PtrCSChar x)) = x
      nonnum TypeCUChar{} (IR (OP_PtrCUChar x)) = x

      num :: NumType a -> IR (Ptr a) -> Operand (Ptr a)
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType a -> IR (Ptr a) -> Operand (Ptr a)
      integral TypeInt{}     (IR (OP_PtrInt     x)) = x
      integral TypeInt8{}    (IR (OP_PtrInt8    x)) = x
      integral TypeInt16{}   (IR (OP_PtrInt16   x)) = x
      integral TypeInt32{}   (IR (OP_PtrInt32   x)) = x
      integral TypeInt64{}   (IR (OP_PtrInt64   x)) = x
      integral TypeWord{}    (IR (OP_PtrWord    x)) = x
      integral TypeWord8{}   (IR (OP_PtrWord8   x)) = x
      integral TypeWord16{}  (IR (OP_PtrWord16  x)) = x
      integral TypeWord32{}  (IR (OP_PtrWord32  x)) = x
      integral TypeWord64{}  (IR (OP_PtrWord64  x)) = x
      integral TypeCShort{}  (IR (OP_PtrCShort  x)) = x
      integral TypeCUShort{} (IR (OP_PtrCUShort x)) = x
      integral TypeCInt{}    (IR (OP_PtrCInt    x)) = x
      integral TypeCUInt{}   (IR (OP_PtrCUInt   x)) = x
      integral TypeCLong{}   (IR (OP_PtrCLong   x)) = x
      integral TypeCULong{}  (IR (OP_PtrCULong  x)) = x
      integral TypeCLLong{}  (IR (OP_PtrCLLong  x)) = x
      integral TypeCULLong{} (IR (OP_PtrCULLong x)) = x

      floating :: FloatingType a -> IR (Ptr a) -> Operand (Ptr a)
      floating TypeFloat{}   (IR (OP_PtrFloat   x)) = x
      floating TypeDouble{}  (IR (OP_PtrDouble  x)) = x
      floating TypeCFloat{}  (IR (OP_PtrCFloat  x)) = x
      floating TypeCDouble{} (IR (OP_PtrCDouble x)) = x

  ir (ScalarPrimType t) = ir t
  ir (PtrPrimType s _)  = scalar s
    where
      scalar :: ScalarType a -> Operand (Ptr a) -> IR (Ptr a)
      scalar (NonNumScalarType t) = nonnum t
      scalar (NumScalarType t)    = num t

      nonnum :: NonNumType a -> Operand (Ptr a) -> IR (Ptr a)
      nonnum TypeBool{}   = IR . OP_PtrBool
      nonnum TypeChar{}   = IR . OP_PtrChar
      nonnum TypeCChar{}  = IR . OP_PtrCChar
      nonnum TypeCSChar{} = IR . OP_PtrCSChar
      nonnum TypeCUChar{} = IR . OP_PtrCUChar

      num :: NumType a -> Operand (Ptr a) -> IR (Ptr a)
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType a -> Operand (Ptr a) -> IR (Ptr a)
      integral TypeInt{}     = IR . OP_PtrInt
      integral TypeInt8{}    = IR . OP_PtrInt8
      integral TypeInt16{}   = IR . OP_PtrInt16
      integral TypeInt32{}   = IR . OP_PtrInt32
      integral TypeInt64{}   = IR . OP_PtrInt64
      integral TypeWord{}    = IR . OP_PtrWord
      integral TypeWord8{}   = IR . OP_PtrWord8
      integral TypeWord16{}  = IR . OP_PtrWord16
      integral TypeWord32{}  = IR . OP_PtrWord32
      integral TypeWord64{}  = IR . OP_PtrWord64
      integral TypeCShort{}  = IR . OP_PtrCShort
      integral TypeCUShort{} = IR . OP_PtrCUShort
      integral TypeCInt{}    = IR . OP_PtrCInt
      integral TypeCUInt{}   = IR . OP_PtrCUInt
      integral TypeCLong{}   = IR . OP_PtrCLong
      integral TypeCULong{}  = IR . OP_PtrCULong
      integral TypeCLLong{}  = IR . OP_PtrCLLong
      integral TypeCULLong{} = IR . OP_PtrCULLong

      floating :: FloatingType a -> Operand (Ptr a) -> IR (Ptr a)
      floating TypeFloat{}   = IR . OP_PtrFloat
      floating TypeDouble{}  = IR . OP_PtrDouble
      floating TypeCFloat{}  = IR . OP_PtrCFloat
      floating TypeCDouble{} = IR . OP_PtrCDouble

  op' (ScalarPrimType t) = op' t
  op' (PtrPrimType s _)  = scalar s
    where
      scalar :: ScalarType a -> Operands (Ptr a) -> Operand (Ptr a)
      scalar (NonNumScalarType t) = nonnum t
      scalar (NumScalarType t)    = num t

      nonnum :: NonNumType a -> Operands (Ptr a) -> Operand (Ptr a)
      nonnum TypeBool{}   (OP_PtrBool   x) = x
      nonnum TypeChar{}   (OP_PtrChar   x) = x
      nonnum TypeCChar{}  (OP_PtrCChar  x) = x
      nonnum TypeCSChar{} (OP_PtrCSChar x) = x
      nonnum TypeCUChar{} (OP_PtrCUChar x) = x

      num :: NumType a -> Operands (Ptr a) -> Operand (Ptr a)
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType a -> Operands (Ptr a) -> Operand (Ptr a)
      integral TypeInt{}     (OP_PtrInt     x) = x
      integral TypeInt8{}    (OP_PtrInt8    x) = x
      integral TypeInt16{}   (OP_PtrInt16   x) = x
      integral TypeInt32{}   (OP_PtrInt32   x) = x
      integral TypeInt64{}   (OP_PtrInt64   x) = x
      integral TypeWord{}    (OP_PtrWord    x) = x
      integral TypeWord8{}   (OP_PtrWord8   x) = x
      integral TypeWord16{}  (OP_PtrWord16  x) = x
      integral TypeWord32{}  (OP_PtrWord32  x) = x
      integral TypeWord64{}  (OP_PtrWord64  x) = x
      integral TypeCShort{}  (OP_PtrCShort  x) = x
      integral TypeCUShort{} (OP_PtrCUShort x) = x
      integral TypeCInt{}    (OP_PtrCInt    x) = x
      integral TypeCUInt{}   (OP_PtrCUInt   x) = x
      integral TypeCLong{}   (OP_PtrCLong   x) = x
      integral TypeCULong{}  (OP_PtrCULong  x) = x
      integral TypeCLLong{}  (OP_PtrCLLong  x) = x
      integral TypeCULLong{} (OP_PtrCULLong x) = x

      floating :: FloatingType a -> Operands (Ptr a) -> Operand (Ptr a)
      floating TypeFloat{}   (OP_PtrFloat   x) = x
      floating TypeDouble{}  (OP_PtrDouble  x) = x
      floating TypeCFloat{}  (OP_PtrCFloat  x) = x
      floating TypeCDouble{} (OP_PtrCDouble x) = x

  ir' (ScalarPrimType t) = ir' t
  ir' (PtrPrimType s _)  = scalar s
    where
      scalar :: ScalarType a -> Operand (Ptr a) -> Operands (Ptr a)
      scalar (NonNumScalarType t) = nonnum t
      scalar (NumScalarType t)    = num t

      nonnum :: NonNumType a -> Operand (Ptr a) -> Operands (Ptr a)
      nonnum TypeBool{}   = OP_PtrBool
      nonnum TypeChar{}   = OP_PtrChar
      nonnum TypeCChar{}  = OP_PtrCChar
      nonnum TypeCSChar{} = OP_PtrCSChar
      nonnum TypeCUChar{} = OP_PtrCUChar

      num :: NumType a -> Operand (Ptr a) -> Operands (Ptr a)
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType a -> Operand (Ptr a) -> Operands (Ptr a)
      integral TypeInt{}     = OP_PtrInt
      integral TypeInt8{}    = OP_PtrInt8
      integral TypeInt16{}   = OP_PtrInt16
      integral TypeInt32{}   = OP_PtrInt32
      integral TypeInt64{}   = OP_PtrInt64
      integral TypeWord{}    = OP_PtrWord
      integral TypeWord8{}   = OP_PtrWord8
      integral TypeWord16{}  = OP_PtrWord16
      integral TypeWord32{}  = OP_PtrWord32
      integral TypeWord64{}  = OP_PtrWord64
      integral TypeCShort{}  = OP_PtrCShort
      integral TypeCUShort{} = OP_PtrCUShort
      integral TypeCInt{}    = OP_PtrCInt
      integral TypeCUInt{}   = OP_PtrCUInt
      integral TypeCLong{}   = OP_PtrCLong
      integral TypeCULong{}  = OP_PtrCULong
      integral TypeCLLong{}  = OP_PtrCLLong
      integral TypeCULLong{} = OP_PtrCULLong

      floating :: FloatingType a -> Operand (Ptr a) -> Operands (Ptr a)
      floating TypeFloat{}   = OP_PtrFloat
      floating TypeDouble{}  = OP_PtrDouble
      floating TypeCFloat{}  = OP_PtrCFloat
      floating TypeCDouble{} = OP_PtrCDouble

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

