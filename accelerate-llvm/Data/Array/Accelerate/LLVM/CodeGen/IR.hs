{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
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
data instance Operands a        = OP_Scalar (Operand a)
data instance Operands (a,b)    = OP_Pair (Operands a) (Operands b)


-- | Given some evidence that 'IR a' represents a scalar type, it can be
-- converted between the IR and Operand data types.
--
class IROP dict where
  op :: dict a -> IR a -> Operand a
  ir :: dict a -> Operand a -> IR a

instance IROP ScalarType where
  op (NumScalarType t)    = op t
  op (NonNumScalarType t) = op t
  --
  ir (NumScalarType t)    = ir t
  ir (NonNumScalarType t) = ir t

instance IROP NumType where
  op (IntegralNumType t) = op t
  op (FloatingNumType t) = op t
  --
  ir (IntegralNumType t) = ir t
  ir (FloatingNumType t) = ir t

instance IROP IntegralType where
  op (TypeInt     _) = unpack
  op (TypeInt8    _) = unpack
  op (TypeInt16   _) = unpack
  op (TypeInt32   _) = unpack
  op (TypeInt64   _) = unpack
  op (TypeWord    _) = unpack
  op (TypeWord8   _) = unpack
  op (TypeWord16  _) = unpack
  op (TypeWord32  _) = unpack
  op (TypeWord64  _) = unpack
  op (TypeCShort  _) = unpack
  op (TypeCUShort _) = unpack
  op (TypeCInt    _) = unpack
  op (TypeCUInt   _) = unpack
  op (TypeCLong   _) = unpack
  op (TypeCULong  _) = unpack
  op (TypeCLLong  _) = unpack
  op (TypeCULLong _) = unpack
  --
  ir (TypeInt     _) = pack
  ir (TypeInt8    _) = pack
  ir (TypeInt16   _) = pack
  ir (TypeInt32   _) = pack
  ir (TypeInt64   _) = pack
  ir (TypeWord    _) = pack
  ir (TypeWord8   _) = pack
  ir (TypeWord16  _) = pack
  ir (TypeWord32  _) = pack
  ir (TypeWord64  _) = pack
  ir (TypeCShort  _) = pack
  ir (TypeCUShort _) = pack
  ir (TypeCInt    _) = pack
  ir (TypeCUInt   _) = pack
  ir (TypeCLong   _) = pack
  ir (TypeCULong  _) = pack
  ir (TypeCLLong  _) = pack
  ir (TypeCULLong _) = pack

instance IROP FloatingType where
  op (TypeFloat   _) = unpack
  op (TypeDouble  _) = unpack
  op (TypeCFloat  _) = unpack
  op (TypeCDouble _) = unpack
  --
  ir (TypeFloat   _) = pack
  ir (TypeDouble  _) = pack
  ir (TypeCFloat  _) = pack
  ir (TypeCDouble _) = pack

instance IROP NonNumType where
  op (TypeBool   _) = unpack
  op (TypeChar   _) = unpack
  op (TypeCChar  _) = unpack
  op (TypeCSChar _) = unpack
  op (TypeCUChar _) = unpack
  --
  ir (TypeBool   _) = pack
  ir (TypeChar   _) = pack
  ir (TypeCChar  _) = pack
  ir (TypeCSChar _) = pack
  ir (TypeCUChar _) = pack

unpack :: (EltRepr a ~ a) => IR a -> Operand a
unpack (IR (OP_Scalar x)) = x

pack :: (EltRepr a ~ a) => Operand a -> IR a
pack x = IR (OP_Scalar x)

