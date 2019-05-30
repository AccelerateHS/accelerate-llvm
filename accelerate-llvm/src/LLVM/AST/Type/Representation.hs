{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Representation
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Representation (

  module LLVM.AST.Type.Representation,
  module Data.Array.Accelerate.Type,
  Ptr,
  AddrSpace(..),

) where

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Product

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Downcast

import qualified LLVM.AST.Type                                      as LLVM

import Foreign.Ptr
import Text.Printf


-- Witnesses to observe the LLVM type hierarchy:
--
-- <http://llvm.org/docs/LangRef.html#type-system>
--
-- Type
--   * void
--   * labels & metadata
--   * function types
--   * first class types (basic types)
--      * primitive types (single value types, things that go in registers)
--          * multi (SIMD vectors of primitive types: pointer and single values)
--          * single value types
--              * int
--              * float
--              * ptr (any first-class or function type)
--      * aggregate types
--          * (static) array
--          * [opaque] structure
--
-- We actually don't want to encode this hierarchy as shown above, since it is
-- not precise enough for our purposes. For example, the `Add` instruction
-- operates on operands of integer type or vector (multi) of integer types, so
-- we would probably prefer to add multi-types as a sub-type of IntegralType,
-- FloatingType, etc.
--
-- We minimally extend Accelerate's existing type hierarchy to support the
-- features we require for code generation: void types, pointer types, and
-- simple aggregate structures (for CmpXchg).
--

data Type a where
  VoidType  :: Type ()
  PrimType  :: PrimType a -> Type a

data PrimType a where
  ScalarPrimType  :: ScalarType a            -> PrimType a          -- scalar value types (things in registers)
  PtrPrimType     :: PrimType a -> AddrSpace -> PrimType (Ptr a)    -- pointers (XXX: volatility?)
  StructPrimType  :: TupleType (ProdRepr a)  -> PrimType a          -- opaque structures (required for CmpXchg)
  ArrayPrimType   :: Word64 -> ScalarType a  -> PrimType a          -- static arrays

-- | All types
--

class IsType a where
  type' :: Type a

instance IsType () where
  type' = VoidType

instance IsType Int where
  type' = PrimType primType

instance IsType Int8 where
  type' = PrimType primType

instance IsType Int16 where
  type' = PrimType primType

instance IsType Int32 where
  type' = PrimType primType

instance IsType Int64 where
  type' = PrimType primType

instance IsType Word where
  type' = PrimType primType

instance IsType Word8 where
  type' = PrimType primType

instance IsType Word16 where
  type' = PrimType primType

instance IsType Word32 where
  type' = PrimType primType

instance IsType Word64 where
  type' = PrimType primType

instance IsType Half where
  type' = PrimType primType

instance IsType Float where
  type' = PrimType primType

instance IsType Double where
  type' = PrimType primType

instance IsType Bool where
  type' = PrimType primType

instance IsType Char where
  type' = PrimType primType

instance IsType (Ptr Int) where
  type' = PrimType primType

instance IsType (Ptr Int8) where
  type' = PrimType primType

instance IsType (Ptr Int16) where
  type' = PrimType primType

instance IsType (Ptr Int32) where
  type' = PrimType primType

instance IsType (Ptr Int64) where
  type' = PrimType primType

instance IsType (Ptr Word) where
  type' = PrimType primType

instance IsType (Ptr Word8) where
  type' = PrimType primType

instance IsType (Ptr Word16) where
  type' = PrimType primType

instance IsType (Ptr Word32) where
  type' = PrimType primType

instance IsType (Ptr Word64) where
  type' = PrimType primType

instance IsType (Ptr Float) where
  type' = PrimType primType

instance IsType (Ptr Double) where
  type' = PrimType primType

instance IsType (Ptr Bool) where
  type' = PrimType primType

instance IsType (Ptr Char) where
  type' = PrimType primType

-- | All primitive types
--

class IsPrim a where
  primType :: PrimType a

instance IsPrim Int where
  primType = ScalarPrimType scalarType

instance IsPrim Int8 where
  primType = ScalarPrimType scalarType

instance IsPrim Int16 where
  primType = ScalarPrimType scalarType

instance IsPrim Int32 where
  primType = ScalarPrimType scalarType

instance IsPrim Int64 where
  primType = ScalarPrimType scalarType

instance IsPrim Word where
  primType = ScalarPrimType scalarType

instance IsPrim Word8 where
  primType = ScalarPrimType scalarType

instance IsPrim Word16 where
  primType = ScalarPrimType scalarType

instance IsPrim Word32 where
  primType = ScalarPrimType scalarType

instance IsPrim Word64 where
  primType = ScalarPrimType scalarType

instance IsPrim Half where
  primType = ScalarPrimType scalarType

instance IsPrim Float where
  primType = ScalarPrimType scalarType

instance IsPrim Double where
  primType = ScalarPrimType scalarType

instance IsPrim Bool where
  primType = ScalarPrimType scalarType

instance IsPrim Char where
  primType = ScalarPrimType scalarType

instance IsPrim (Ptr Int) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Int8) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Int16) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Int32) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Int64) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Word) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Word8) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Word16) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Word32) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Word64) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Half) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Float) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Double) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Bool) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Char) where
  primType = PtrPrimType primType defaultAddrSpace

instance Show (Type a) where
  show VoidType        = "()"
  show (PrimType t)    = show t

instance Show (PrimType a) where
  show (ScalarPrimType t)             = show t
  show (StructPrimType t)             = show t
  show (ArrayPrimType n t)            = printf "[%d x %s]" n (show t)
  show (PtrPrimType t (AddrSpace n))  = printf "Ptr%s %s" a p
    where
      p             = show t
      a | n == 0    = ""
        | otherwise = printf "[addrspace %d]" n
      -- p | PtrPrimType{} <- t  = printf "(%s)" (show t)
      --   | otherwise           = show t


-- | Does the concrete type represent signed or unsigned values?
--
class IsSigned dict where
  signed   :: dict a -> Bool
  signed   = not . unsigned
  --
  unsigned :: dict a -> Bool
  unsigned = not . signed

instance IsSigned ScalarType where
  signed (SingleScalarType t) = signed t
  signed (VectorScalarType t) = signed t

instance IsSigned SingleType where
  signed (NumSingleType t)    = signed t
  signed (NonNumSingleType t) = signed t

instance IsSigned VectorType where
  signed (VectorType _ t) = signed t

instance IsSigned BoundedType where
  signed (IntegralBoundedType t) = signed t
  signed (NonNumBoundedType t)   = signed t

instance IsSigned NumType where
  signed (IntegralNumType t) = signed t
  signed (FloatingNumType t) = signed t

instance IsSigned IntegralType where
  signed = \case
    TypeInt{}     -> True
    TypeInt8{}    -> True
    TypeInt16{}   -> True
    TypeInt32{}   -> True
    TypeInt64{}   -> True
    _             -> False

instance IsSigned FloatingType where
  signed _ = True

instance IsSigned NonNumType where
  signed = \case
    TypeBool{}    -> False
    TypeChar{}    -> False


-- | Recover the type of a container
--
class TypeOf f where
  typeOf :: f a -> Type a


-- | Convert to llvm-hs
--
instance Downcast (Type a) LLVM.Type where
  downcast VoidType     = LLVM.VoidType
  downcast (PrimType t) = downcast t

instance Downcast (PrimType a) LLVM.Type where
  downcast (ScalarPrimType t)   = downcast t
  downcast (PtrPrimType t a)    = LLVM.PointerType (downcast t) a
  downcast (ArrayPrimType n t)  = LLVM.ArrayType n (downcast t)
  downcast (StructPrimType t)   = LLVM.StructureType False (go t)
    where
      go :: TupleType t -> [LLVM.Type]
      go TypeRunit         = []
      go (TypeRscalar s)   = [downcast s]
      go (TypeRpair ta tb) = go ta ++ go tb

