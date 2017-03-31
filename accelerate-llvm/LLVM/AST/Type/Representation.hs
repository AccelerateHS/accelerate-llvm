{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Representation
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
  ScalarPrimType :: ScalarType a -> PrimType a
  PtrPrimType    :: PrimType a -> AddrSpace -> PrimType (Ptr a)   -- volatility?
  TupleType      :: TupleType (ProdRepr a) -> PrimType a          -- HAX: aggregate structures
  ArrayType      :: Word64 -> ScalarType a -> PrimType a          -- HAX: static array


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

instance IsType CShort where
  type' = PrimType primType

instance IsType CUShort where
  type' = PrimType primType

instance IsType CInt where
  type' = PrimType primType

instance IsType CUInt where
  type' = PrimType primType

instance IsType CLong where
  type' = PrimType primType

instance IsType CULong where
  type' = PrimType primType

instance IsType CLLong where
  type' = PrimType primType

instance IsType CULLong where
  type' = PrimType primType

instance IsType Float where
  type' = PrimType primType

instance IsType Double where
  type' = PrimType primType

instance IsType CFloat where
  type' = PrimType primType

instance IsType CDouble where
  type' = PrimType primType

instance IsType Bool where
  type' = PrimType primType

instance IsType Char where
  type' = PrimType primType

instance IsType CChar where
  type' = PrimType primType

instance IsType CSChar where
  type' = PrimType primType

instance IsType CUChar where
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

instance IsType (Ptr CShort) where
  type' = PrimType primType

instance IsType (Ptr CUShort) where
  type' = PrimType primType

instance IsType (Ptr CInt) where
  type' = PrimType primType

instance IsType (Ptr CUInt) where
  type' = PrimType primType

instance IsType (Ptr CLong) where
  type' = PrimType primType

instance IsType (Ptr CULong) where
  type' = PrimType primType

instance IsType (Ptr CLLong) where
  type' = PrimType primType

instance IsType (Ptr CULLong) where
  type' = PrimType primType

instance IsType (Ptr Float) where
  type' = PrimType primType

instance IsType (Ptr Double) where
  type' = PrimType primType

instance IsType (Ptr CFloat) where
  type' = PrimType primType

instance IsType (Ptr CDouble) where
  type' = PrimType primType

instance IsType (Ptr Bool) where
  type' = PrimType primType

instance IsType (Ptr Char) where
  type' = PrimType primType

instance IsType (Ptr CChar) where
  type' = PrimType primType

instance IsType (Ptr CSChar) where
  type' = PrimType primType

instance IsType (Ptr CUChar) where
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

instance IsPrim CShort where
  primType = ScalarPrimType scalarType

instance IsPrim CUShort where
  primType = ScalarPrimType scalarType

instance IsPrim CInt where
  primType = ScalarPrimType scalarType

instance IsPrim CUInt where
  primType = ScalarPrimType scalarType

instance IsPrim CLong where
  primType = ScalarPrimType scalarType

instance IsPrim CULong where
  primType = ScalarPrimType scalarType

instance IsPrim CLLong where
  primType = ScalarPrimType scalarType

instance IsPrim CULLong where
  primType = ScalarPrimType scalarType

instance IsPrim Float where
  primType = ScalarPrimType scalarType

instance IsPrim Double where
  primType = ScalarPrimType scalarType

instance IsPrim CFloat where
  primType = ScalarPrimType scalarType

instance IsPrim CDouble where
  primType = ScalarPrimType scalarType

instance IsPrim Bool where
  primType = ScalarPrimType scalarType

instance IsPrim Char where
  primType = ScalarPrimType scalarType

instance IsPrim CChar where
  primType = ScalarPrimType scalarType

instance IsPrim CSChar where
  primType = ScalarPrimType scalarType

instance IsPrim CUChar where
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

instance IsPrim (Ptr CShort) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CUShort) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CInt) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CUInt) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CLong) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CULong) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CLLong) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CULLong) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Float) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Double) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CFloat) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CDouble) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Bool) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr Char) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CChar) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CSChar) where
  primType = PtrPrimType primType defaultAddrSpace

instance IsPrim (Ptr CUChar) where
  primType = PtrPrimType primType defaultAddrSpace


instance Show (Type a) where
  show VoidType        = "()"
  show (PrimType t)    = show t

instance Show (PrimType a) where
  show (ScalarPrimType t)            = show t
  show (TupleType t)                 = show t
  show (ArrayType n t)               = printf "[%d x %s]" n (show t)
  show (PtrPrimType t (AddrSpace n)) = printf "Ptr%s %s" a p
    where
      p             = show t
      a | n == 0    = ""
        | otherwise = printf "[addrspace %d]" n
      -- p | PtrPrimType{} <- t  = printf "(%s)" (show t)
      --   | otherwise           = show t

