{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : LLVM.General.AST.Type.Representation
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.General.AST.Type.Representation
  where

import Data.Bits
import Data.Int
import Data.Proxy
import Data.Word
import Foreign.C.Types

import GHC.Base
import GHC.Ptr
import GHC.TypeLits

import LLVM.General.AST.Type.Name

import qualified LLVM.General.AST.Type                          as LLVM
import qualified LLVM.General.AST.AddrSpace                     as LLVM
import qualified LLVM.General.AST.Name                          as LLVM


-- <http://llvm.org/docs/LangRef.html#type-system>
--
-- IsType
--   * void
--   * labels & metadata
--   * function types
--   * first class types (basic types)
--      * primitive types (single value types, things that go in registers)
--          * multi
--          * atomic
--              * int
--              * float
--              * ptr   (any first-class or function type)
--      * aggregate types
--          * (static) array
--          * [opaque] structure
--

-- | The 'IsType' class classifies all Haskell types that have a representation
-- in LLVM.
--
class IsType a where
  llvmType :: a -> LLVM.Type

instance IsType () where
  llvmType _ = LLVM.VoidType

instance IsType (Name a) where
  llvmType (Name s)   = LLVM.NamedTypeReference (LLVM.Name s)
  llvmType (UnName n) = LLVM.NamedTypeReference (LLVM.UnName n)


-- Function types
-- --------------
--
--   * http://hackage.haskell.org/package/ivory-0.1.0.0/docs/src/Ivory-Language-Proc.html#Proc
--
--   * How we do it with sharing recovery
--

class FunType a where
  funType :: a -> LLVM.Type


-- First-class types
-- -----------------

class BasicType a where
  basicType :: a -> LLVM.Type

instance BasicType a => IsType a where
  llvmType = basicType

instance SingleValueType a => BasicType a where
  basicType = singleValueType


-- Primitive types
-- ---------------

class SingleValueType a where
  singleValueType :: a -> LLVM.Type

instance AtomicType a => SingleValueType a where
  singleValueType = atomicType


class AtomicType a where
  atomicType :: a -> LLVM.Type

instance AtomicType Int8    where atomicType _ = LLVM.IntegerType 8
instance AtomicType Int16   where atomicType _ = LLVM.IntegerType 16
instance AtomicType Int32   where atomicType _ = LLVM.IntegerType 32
instance AtomicType Int64   where atomicType _ = LLVM.IntegerType 64
instance AtomicType Word8   where atomicType _ = LLVM.IntegerType 8
instance AtomicType Word16  where atomicType _ = LLVM.IntegerType 16
instance AtomicType Word32  where atomicType _ = LLVM.IntegerType 32
instance AtomicType Word64  where atomicType _ = LLVM.IntegerType 64
instance AtomicType Char    where atomicType _ = LLVM.IntegerType 32
instance AtomicType Bool    where atomicType _ = LLVM.IntegerType 1

instance AtomicType CShort  where atomicType _ = LLVM.IntegerType 16
instance AtomicType CUShort where atomicType _ = LLVM.IntegerType 16
instance AtomicType CInt    where atomicType _ = LLVM.IntegerType 32
instance AtomicType CUInt   where atomicType _ = LLVM.IntegerType 32
instance AtomicType CLLong  where atomicType _ = LLVM.IntegerType 64
instance AtomicType CULLong where atomicType _ = LLVM.IntegerType 64
instance AtomicType CChar   where atomicType _ = LLVM.IntegerType 8
instance AtomicType CUChar  where atomicType _ = LLVM.IntegerType 8
instance AtomicType CSChar  where atomicType _ = LLVM.IntegerType 8

instance AtomicType Float   where atomicType _ = LLVM.FloatingPointType 32 LLVM.IEEE
instance AtomicType CFloat  where atomicType _ = LLVM.FloatingPointType 32 LLVM.IEEE
instance AtomicType Double  where atomicType _ = LLVM.FloatingPointType 64 LLVM.IEEE
instance AtomicType CDouble where atomicType _ = LLVM.FloatingPointType 64 LLVM.IEEE

instance AtomicType Int  where
  atomicType _ = LLVM.IntegerType $( [| fromIntegral (finiteBitSize (undefined::Int)) |] )

instance AtomicType Word where
  atomicType _ = LLVM.IntegerType $( [| fromIntegral (finiteBitSize (undefined::Word)) |] )

instance AtomicType CLong where
  atomicType _ = LLVM.IntegerType $( [| fromIntegral (finiteBitSize (undefined::CLong)) |] )

instance AtomicType CULong where
  atomicType _ = LLVM.IntegerType $( [| fromIntegral (finiteBitSize (undefined::CULong)) |] )


-- Pointers to objects in memory. Pointers are primitive types, but can point to
-- non-primitive derived types such as SIMD vectors as well as primitive types.
--
instance BasicType t => AtomicType (Ptr t) where
  atomicType _ = LLVM.PointerType (basicType (undefined :: t)) (LLVM.AddrSpace 0)

-- instance FunType f => AtomicType (Ptr f) where
--   atomicType _ = LLVM.PointerType (funType (undefined :: f)) (LLVM.AddrSpace 0)


-- | A @'RegionPtr' r a@ is a pointer into a specific memory location 'a' in
-- address space 'r'. The default address space is number zero. The semantics of
-- non-zero address spaces are target specific.
--
type role RegionPtr representational representational
data RegionPtr r a = RegionPtr Addr#
  deriving (Eq, Ord)

class Region r where
  llvmAddrSpace :: r -> LLVM.AddrSpace

instance (Region r, BasicType t) => AtomicType (RegionPtr r t) where
  atomicType _ = LLVM.PointerType
                   (basicType (undefined :: t))
                   (llvmAddrSpace (undefined :: r))

ptrToRegionPtr :: Ptr a -> RegionPtr r a
ptrToRegionPtr (Ptr addr#) = RegionPtr addr#

regionPtrToPtr :: RegionPtr r a -> Ptr a
regionPtrToPtr (RegionPtr addr#) = Ptr addr#

-- data Shared
-- instance Region Shared where
--   llvmAddrSpace _ = LLVM.AddrSpace 3


-- SIMD vector types
--
data Multi :: Nat -> * -> * where       -- FIXME: SomeNat ??
  Multi :: (AtomicType a, KnownNat n, 1 <= n) => Multi n a

instance (AtomicType a, KnownNat n, 1 <= n) => SingleValueType (Multi n a) where
  singleValueType _ = LLVM.VectorType
                        (fromIntegral (natVal (Proxy :: Proxy n)))
                        (atomicType (undefined :: a))


-- Aggregate types
--

-- Static arrays are a simple derived type that arranges elements sequentially
-- in memory.
--
data Array :: Nat -> * -> * where       -- FIXME: SomeNat ??
  Array :: (BasicType a, KnownNat n) => Array n a

instance (KnownNat n, BasicType a) => BasicType (Array n a) where
  basicType _ = LLVM.ArrayType
                  (fromIntegral (natVal (Proxy :: Proxy n)))
                  (basicType (undefined :: a))

