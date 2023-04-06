{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Constant
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Constant
  where

import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Name
import LLVM.AST.Type.Representation

import qualified LLVM.AST.Constant                                  as LLVM
import qualified LLVM.AST.Float                                     as LLVM
import qualified LLVM.AST.Type                                      as LLVM

import Data.Constraint
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Primitive.Vec


-- | Although constant expressions and instructions have many similarities,
-- there are important differences - so they're represented using different
-- types in this AST. At the cost of making it harder to move an code back and
-- forth between being constant and not, this approach embeds more of the rules
-- of what IR is legal into the Haskell types.
--
-- <http://llvm.org/docs/LangRef.html#constants>
--
-- <http://llvm.org/docs/LangRef.html#constant-expressions>
--
data Constant a where
  ScalarConstant        :: ScalarType a
                        -> a
                        -> Constant a

  BooleanConstant       :: Bool
                        -> Constant Bool

  UndefConstant         :: Type a
                        -> Constant a

  NullPtrConstant       :: Type (Ptr a)
                        -> Constant (Ptr a)

  GlobalReference       :: Type a
                        -> Name a
                        -> Constant a

  ConstantGetElementPtr :: ScalarType a
                        -> Constant (Ptr a)
                        -> [Constant i]
                        -> Constant (Ptr a)


-- | Convert to llvm-hs
--
instance Downcast (Constant a) LLVM.Constant where
  downcast = \case
    UndefConstant t             -> LLVM.Undef (downcast t)
#if MIN_VERSION_llvm_hs(15,0,0)
    GlobalReference _ n         -> LLVM.GlobalReference (downcast n)
    ConstantGetElementPtr t n i -> LLVM.GetElementPtr inbounds (downcast t) (downcast n) (downcast i)
#else
    GlobalReference t n         -> LLVM.GlobalReference (downcast t) (downcast n)
    ConstantGetElementPtr _ n i -> LLVM.GetElementPtr inbounds (downcast n) (downcast i)
#endif
    BooleanConstant x           -> LLVM.Int 1 (toInteger (fromEnum x))
    NullPtrConstant t           -> LLVM.Null (downcast t)
    ScalarConstant t x          -> scalar t x
    where
      scalar :: ScalarType s -> s -> LLVM.Constant
      scalar (SingleScalarType s) = single s
      scalar (VectorScalarType s) = vector s

      single :: SingleType s -> s -> LLVM.Constant
      single (NumSingleType s) = num s

      vector :: VectorType s -> s -> LLVM.Constant
      vector (VectorType _ s) (Vec ba#)
        = LLVM.Vector
        $ map (single s)
        $ singlePrim s `withDict` foldrByteArray (:) [] (ByteArray ba#)

      num :: NumType s -> s -> LLVM.Constant
      num (IntegralNumType s) v
        | IntegralDict <- integralDict s
        = LLVM.Int (LLVM.typeBits (downcast s)) (fromIntegral v)

      num (FloatingNumType s) v
        = LLVM.Float
        $ case s of
            TypeFloat                        -> LLVM.Single v
            TypeDouble                       -> LLVM.Double v
            TypeHalf | Half (CUShort u) <- v -> LLVM.Half u

      singlePrim :: SingleType s -> Dict (Prim s)
      singlePrim (NumSingleType s) = numPrim s

      numPrim :: NumType s -> Dict (Prim s)
      numPrim (IntegralNumType s) = integralPrim s
      numPrim (FloatingNumType s) = floatingPrim s

      integralPrim :: IntegralType s -> Dict (Prim s)
      integralPrim TypeInt    = Dict
      integralPrim TypeInt8   = Dict
      integralPrim TypeInt16  = Dict
      integralPrim TypeInt32  = Dict
      integralPrim TypeInt64  = Dict
      integralPrim TypeWord   = Dict
      integralPrim TypeWord8  = Dict
      integralPrim TypeWord16 = Dict
      integralPrim TypeWord32 = Dict
      integralPrim TypeWord64 = Dict

      floatingPrim :: FloatingType s -> Dict (Prim s)
      floatingPrim TypeHalf   = Dict
      floatingPrim TypeFloat  = Dict
      floatingPrim TypeDouble = Dict

      inbounds :: Bool
      inbounds = True

instance TypeOf Constant where
  typeOf (BooleanConstant _)           = type'
  typeOf (ScalarConstant t _)          = PrimType (ScalarPrimType t)
  typeOf (UndefConstant t)             = t
  typeOf (NullPtrConstant t)           = t
  typeOf (GlobalReference t _)         = t
  typeOf (ConstantGetElementPtr _ p _) = typeOf p

