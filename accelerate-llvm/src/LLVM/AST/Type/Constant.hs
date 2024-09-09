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
import LLVM.AST.Type.GetElementPtr
import LLVM.AST.Type.Name
import LLVM.AST.Type.Representation

import qualified Text.LLVM                                          as LLVM

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

  ConstantGetElementPtr :: GetElementPtr Constant (Ptr a) (Ptr b)
                        -> Constant (Ptr b)


-- | Convert to llvm-pretty
--
instance Downcast (Constant a) (LLVM.Typed LLVM.Value) where
  downcast = \case
    UndefConstant t             -> LLVM.Typed (downcast t) LLVM.ValUndef
    GlobalReference t n         -> LLVM.Typed (downcast t) (LLVM.ValSymbol (nameToPrettyS n))
    instr@(ConstantGetElementPtr (GEP t n i1 path)) ->
      LLVM.Typed (downcast (typeOf instr)) (LLVM.ValConstExpr (LLVM.ConstGEP inbounds Nothing (downcast t) (downcast n) (downcast i1 : downcast path)))
    BooleanConstant x           -> LLVM.Typed (LLVM.PrimType (LLVM.Integer 1)) (LLVM.ValInteger (toInteger (fromEnum x)))
    NullPtrConstant t           -> LLVM.Typed (downcast t) LLVM.ValNull
    ScalarConstant t x          -> scalar t x
    where
      scalar :: ScalarType s -> s -> LLVM.Typed LLVM.Value
      scalar (SingleScalarType s) = single s
      scalar (VectorScalarType s) = vector s

      single :: SingleType s -> s -> LLVM.Typed LLVM.Value
      single (NumSingleType s) = num s

      vector :: VectorType s -> s -> LLVM.Typed LLVM.Value
      vector (VectorType n s) (Vec ba#)
        = LLVM.Typed (LLVM.Vector (fromIntegral n) (downcast s))
        $ LLVM.ValVector (downcast s)
        $ map (LLVM.typedValue . single s)
        $ singlePrim s `withDict` foldrByteArray (:) [] (ByteArray ba#)

      num :: NumType s -> s -> LLVM.Typed LLVM.Value
      num (IntegralNumType s) v
        | IntegralDict <- integralDict s
        = LLVM.Typed (downcast s) (LLVM.ValInteger (fromIntegral v))

      num (FloatingNumType s) v
        = case s of
            TypeFloat                        -> LLVM.Typed (LLVM.PrimType (LLVM.FloatType LLVM.Float)) (LLVM.ValFloat v)
            TypeDouble                       -> LLVM.Typed (LLVM.PrimType (LLVM.FloatType LLVM.Double)) (LLVM.ValDouble v)
            TypeHalf | Half (CUShort _u) <- v ->
              -- LLVM.Typed (LLVM.PrimType (LLVM.FloatType LLVM.Half)) (_ _u)
              error "TODO: Half floats unsupported by llvm-pretty"

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
  typeOf (ConstantGetElementPtr gep)   = typeOf gep
