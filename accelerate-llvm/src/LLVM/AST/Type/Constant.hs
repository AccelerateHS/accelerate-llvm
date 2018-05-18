{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Constant
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

import Foreign.C.Types


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

  UndefConstant         :: Type a
                        -> Constant a

  GlobalReference       :: Type a
                        -> Name a
                        -> Constant a


-- | Convert to llvm-hs
--
instance Downcast (Constant a) LLVM.Constant where
  downcast = \case
    UndefConstant t       -> LLVM.Undef (downcast t)
    GlobalReference t n   -> LLVM.GlobalReference (downcast t) (downcast n)
    ScalarConstant t x    -> scalar t x
    where
      scalar :: ScalarType s -> s -> LLVM.Constant
      scalar (SingleScalarType s) = single s
      scalar (VectorScalarType s) = vector s

      single :: SingleType s -> s -> LLVM.Constant
      single (NumSingleType s)    = num s
      single (NonNumSingleType s) = nonnum s

      vector :: VectorType s -> s -> LLVM.Constant
      vector (Vector2Type s) (V2 a b)     = LLVM.Vector $ map (single s) [a,b]
      vector (Vector3Type s) (V3 a b c)   = LLVM.Vector $ map (single s) [a,b,c]
      vector (Vector4Type s) (V4 a b c d) = LLVM.Vector $ map (single s) [a,b,c,d]
      vector (Vector8Type s) (V8 a b c d e f g h) =
        LLVM.Vector $ map (single s) [a,b,c,d,e,f,g,h]
      vector (Vector16Type s) (V16 a b c d e f g h i j k l m n o p) =
        LLVM.Vector $ map (single s) [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]

      num :: NumType s -> s -> LLVM.Constant
      num (IntegralNumType s) v
        | IntegralDict <- integralDict s
        = LLVM.Int (LLVM.typeBits (downcast s)) (fromIntegral v)

      num (FloatingNumType s) v
        = LLVM.Float
        $ case s of
            TypeFloat{}                            -> LLVM.Single v
            TypeDouble{}                           -> LLVM.Double v
            TypeCDouble{} | CDouble u <- v         -> LLVM.Double u
            TypeCFloat{}  | CFloat u <- v          -> LLVM.Single u
            TypeHalf{}    | Half (CUShort u) <- v  -> LLVM.Half u

      nonnum :: NonNumType s -> s -> LLVM.Constant
      nonnum s v
        = LLVM.Int (LLVM.typeBits (downcast s))
        $ case s of
            TypeBool{}   -> fromIntegral (fromEnum v)
            TypeChar{}   -> fromIntegral (fromEnum v)
            TypeCChar{}  -> fromIntegral (fromEnum v)
            TypeCUChar{} -> fromIntegral (fromEnum v)
            TypeCSChar{} -> fromIntegral (fromEnum v)


instance TypeOf Constant where
  typeOf (ScalarConstant t _)  = PrimType (ScalarPrimType t)
  typeOf (UndefConstant t)     = t
  typeOf (GlobalReference t _) = t

