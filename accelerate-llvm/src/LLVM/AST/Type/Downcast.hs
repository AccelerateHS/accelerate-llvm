{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Downcast
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Downcast (

  Downcast(..)

) where

import Data.Bits
import Foreign.C.Types

import Data.Array.Accelerate.Type
import qualified LLVM.AST.Type                                      as LLVM


-- | Convert a value from our representation of the LLVM AST which uses
-- Haskell-level types, into the llvm-hs representation where types are
-- represented only at the value level.
--
-- The type-level information to generate the appropriate value-level types.
--
class Downcast typed untyped where
  downcast :: typed -> untyped

instance Downcast a a' => Downcast [a] [a'] where
  downcast = map downcast

instance Downcast a a' => Downcast (Maybe a) (Maybe a') where
  downcast Nothing  = Nothing
  downcast (Just x) = Just (downcast x)

instance (Downcast a a', Downcast b b') => Downcast (a,b) (a',b') where
  downcast (a,b) = (downcast a, downcast b)

instance (Downcast a a', Downcast b b') =>  Downcast (Either a b) (Either a' b') where
  downcast (Left a)  = Left (downcast a)
  downcast (Right b) = Right (downcast b)


instance Downcast (ScalarType a) LLVM.Type where
  downcast (SingleScalarType t) = downcast t
  downcast (VectorScalarType t) = downcast t

instance Downcast (SingleType a) LLVM.Type where
  downcast (NumSingleType t)    = downcast t
  downcast (NonNumSingleType t) = downcast t

instance Downcast (VectorType a) LLVM.Type where
  downcast (Vector2Type t)  = LLVM.VectorType 2 (downcast t)
  downcast (Vector3Type t)  = LLVM.VectorType 3 (downcast t)
  downcast (Vector4Type t)  = LLVM.VectorType 4 (downcast t)
  downcast (Vector8Type t)  = LLVM.VectorType 8 (downcast t)
  downcast (Vector16Type t) = LLVM.VectorType 16 (downcast t)

instance Downcast (BoundedType t) LLVM.Type where
  downcast (IntegralBoundedType t) = downcast t
  downcast (NonNumBoundedType t)   = downcast t

instance Downcast (NumType a) LLVM.Type where
  downcast (IntegralNumType t) = downcast t
  downcast (FloatingNumType t) = downcast t

instance Downcast (IntegralType a) LLVM.Type where
  downcast TypeInt{}     = LLVM.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: Int)) |] )
  downcast TypeInt8{}    = LLVM.IntegerType 8
  downcast TypeInt16{}   = LLVM.IntegerType 16
  downcast TypeInt32{}   = LLVM.IntegerType 32
  downcast TypeInt64{}   = LLVM.IntegerType 64
  downcast TypeWord{}    = LLVM.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: Word)) |] )
  downcast TypeWord8{}   = LLVM.IntegerType 8
  downcast TypeWord16{}  = LLVM.IntegerType 16
  downcast TypeWord32{}  = LLVM.IntegerType 32
  downcast TypeWord64{}  = LLVM.IntegerType 64
  downcast TypeCShort{}  = LLVM.IntegerType 16
  downcast TypeCUShort{} = LLVM.IntegerType 16
  downcast TypeCInt{}    = LLVM.IntegerType 32
  downcast TypeCUInt{}   = LLVM.IntegerType 32
  downcast TypeCLong{}   = LLVM.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: CLong)) |] )
  downcast TypeCULong{}  = LLVM.IntegerType $( [| fromIntegral (finiteBitSize (undefined :: CULong)) |] )
  downcast TypeCLLong{}  = LLVM.IntegerType 64
  downcast TypeCULLong{} = LLVM.IntegerType 64

instance Downcast (FloatingType a) LLVM.Type where
  downcast TypeHalf{}    = LLVM.FloatingPointType LLVM.HalfFP
  downcast TypeFloat{}   = LLVM.FloatingPointType LLVM.FloatFP
  downcast TypeDouble{}  = LLVM.FloatingPointType LLVM.DoubleFP
  downcast TypeCFloat{}  = LLVM.FloatingPointType LLVM.FloatFP
  downcast TypeCDouble{} = LLVM.FloatingPointType LLVM.DoubleFP

instance Downcast (NonNumType a) LLVM.Type where
  downcast TypeBool{}   = LLVM.IntegerType 1
  downcast TypeChar{}   = LLVM.IntegerType 32
  downcast TypeCChar{}  = LLVM.IntegerType 8
  downcast TypeCSChar{} = LLVM.IntegerType 8
  downcast TypeCUChar{} = LLVM.IntegerType 8

