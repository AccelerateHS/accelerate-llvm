{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Type
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Type
  where

import LLVM.AST.Type.Representation
import Data.Array.Accelerate.Array.Sugar


-- | Extract some evidence that a reified type implies that type is a valid
-- element
--
data EltDict a where
  EltDict :: Elt a => EltDict a

singleElt :: SingleType a -> EltDict a
singleElt (NumSingleType    t) = numElt t
singleElt (NonNumSingleType t) = nonNumElt t

numElt :: NumType a -> EltDict a
numElt (IntegralNumType t) = integralElt t
numElt (FloatingNumType t) = floatingElt t

integralElt :: IntegralType a -> EltDict a
integralElt TypeInt{}     = EltDict
integralElt TypeInt8{}    = EltDict
integralElt TypeInt16{}   = EltDict
integralElt TypeInt32{}   = EltDict
integralElt TypeInt64{}   = EltDict
integralElt TypeWord{}    = EltDict
integralElt TypeWord8{}   = EltDict
integralElt TypeWord16{}  = EltDict
integralElt TypeWord32{}  = EltDict
integralElt TypeWord64{}  = EltDict
integralElt TypeCShort{}  = EltDict
integralElt TypeCUShort{} = EltDict
integralElt TypeCInt{}    = EltDict
integralElt TypeCUInt{}   = EltDict
integralElt TypeCLong{}   = EltDict
integralElt TypeCULong{}  = EltDict
integralElt TypeCLLong{}  = EltDict
integralElt TypeCULLong{} = EltDict

floatingElt :: FloatingType a -> EltDict a
floatingElt TypeHalf{}    = EltDict
floatingElt TypeFloat{}   = EltDict
floatingElt TypeDouble{}  = EltDict
floatingElt TypeCFloat{}  = EltDict
floatingElt TypeCDouble{} = EltDict

nonNumElt :: NonNumType a -> EltDict a
nonNumElt TypeBool{}   = EltDict
nonNumElt TypeChar{}   = EltDict
nonNumElt TypeCChar{}  = EltDict
nonNumElt TypeCSChar{} = EltDict
nonNumElt TypeCUChar{} = EltDict

