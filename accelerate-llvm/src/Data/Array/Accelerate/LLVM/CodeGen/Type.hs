{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Type
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Type
  where

import LLVM.AST.Type.Representation
import Data.Array.Accelerate.Array.Sugar

import Data.Constraint


-- | Extract some evidence that a reified type implies that type is a valid
-- element
--
singleElt :: SingleType a -> Dict (Elt a)
singleElt (NumSingleType    t) = numElt t
singleElt (NonNumSingleType t) = nonNumElt t

numElt :: NumType a -> Dict (Elt a)
numElt (IntegralNumType t) = integralElt t
numElt (FloatingNumType t) = floatingElt t

integralElt :: IntegralType a -> Dict (Elt a)
integralElt TypeInt{}    = Dict
integralElt TypeInt8{}   = Dict
integralElt TypeInt16{}  = Dict
integralElt TypeInt32{}  = Dict
integralElt TypeInt64{}  = Dict
integralElt TypeWord{}   = Dict
integralElt TypeWord8{}  = Dict
integralElt TypeWord16{} = Dict
integralElt TypeWord32{} = Dict
integralElt TypeWord64{} = Dict

floatingElt :: FloatingType a -> Dict (Elt a)
floatingElt TypeHalf{}   = Dict
floatingElt TypeFloat{}  = Dict
floatingElt TypeDouble{} = Dict

nonNumElt :: NonNumType a -> Dict (Elt a)
nonNumElt TypeBool{} = Dict
nonNumElt TypeChar{} = Dict

