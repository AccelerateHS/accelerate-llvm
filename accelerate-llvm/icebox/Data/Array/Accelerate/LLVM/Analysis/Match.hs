{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Analysis.Match
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Analysis.Match (

  module Data.Array.Accelerate.Analysis.Match,
  module Data.Array.Accelerate.LLVM.Analysis.Match,

) where

import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar

import Data.Typeable


-- Match reified shape types
--
matchShapeType
    :: forall sh sh'. (Shape sh, Shape sh')
    => sh
    -> sh'
    -> Maybe (sh :~: sh')
matchShapeType _ _
  | Just Refl <- matchTupleType (eltType (undefined::sh)) (eltType (undefined::sh'))
  = gcast Refl

matchShapeType _ _
  = Nothing

