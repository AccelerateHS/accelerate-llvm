{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
-- |
-- Module      : LLVM.General.AST.Type.Bits
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.General.AST.Type.Bits (

  BitSize,
  BitSizeEq,
  (<)(),
  (>)(),

) where

import Data.Array.Accelerate.Type
import GHC.TypeLits

type a < b = (a + 1) <= b
type a > b = b <= (a + 1)

