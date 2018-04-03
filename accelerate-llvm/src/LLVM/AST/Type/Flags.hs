{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Flags
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Flags (

  NSW(..), NUW(..), FastMathFlags(..)

) where

import Data.Default.Class
import LLVM.AST.Instruction                               ( FastMathFlags(..) )


-- If the 'NoSignedWrap' or 'NoUnsignedWrap' keywords are present, the result
-- value of an operation is a poison value if signed and/or unsigned overflow,
-- respectively, occurs.
--
data NSW = NoSignedWrap   | SignedWrap
data NUW = NoUnsignedWrap | UnsignedWrap

instance Default NSW where
  def = SignedWrap

instance Default NUW where
  def = UnsignedWrap

instance Default FastMathFlags where
#if MIN_VERSION_llvm_hs_pure(6,0,0)
  def = FastMathFlags
          { allowReassoc    = True
          , noNaNs          = True
          , noInfs          = True
          , noSignedZeros   = True
          , allowReciprocal = True
          , allowContract   = True
          , approxFunc      = True
          }
#else
  def = UnsafeAlgebra -- allow everything
#endif
