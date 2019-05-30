{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Target
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Target
  where

import LLVM.AST.DataLayout                                ( DataLayout )
import Data.ByteString.Short                              ( ShortByteString )


-- | Describes some target specific information needed for code generation
--
class Target t where
  targetTriple      :: Maybe ShortByteString
  targetDataLayout  :: Maybe DataLayout

