{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.InlineAssembly
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.InlineAssembly (

  module LLVM.AST.Type.InlineAssembly,
  LLVM.Dialect(..),

) where

import LLVM.AST.Type.Downcast

import qualified LLVM.AST.Type                                      as LLVM
import qualified LLVM.AST.InlineAssembly                            as LLVM

import Data.ByteString
import Data.ByteString.Short


-- | The 'call' instruction might be a label or inline assembly
--
data InlineAssembly where
  InlineAssembly :: ByteString            -- assembly
                 -> ShortByteString       -- constraints
                 -> Bool                  -- has side effects?
                 -> Bool                  -- align stack?
                 -> LLVM.Dialect
                 -> InlineAssembly

instance Downcast (LLVM.Type, InlineAssembly) LLVM.InlineAssembly where
  downcast (t, InlineAssembly asm cst s a d) =
    LLVM.InlineAssembly t asm cst s a d

