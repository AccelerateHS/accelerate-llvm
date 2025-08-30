{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Metadata
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Metadata
  where

import LLVM.AST.Type.Downcast

import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty as LLVM

import Data.ByteString.Short                              ( ShortByteString )
import qualified Data.ByteString.Short.Char8              as SBS8


-- | Metadata does not have a type, and is not a value.
--
-- <http://llvm.org/docs/LangRef.html#metadata>
--
data MetadataNode
  = MetadataNode ![Maybe Metadata]
  | MetadataNodeReference {-# UNPACK #-} !Int

data Metadata
  = MetadataStringOperand {-# UNPACK #-} !ShortByteString
  | MetadataConstantOperand !(LLVM.Typed LLVM.Value)
  | MetadataNodeOperand !MetadataNode


-- | Convert to llvm-hs
--
instance Downcast Metadata LLVM.ValMd where
  downcast (MetadataStringOperand s)   = LLVM.ValMdString (SBS8.unpack s)
  downcast (MetadataConstantOperand o) = LLVM.ValMdValue o
  downcast (MetadataNodeOperand n)     = downcast n

instance Downcast MetadataNode LLVM.ValMd where
  downcast (MetadataNode n)            = LLVM.ValMdNode (downcast n)
  downcast (MetadataNodeReference r)   = LLVM.ValMdRef r
