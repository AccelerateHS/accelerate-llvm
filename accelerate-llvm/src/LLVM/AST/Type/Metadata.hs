{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Metadata
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Metadata
  where

import LLVM.AST.Type.Downcast

import qualified LLVM.AST.Constant                        as LLVM
import qualified LLVM.AST.Operand                         as LLVM

import Data.ByteString.Short                              ( ShortByteString )


-- | Metadata does not have a type, and is not a value.
--
-- <http://llvm.org/docs/LangRef.html#metadata>
--
data MetadataNode
  = MetadataNode ![Maybe Metadata]
  | MetadataNodeReference {-# UNPACK #-} !LLVM.MetadataNodeID

data Metadata
  = MetadataStringOperand {-# UNPACK #-} !ShortByteString
  | MetadataConstantOperand !LLVM.Constant
  | MetadataNodeOperand !MetadataNode


-- | Convert to llvm-hs
--
instance Downcast Metadata LLVM.Metadata where
  downcast (MetadataStringOperand s)   = LLVM.MDString s
  downcast (MetadataConstantOperand o) = LLVM.MDValue (LLVM.ConstantOperand o)
  downcast (MetadataNodeOperand n)     = LLVM.MDNode (downcast n)

instance Downcast MetadataNode LLVM.MetadataNode where
  downcast (MetadataNode n)            = LLVM.MetadataNode (downcast n)
  downcast (MetadataNodeReference r)   = LLVM.MetadataNodeReference r

