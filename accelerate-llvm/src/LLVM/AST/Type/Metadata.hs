{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Metadata
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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

#if MIN_VERSION_llvm_hs_pure(6,1,0)
instance Downcast MetadataNode (LLVM.MDRef LLVM.MDNode) where
  downcast (MetadataNode n)            = LLVM.MDInline (downcast n)
  downcast (MetadataNodeReference r)   = LLVM.MDRef r

instance Downcast [Maybe Metadata] LLVM.MDNode where
  downcast = LLVM.MDTuple . map downcast
#else
instance Downcast MetadataNode LLVM.MetadataNode where
  downcast (MetadataNode n)            = LLVM.MetadataNode (downcast n)
  downcast (MetadataNodeReference r)   = LLVM.MetadataNodeReference r
#endif

