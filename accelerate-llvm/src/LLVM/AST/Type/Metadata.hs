{-# LANGUAGE GADTs #-}
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

