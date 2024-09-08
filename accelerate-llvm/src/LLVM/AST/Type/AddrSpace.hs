{-# LANGUAGE DeriveDataTypeable    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.AddrSpace
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Pointers exist in a particular address space
--

module LLVM.AST.Type.AddrSpace (

  AddrSpace(..),
  defaultAddrSpace,

) where

import Data.Data
import Data.Word (Word32)


-- | <http://llvm.org/docs/LangRef.html#pointer-type>
--
-- Copied from llvm-hs-pure.
data AddrSpace = AddrSpace Word32
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | The default address space is number zero. The semantics of non-zero address
-- spaces are target dependent.
--
defaultAddrSpace :: AddrSpace
defaultAddrSpace = AddrSpace 0

