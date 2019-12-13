{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.AddrSpace
-- Copyright   : [2016..2019] The Accelerate Team
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

import LLVM.AST.AddrSpace


-- | The default address space is number zero. The semantics of non-zero address
-- spaces are target dependent.
--
defaultAddrSpace :: AddrSpace
defaultAddrSpace = AddrSpace 0

