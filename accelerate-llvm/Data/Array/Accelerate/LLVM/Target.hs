{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Target
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
  targetTriple          :: t {- dummy -} -> Maybe ShortByteString
  targetDataLayout      :: t {- dummy -} -> Maybe DataLayout

