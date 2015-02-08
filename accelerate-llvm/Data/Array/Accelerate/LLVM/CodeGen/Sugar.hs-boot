{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RoleAnnotations #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Sugar-boot
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Sugar
  where

type role IROpenAcc phantom phantom phantom     -- temporary
data IROpenAcc arch aenv a where
  IROpenAcc :: () {- ??? -}
            -> IROpenAcc arch aenv a

