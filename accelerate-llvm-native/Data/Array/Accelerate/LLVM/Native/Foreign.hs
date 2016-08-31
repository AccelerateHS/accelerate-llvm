{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Foreign
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Foreign (

  -- Foreign functions
  ForeignAcc(..),
  ForeignExp(..),

  -- useful re-exports
  Native,
  liftIO,

) where

import qualified Data.Array.Accelerate.Array.Sugar                  as S

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.Foreign
import Data.Array.Accelerate.LLVM.Native.Target

import Control.Monad.State
import Data.Typeable


instance Foreign Native where
  foreignAcc _ (ff :: asm (a -> b)) =
    case cast ff of
      Just (ForeignAcc _ asm :: ForeignAcc (a -> b)) -> Just (const asm)
      Nothing                                        -> Nothing

  foreignExp _ (ff :: asm (x -> y)) =
    case cast ff of
      Just (ForeignExp _ asm :: ForeignExp (x -> y)) -> Just asm
      Nothing                                        -> Nothing


instance S.Foreign ForeignAcc where
  strForeign (ForeignAcc s _) = s

instance S.Foreign ForeignExp where
  strForeign (ForeignExp s _) = s


-- Foreign functions in the Native backend.
--
-- This is just some arbitrary monadic computation.
--
data ForeignAcc f where
  ForeignAcc :: String
             -> (a -> LLVM Native b)
             -> ForeignAcc (a -> b)

-- Foreign expressions in the Native backend.
--
-- I'm not sure how useful this is; perhaps we want a way to splice in an
-- arbitrary llvm-general term, which would give us access to instructions not
-- currently encoded in Accelerate (i.e. SIMD operations, struct types, etc.)
--
data ForeignExp f where
  ForeignExp :: String
             -> IRFun1 Native () (x -> y)
             -> ForeignExp (x -> y)

