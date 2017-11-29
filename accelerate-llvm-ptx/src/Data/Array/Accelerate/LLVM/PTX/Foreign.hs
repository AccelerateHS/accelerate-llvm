{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Foreign
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Foreign (

  -- Foreign functions
  ForeignAcc(..),
  ForeignExp(..),

  -- useful re-exports
  LLVM,
  PTX(..),
  Context(..),
  liftIO,
  withDevicePtr,
  module Data.Array.Accelerate.LLVM.PTX.Array.Data,
  module Data.Array.Accelerate.LLVM.PTX.Execute.Async,

) where

import qualified Data.Array.Accelerate.Array.Sugar                  as S

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.Foreign
import Data.Array.Accelerate.LLVM.PTX.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Array.Prim
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import Data.Array.Accelerate.LLVM.PTX.Target

import Control.Monad.State
import Data.Typeable


instance Foreign PTX where
  foreignAcc _ (ff :: asm (a -> b))
    | Just (ForeignAcc _ asm :: ForeignAcc (a -> b)) <- cast ff = Just asm
    | otherwise                                                 = Nothing

  foreignExp _ (ff :: asm (x -> y))
    | Just (ForeignExp _ asm :: ForeignExp (x -> y)) <- cast ff = Just asm
    | otherwise                                                 = Nothing

instance S.Foreign ForeignAcc where
  strForeign (ForeignAcc s _) = s

instance S.Foreign ForeignExp where
  strForeign (ForeignExp s _) = s


-- Foreign functions in the PTX backend.
--
data ForeignAcc f where
  ForeignAcc :: String
             -> (Stream -> a -> LLVM PTX b)
             -> ForeignAcc (a -> b)

-- Foreign expressions in the PTX backend.
--
data ForeignExp f where
  ForeignExp :: String
             -> IRFun1 PTX () (x -> y)
             -> ForeignExp (x -> y)

deriving instance Typeable ForeignAcc
deriving instance Typeable ForeignExp

