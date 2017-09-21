{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Plugin.Annotation
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Plugin.Annotation (

  Object(..),

) where

import Data.Data

data Object = Object FilePath
  deriving (Show, Data, Typeable)

