{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Environment
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Environment (

  module Data.Array.Accelerate.LLVM.Execute.Environment,
  module Data.Array.Accelerate.LLVM.Native.Execute.Environment,

) where

import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Execute.Environment

type Val = ValR Native

