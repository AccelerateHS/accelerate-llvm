-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.Execute.Environment
-- Copyright   : [2014..2015] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Multi.Execute.Environment (

  Aval, aprj

) where

import Data.Array.Accelerate.LLVM.Multi.Target
import Data.Array.Accelerate.LLVM.Execute.Environment

type Aval = AvalR Multi

