-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Environment
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Environment (

  module Data.Array.Accelerate.LLVM.Execute.Environment,
  module Data.Array.Accelerate.LLVM.PTX.Execute.Environment,

) where

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.Execute.Environment

type Val = ValR PTX

