-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Link.Cache
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Link.Cache (

  KernelTable,
  LC.new, LC.dlsym,

) where

import Data.Array.Accelerate.LLVM.PTX.Link.Object
import qualified Data.Array.Accelerate.LLVM.Link.Cache              as LC

type KernelTable = LC.LinkCache FunctionTable ObjectCode

