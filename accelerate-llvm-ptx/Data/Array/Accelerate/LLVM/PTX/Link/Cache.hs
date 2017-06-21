-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Link.Cache
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

