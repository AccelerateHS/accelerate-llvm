-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.Cache
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link.Cache (

  LinkCache,
  LC.new, LC.dlsym,

) where

import Data.Array.Accelerate.LLVM.Native.Link.Object
import qualified Data.Array.Accelerate.LLVM.Link.Cache              as LC

type LinkCache = LC.LinkCache FunctionTable ObjectCode

