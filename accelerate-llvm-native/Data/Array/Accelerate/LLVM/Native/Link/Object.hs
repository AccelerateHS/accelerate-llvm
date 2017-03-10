-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.Object
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link.Object
  where

import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr


-- | Object code consisting of a list of named function pointers to executable
-- code, a memory region in the target address space containing the actual
-- instructions.
--
data ObjectCode     = ObjectCode {-# UNPACK #-} !FunctionTable
                                 {-# UNPACK #-} !(ForeignPtr Word8)

data FunctionTable  = FunctionTable { functionTable :: [Function] }
type Function       = (String, FunPtr ())

