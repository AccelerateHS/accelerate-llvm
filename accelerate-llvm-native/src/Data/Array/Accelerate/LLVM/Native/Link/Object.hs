-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.Object
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link.Object
  where

import Data.List
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr

import Data.ByteString.Short.Char8                                  ( ShortByteString, unpack )
import Data.Array.Accelerate.Lifetime


-- | The function table is a list of function names together with a pointer in
-- the target address space containing the corresponding executable code.
--
data FunctionTable  = FunctionTable { functionTable :: [Function] }
type Function       = (ShortByteString, FunPtr ())

instance Show FunctionTable where
  showsPrec _ f
    = showString "<<"
    . showString (intercalate "," [ unpack n | (n,_) <- functionTable f ])
    . showString ">>"

-- | Object code consists of memory in the target address space.
--
type ObjectCode     = Lifetime [Segment]
data Segment        = Segment {-# UNPACK #-} !Int                 -- size in bytes
                              {-# UNPACK #-} !(ForeignPtr Word8)  -- memory in target address space

