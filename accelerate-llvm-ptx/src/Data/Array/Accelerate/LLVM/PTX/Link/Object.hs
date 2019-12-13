-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Link.Object
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Link.Object
  where

import Data.Array.Accelerate.Lifetime
import Data.ByteString.Short.Char8                                  ( ShortByteString, unpack )
import Data.List
import qualified Foreign.CUDA.Driver                                as CUDA


-- | The kernel function table is a list of the kernels implemented by a given
-- CUDA device module
--
data FunctionTable  = FunctionTable { functionTable :: [Kernel] }
data Kernel         = Kernel
  { kernelName                  :: {-# UNPACK #-} !ShortByteString
  , kernelFun                   :: {-# UNPACK #-} !CUDA.Fun
  , kernelSharedMemBytes        :: {-# UNPACK #-} !Int
  , kernelThreadBlockSize       :: {-# UNPACK #-} !Int
  , kernelThreadBlocks          :: (Int -> Int)
  }

instance Show FunctionTable where
  showsPrec _ f
    = showString "<<"
    . showString (intercalate "," [ unpack (kernelName k) | k <- functionTable f ])
    . showString ">>"

-- | Object code consists of executable code in the device address space
--
type ObjectCode = Lifetime CUDA.Module

