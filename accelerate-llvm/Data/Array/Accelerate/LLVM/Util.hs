-- |
-- Module      : Data.Array.Accelerate.LLVM.Util
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Generate types for the reified elements of an array computation
--

module Data.Array.Accelerate.LLVM.Util
  where

-- standard library
import Data.Word
import qualified Data.Bits as B


-- | The number of bits in a type
--
{-# INLINE bitSize #-}
bitSize :: B.Bits a => a -> Word32
bitSize x = fromIntegral (B.bitSize x)


-- | Convert a boolean value into an integral value, where False is zero and
-- True is one.
--
{-# INLINE fromBool #-}
fromBool :: Integral i => Bool -> i
fromBool True  = 1
fromBool False = 0

-- | Convert an integral value into a boolean. We follow the C convention, where
-- zero is False and all other values represent True.
--
{-# INLINE toBool #-}
toBool :: Integral i => i -> Bool
toBool 0 = False
toBool _ = True

