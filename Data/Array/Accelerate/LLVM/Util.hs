-- |
-- Module      : Data.Array.Accelerate.LLVM.Util
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Generate types for the reified elements of an array computation
--

module Data.Array.Accelerate.LLVM.Util
  where


import qualified Data.Bits as B


{-# INLINE bitSize #-}
bitSize :: (B.Bits a, Integral i) => a -> i
bitSize x = fromIntegral (B.bitSize x)

