{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Data.ByteString.Short.Extra
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.ByteString.Short.Extra (

  ShortByteString,
  take,

) where

import Data.ByteString.Short                                        ( ShortByteString )
import qualified Data.ByteString.Short                              as BS
import qualified Data.ByteString.Short.Internal                     as BI
import Prelude                                                      hiding ( take )

import GHC.ST
import GHC.Exts


-- | /O(n)/ @'take' n@ applied to the ShortByteString @xs@, returns the prefix
-- of @xs@ of length @n@ as a new ShortByteString, or @xs@ itself if
-- @n > 'length' xs@
--
take :: Int -> ShortByteString -> ShortByteString
take n xs
  | n >= BS.length xs = xs
  | otherwise         = runST $ do
      mba <- newByteArray n
      copyByteArray (asBA xs) 0 mba 0 n
      ba  <- unsafeFreezeByteArray mba
      return (asSBS ba)


------------------------------------------------------------------------
-- Internal utils

asBA :: ShortByteString -> BA
asBA (BI.SBS ba#) = BA# ba#

asSBS :: BA -> ShortByteString
asSBS (BA# ba#) = BI.SBS ba#


------------------------------------------------------------------------
-- Primop wrappers

data BA    = BA# ByteArray#
data MBA s = MBA# (MutableByteArray# s)

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# len#) =
    ST $ \s -> case newByteArray# len# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA# mba#) =
    ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', BA# ba# #)

copyByteArray :: BA -> Int -> MBA s -> Int -> Int -> ST s ()
copyByteArray (BA# src#) (I# src_off#) (MBA# dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyByteArray# src# src_off# dst# dst_off# len# s of
                 s' -> (# s', () #)

