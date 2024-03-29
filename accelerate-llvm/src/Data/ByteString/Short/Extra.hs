{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.ByteString.Short.Extra
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.ByteString.Short.Extra (

  ShortByteString,
  take,
  takeWhile,
  liftSBS,

) where

import Data.ByteString.Short
import qualified Data.ByteString.Short                              as BS
import qualified Data.ByteString.Short.Internal                     as BI

import Language.Haskell.TH.Extra                                    ( CodeQ )
import qualified Language.Haskell.TH.Extra                          as TH

import System.IO.Unsafe
import Prelude                                                      hiding ( take, takeWhile )

#if !MIN_VERSION_bytestring(0,11,3)
import GHC.ST
import GHC.Word
#endif
import GHC.Exts


-- | Lift a ShortByteString into a Template Haskell splice
--
liftSBS :: ShortByteString -> CodeQ ShortByteString
liftSBS bs =
  let bytes = BS.unpack bs
      len   = BS.length bs
  in
  [|| unsafePerformIO $ BI.createFromPtr $$( TH.unsafeCodeCoerce [| Ptr $(TH.litE (TH.StringPrimL bytes)) |]) len ||]


#if !MIN_VERSION_bytestring(0,11,3)
-- | /O(n)/ @'take' n@ applied to the ShortByteString @xs@, returns the prefix
-- of @xs@ of length @n@ as a new ShortByteString, or @xs@ itself if
-- @n > 'length' xs@
--
{-# INLINEABLE take #-}
take :: Int -> ShortByteString -> ShortByteString
take n xs
  | n >= BS.length xs = xs
  | otherwise         = runST $ do
      mba <- newByteArray n
      copyByteArray (asBA xs) 0 mba 0 n
      ba  <- unsafeFreezeByteArray mba
      return (asSBS ba)

-- | 'takeWhile', applied to a predicate @p@ and a ShortByteString @xs@, returns
-- the longest prefix (possibly empty) of @xs@ of elements that satisfy @p@.
--
{-# INLINEABLE takeWhile #-}
takeWhile :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
takeWhile f ps = take (findIndexOrEnd (not . f) ps) ps


-- | Return the index of the first element satisfying the predicate, otherwise
-- return the length of the string if no such element is found.
--
{-# INLINEABLE findIndexOrEnd #-}
findIndexOrEnd :: (Word8 -> Bool) -> ShortByteString -> Int
findIndexOrEnd p xs = go 0
  where
    !ba = asBA xs
    !n  = BS.length xs
    --
    go !i | i >= n                   = i
          | p (indexWord8Array ba i) = i
          | otherwise                = go (i+1)


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

indexWord8Array :: BA -> Int -> Word8
indexWord8Array (BA# ba#) (I# i#) = W8# (indexWord8Array# ba# i#)

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
#endif

