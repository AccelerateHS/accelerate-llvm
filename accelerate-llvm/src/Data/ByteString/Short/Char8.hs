{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.ByteString.Short.Char8
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.ByteString.Short.Char8 (

  ShortByteString,
  pack,
  unpack,
  takeWhile,

) where

import Data.ByteString.Short                                        ( ShortByteString )
import Prelude                                                      as P hiding ( takeWhile )
import qualified Data.ByteString.Internal                           as BI
import qualified Data.ByteString.Short                              as BS
import qualified Data.ByteString.Short.Extra                        as BS


-- | /O(n)/ Convert a 'ShortByteString' into a list.
--
{-# INLINEABLE unpack #-}
unpack :: ShortByteString -> [Char]
unpack = P.map BI.w2c . BS.unpack

-- | /O(n)/ Convert a 'String' into a 'ShortByteString'.
--
{-# INLINEABLE pack #-}
pack :: String -> ShortByteString
pack = BS.pack . P.map BI.c2w

-- | 'takeWhile', applied to a predicate @p@ and a ShortByteString @xs@, returns
-- the longest prefix (possibly empty) of @xs@ of elements that satisfy @p@.
--
{-# INLINEABLE takeWhile #-}
takeWhile :: (Char -> Bool) -> ShortByteString -> ShortByteString
takeWhile f = BS.takeWhile (f . BI.w2c)

