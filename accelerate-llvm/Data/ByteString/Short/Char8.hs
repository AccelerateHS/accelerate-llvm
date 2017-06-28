-- |
-- Module      : Data.ByteString.Short.Char8
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.ByteString.Short.Char8 (

  ShortByteString,
  unpack,

) where

import Data.ByteString.Short                                        ( ShortByteString )
import Prelude                                                      as P
import qualified Data.ByteString.Internal                           as BI
import qualified Data.ByteString.Short                              as BS


-- | /O(n)/ Convert a 'ShortByteString' into a list.
--
unpack :: ShortByteString -> [Char]
unpack = P.map BI.w2c . BS.unpack

