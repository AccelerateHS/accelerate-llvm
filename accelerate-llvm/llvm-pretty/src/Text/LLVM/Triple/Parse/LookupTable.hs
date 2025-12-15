{- |
Module      : Text.LLVM.Triple.Parse.LookupTable
Description : Helpers for parsing a finite set of strings.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental

This module is not exposed as part of the library API.
-}

{-# LANGUAGE TupleSections #-}

module Text.LLVM.Triple.Parse.LookupTable
  ( -- * Construction
    LookupTable
  , makeTable
  , enumTable
  , enumTableVariants
    -- * Queries
  , lookupBy
  , lookup
  , lookupByPrefix
  , lookupByWithDefault
  , lookupWithDefault
  , lookupByPrefixWithDefault
  , lookupBySuffixWithDefault
  ) where

import           Prelude hiding (lookup)
import qualified Data.Maybe as Maybe
import qualified Data.List as List

--------------------------------------------------------------------------------
-- Construction

-- | A table from constrant strings to values, to be used in parsing a finite
-- set of possibilities. Could be replaced by a fancier data structure, but
-- there's only one target triple per module, after all.
--
-- Hopefully, GHC is smart enough to construct these at compile-time.
newtype LookupTable a = LookupTable { getLookupTable :: [(String, a)] }

makeTable :: [(String, a)] -> LookupTable a
makeTable = LookupTable

enumTable :: Bounded a => Enum a => (a -> String) -> LookupTable a
enumTable prnt = makeTable [(prnt a, a) | a <- enumFrom minBound]

enumTableVariants :: Bounded a => Enum a => (a -> [String]) -> LookupTable a
enumTableVariants prnt =
  makeTable (concatMap (\a -> map (,a) (prnt a)) (enumFrom minBound))

--------------------------------------------------------------------------------
-- Queries

lookupBy :: (String -> Bool) -> LookupTable a -> Maybe a
lookupBy p = Maybe.listToMaybe . map snd . filter (p . fst) . getLookupTable

lookup :: String -> LookupTable a -> Maybe a
lookup s = lookupBy (== s)

lookupByPrefix :: String -> LookupTable a -> Maybe a
lookupByPrefix pfx = lookupBy (pfx `List.isPrefixOf`)

lookupByWithDefault :: a -> (String -> Bool) -> LookupTable a -> a
lookupByWithDefault def p = Maybe.fromMaybe def . lookupBy p

lookupWithDefault :: LookupTable a -> a -> String -> a
lookupWithDefault table def val = lookupByWithDefault def (== val) table

lookupByPrefixWithDefault :: LookupTable a -> a -> String -> a
lookupByPrefixWithDefault table def pfx =
  lookupByWithDefault def (pfx `List.isPrefixOf`) table

lookupBySuffixWithDefault :: LookupTable a -> a -> String -> a
lookupBySuffixWithDefault table def sfx =
  lookupByWithDefault def (sfx `List.isSuffixOf`) table
