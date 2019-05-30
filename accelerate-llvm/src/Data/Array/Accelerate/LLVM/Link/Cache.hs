{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Link.Cache
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Link.Cache (

  LinkCache,
  new, dlsym,

) where

import Data.Array.Accelerate.Debug
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.LLVM.Compile.Cache

import Control.Monad
import Control.Concurrent.MVar
import Data.Map.Strict                                              ( Map )
import Prelude                                                      hiding ( lookup )
import Text.Printf
import qualified Data.Map.Strict                                    as Map


-- Simple reference-counted linker cache for function tables 'f' implemented by
-- object code 'o'.
--
data LinkCache f o = LinkCache {-# UNPACK #-} !(MVar (Map UID (Entry f o)))
data Entry f o     = Entry {-# UNPACK #-} !Int !f !o


-- Create a new linker cache
--
new :: IO (LinkCache f o)
new = LinkCache `liftM` newMVar Map.empty


-- Return the binding addresses for the given kernel functions (by key). If the
-- functions do not already exist in the cache, the supplied continuation will
-- be run in order to generate them. This happens as a single atomic step; thus
-- the cache is thread safe.
--
dlsym :: UID -> LinkCache f o -> IO (f,o) -> IO (Lifetime f)
dlsym key cache@(LinkCache var) k = do
  modifyMVar var $ \m ->
    case Map.lookup key m of
      -- Run the supplied function to generate the object code and add to the cache
      Nothing -> do
        (f,o)  <- k
        ticket <- issue key f cache
        return ( Map.insert key (Entry 1 f o) m, ticket )

      -- Return the existing object code
      Just (Entry c f o) -> do
        ticket <- issue key f cache
        return ( Map.insert key (Entry (c+1) f o) m, ticket )


{--
-- Insert the given function table and object code into the cache. The returned
-- value must be kept alive for as long as you need the object code to live;
-- linker table entries are removed once all tickets referring to them are
-- GC'ed.
--
-- NOTE: It is an error if the entry already exists in the table. Thus, there is
-- a potential race condition between 'lookup' and 'insert'. On collision, it
-- would be fine to return a reference to the existing implementation instead
-- and discard the input values, but 'dlsym' solves this anyway.
--
insert :: Int -> f -> o -> LinkCache f o -> IO (Lifetime f)
insert key functionTable objectCode cache@(LinkCache var) = do
  ticket <- issue key functionTable cache
  modifyMVar_ var $ \m ->
    let collision = $internalError "insert" "duplicate entry"
    in  return $! Map.insertWith collision key (Entry 1 functionTable objectCode) m
  --
  return ticket


-- Check the linker cache for the given functions; if found return the
-- corresponding function table.
--
lookup :: Int -> LinkCache f o -> IO (Maybe (Lifetime f))
lookup key cache@(LinkCache var) = do
  modifyMVar var $ \m ->
    case Map.lookup key m of
      Nothing             -> return (m, Nothing)
      Just (Entry c f o)  -> do
        ticket <- issue key f cache
        return ( Map.insert key (Entry (c+1) f o) m, Just ticket )
--}


-- Issue a new ticket for the given table key/function table. When the returned
-- lifetime is GC'ed it decreasing the reference count of the corresponding
-- entry, and removes it from the table entirely once the count drops to zero.
--
issue :: UID -> f -> LinkCache f o -> IO (Lifetime f)
issue key fun (LinkCache var) = do
  ticket <- newLifetime fun
  addFinalizer ticket $
    let refcount (Entry c f o)
          | c <= 1    = trace dump_ld (printf "ld: remove object code %s" (show key)) Nothing
          | otherwise = Just (Entry (c-1) f o)
    in
    modifyMVar_ var $ \m -> return $! Map.update refcount key m
  --
  return ticket

