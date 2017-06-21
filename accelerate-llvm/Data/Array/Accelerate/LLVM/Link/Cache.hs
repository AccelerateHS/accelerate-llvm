{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Link.Cache
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Link.Cache (

  LinkCache,
  new, lookup, insert,

) where

import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Debug

import Control.Monad
import Control.Concurrent.MVar
import Data.IntMap.Strict                                           ( IntMap )
import Prelude                                                      hiding ( lookup )
import Text.Printf
import qualified Data.IntMap.Strict                                 as IM


-- Simple reference-counted linker cache for function tables 'f' implemented by
-- object code 'o'.
--
data LinkCache f o = LinkCache {-# UNPACK #-} !(MVar (IntMap (Entry f o)))
data Entry f o     = Entry {-# UNPACK #-} !Int !f !o


-- Create a new linker cache
--
new :: IO (LinkCache f o)
new = LinkCache `liftM` newMVar IM.empty


-- Insert the given function table and object code into the cache. The returned
-- ticket must be kept alive for as long as you need the object code to live;
-- linker table entries are removed once all tickets referring to them are
-- GC'ed.
--
-- Inserting a duplicate entry is an error (why?)
--
insert :: Int -> f -> o -> LinkCache f o -> IO (Lifetime f)
insert key functionTable objectCode cache@(LinkCache var) = do
  ticket <- issue key functionTable cache
  modifyMVar_ var $ \im ->
    let collision = $internalError "insert" "duplicate entry"
    in  return $! IM.insertWith collision key (Entry 1 functionTable objectCode) im
  --
  return ticket


-- Check the linker cache for the given functions; if found return the
-- corresponding function table.
--
lookup :: Int -> LinkCache f o -> IO (Maybe (Lifetime f))
lookup key cache@(LinkCache var) = do
  modifyMVar var $ \im ->
    case IM.lookup key im of
      Nothing             -> return (im, Nothing)
      Just (Entry c f o)  -> do
        ticket <- issue key f cache
        return ( IM.insert key (Entry (c+1) f o) im, Just ticket )


-- Issue a new ticket for the given table key/function table. When the returned
-- lifetime is GC'ed it decreasing the reference count of the corresponding
-- entry, and removes it from the table entirely once the count drops to zero.
--
issue :: Int -> f -> LinkCache f o -> IO (Lifetime f)
issue key fun (LinkCache var) = do
  ticket <- newLifetime fun
  addFinalizer ticket $
    let refcount (Entry c f o)
          | c <= 1    = trace dump_ld (printf "ld: remove object code %016X" key) Nothing
          | otherwise = Just (Entry (c-1) f o)
    in
    modifyMVar_ var $ \im -> return $! IM.update refcount key im
  --
  return ticket

