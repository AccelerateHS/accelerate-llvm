{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Array.Table
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Array.Table
  where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.LLVM.Array.Nursery                 ( Nursery(..), NRS )
import qualified Data.Array.Accelerate.LLVM.Array.Nursery       as Nursery
import qualified Data.Array.Accelerate.LLVM.Debug               as Debug

-- standard library
import Prelude                                                  hiding ( lookup )
import Control.Concurrent.MVar
import Data.HashMap.Strict                                      ( HashMap )
import Data.Hashable
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import System.Mem.Weak                                          as Weak
import qualified Data.HashMap.Strict                            as Hash

import GHC.Base
import GHC.Ptr

#include "accelerate.h"


-- | The memory table is used to associate a host-side Accelerate array with a
-- corresponding array in on a remote target.
--
-- Old entries in the table are garbage collected from the table once the host
-- array is no longer reachable on the heap. The table stores weak references to
-- its entries. Once the key becomes unreachable, a finaliser will fire and
-- remove this entry from the table, and further attempts to dereference the
-- weak pointer will fail.
--
data MemoryTable c where
  MemoryTable :: Remote c => {
      memoryTable       :: {-# UNPACK #-} !(MT c)
    , memoryNursery     :: {-# UNPACK #-} !(Nursery (c ()))
    , weakTable         :: {-# UNPACK #-} !(Weak (MT c))
    }
    -> MemoryTable c

type MT c = MVar ( HashMap HostArray (RemoteArray c) )

data HostArray where
  HostArray     :: Typeable e
                => {-# UNPACK #-} !(Ptr e)
                -> HostArray

data RemoteArray c where
  RemoteArray   :: Typeable e
                => {-# UNPACK #-} !(Weak (c e))
                -> RemoteArray c

instance Eq HostArray where
  HostArray a1 == HostArray a2 = maybe False (== a2) (gcast a1)

instance Hashable HostArray where
  hashWithSalt salt (HostArray (Ptr a#)) = salt `hashWithSalt` (I# (addr2Int# a#))

instance Show HostArray where
  show (HostArray p) = "Array " ++ show p


-- | Functions to allocate and delete remote arrays. We also need to be able to
-- cast the type of the remote array data, for use with the nursery.
--
-- TLM: Using a class here might not be appropriate. E.g. we need to make the
--      correct CUDA context active before doing any de/allocations.
--
class Remote c where
  castRemote    :: c a -> c b
  freeRemote    :: c a -> IO ()
  mallocRemote  :: Int -> IO (c a)


-- | Create a new memory table from host to remote arrays. When the structure is
-- collected it will finalise all entries in the table.
--
new :: Remote c => IO (MemoryTable c)
new = do
  message "initialise memory table"
  ref   <- newMVar ( Hash.empty )
  nrs   <- Nursery.new freeRemote
  weak  <- mkWeakMVar ref (finalise ref)
  return $! MemoryTable ref nrs weak
  where
    finalise :: MT c -> IO ()
    finalise r = do
      message "finalise memory table"
      withMVar r (mapM_ (\(RemoteArray w) -> Weak.finalize w) . Hash.elems)


-- | Lookup the remote array corresponding to the given host-side array
--
{-# INLINEABLE lookup #-}
lookup :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a, Typeable b)
       => MemoryTable c
       -> ArrayData e
       -> IO (Maybe (c b))
lookup MemoryTable{..} !adata = do
  let !key      =  makeHostArray adata
  mw            <- withMVar memoryTable (\mt -> return (Hash.lookup key mt))
  case mw of
    Nothing
      -> trace ("lookup/not found: " ++ show key) $ return Nothing

    Just (RemoteArray w)
      -> do
            mv <- deRefWeak w
            case mv of
              Just v
                | Just p <- gcast v -> trace ("lookup/found: " ++ show key) $ return (Just p)
                | otherwise         -> INTERNAL_ERROR(error) "memory table/lookup" $ "type mismatch"

              -- Note: [Weak pointer weirdness]
              --
              -- There is an awkward race condition here. After the lookup is
              -- successful, there might conceivably be no further references to
              -- 'adata'. If that is so, and a garbage collection intervenes,
              -- the weak pointer might get tombstoned before 'deRefWeak' gets
              -- to it. In this case 'deRefWeak' returns 'Nothing' and we throw
              -- an error (below). However, because we use 'adata' in the
              -- failure case, this ensure that it is reachable in the
              -- continuation and thus 'deRefWeak' always succeeds! This sort of
              -- weirdness --- typical in the world of weak pointers --- is why
              -- we _must not_ reuse the stable name 'sa' computed above in the
              -- error message.
              --
              Nothing
                -> let !key' = makeHostArray adata
                   in  INTERNAL_ERROR(error) "memory table/lookup" ("dead weak pointer: " ++ show key')

-- | Convert the host array data into a memory table key
--
{-# INLINEABLE makeHostArray #-}
makeHostArray
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a)
    => ArrayData e
    -> HostArray
makeHostArray !adata = HostArray (ptrsOfArrayData adata)


-- | Allocate a new remote array and associate it with the given host side
-- array. This will attempt to reuse an old array from the nursery if possible.
--
-- In order to increase the hit rate to the nursery, allocations are not made
-- for the exact number of bytes requested, but instead rounded up to a
-- chunk/page size.
--
{-# INLINEABLE malloc #-}
malloc
    :: forall c e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a, Storable a)
    => MemoryTable c
    -> ArrayData e              -- Associated host array
    -> Int                      -- The number of _elements_ in the array
    -> IO (c a)
malloc mt@MemoryTable{..} !adata !n =
  let
      -- Calculate the number of elements that should actually be allocated. If
      -- this is a singleton array then just allocate for that element,
      -- otherwise round up to the page size.
      --
      numElements
        | n <= 1        = 1
        | otherwise     = pageSize * multipleOf (fromIntegral n) (fromIntegral pageSize)

      pageSize          = 1024
      multipleOf x f    = floor ((x + (f-1)) / f :: Double)
      bytes             = numElements * sizeOf (undefined :: a)
  in do
    mp  <- Nursery.lookup bytes memoryNursery
    ptr <- case mp of
             Just p     -> trace "malloc/nursery" $ return (castRemote p)
             Nothing    -> trace "malloc/new"     $ mallocRemote numElements    -- TODO: exception handling?
    --
    insert mt adata ptr bytes
    return ptr


-- | Add a remote array to managed memory table. The remote is deallocated
-- (moved to the nursery) when the host side array is garbage collected.
--
{-# INLINEABLE insert #-}
insert
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a)
    => MemoryTable c
    -> ArrayData e
    -> c a
    -> Int
    -> IO ()
insert (MemoryTable !memoryTable (Nursery _ !weakNursery) !weakTable) !adata !ptr !bytes =
  let !key      = makeHostArray adata
  in do
    remote      <- RemoteArray `fmap` mkWeak adata ptr (Just $ delete weakTable weakNursery key ptr bytes)
    message ("insert: " ++ show key)
    modifyMVar_ memoryTable (return . Hash.insert key remote)


{-# INLINEABLE delete #-}
delete
    :: Remote c
    => Weak (MT c)
    -> Weak (NRS (c ()))
    -> HostArray
    -> c a
    -> Int
    -> IO ()
delete !weak_mt !weak_nrs !key !remote !bytes = do
  -- First check if the memory table is still active. If it is, we first need to
  -- remove this entry from the table.
  mmt   <- deRefWeak weak_mt
  case mmt of
    Nothing     -> message ("finalise/dead table: " ++ show key)
    Just r      -> modifyMVar_ r (return . Hash.delete key)

  -- If the nursery if still alive, stash the data there. Otherwise, just delete
  -- it immediately.
  mnrs  <- deRefWeak weak_nrs
  case mnrs of
    Nothing     -> trace ("finalise/free: "  ++ show key) $ freeRemote remote
    Just nrs    -> trace ("finalise/stash: " ++ show key) $ Nursery.insert bytes (castRemote remote) nrs


-- Debug
-- -----

{-# INLINE showBytes #-}
showBytes :: Int -> String
showBytes x = Debug.showFFloatSIBase (Just 0) 1024 (fromIntegral x :: Double) "B"

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = Debug.message Debug.dump_gc ("gc: " ++ msg) >> next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

