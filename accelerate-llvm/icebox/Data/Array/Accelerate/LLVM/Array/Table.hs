{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Array.Table
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Array.Table (

  MemoryTable, MallocRemote, FreeRemote,
  new, member, lookup, malloc, cleanup,

) where

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.LLVM.Array.Nursery                 ( Nursery(..), NRS )
import qualified Data.Array.Accelerate.LLVM.Array.Nursery       as Nursery
import qualified Data.Array.Accelerate.Debug                    as Debug

-- standard library
import Prelude                                                  hiding ( lookup )
import Control.Concurrent
import Control.Monad
import Data.IntMap.Strict                                       ( IntMap )
import Data.Maybe
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import System.Mem
import System.Mem.Weak
import Unsafe.Coerce                                            ( unsafeCoerce )
import qualified Data.IntMap.Strict                             as IM

import GHC.Base
import GHC.Ptr


-- | The memory table is used to associate a host-side Accelerate array with a
-- corresponding array in on a remote target.
--
-- Old entries in the table are garbage collected from the table once the host
-- array is no longer reachable on the heap. The table stores weak references to
-- its entries. Once the key becomes unreachable, a finaliser will fire and
-- remove this entry from the table, and further attempts to dereference the
-- weak pointer will fail.
--
-- PRECONDITION: The underlying Accelerate array is pinned.
--
data MemoryTable c = MemoryTable {
    memoryTable         :: {-# UNPACK #-} !(MT c)
  , memoryNursery       :: {-# UNPACK #-} !(Nursery (c ()))
  , weakTable           :: {-# UNPACK #-} !(Weak (MT c))
  }

type MT c = MVar ( IntMap (RemoteArray c) )

type HostArray = Int

data RemoteArray c where
  RemoteArray   :: Typeable e
                => {-# UNPACK #-} !(Weak (c e))
                -> RemoteArray c


-- The type of functions to allocate and deallocate remote arrays.
--
type FreeRemote c       = forall a. c a -> IO ()
type MallocRemote c a   = Int -> IO (c a)


-- | TODO: derp...
--
castRemote :: c a -> c b
castRemote = unsafeCoerce


-- | Create a new memory table from host to remote arrays. When the structure is
-- collected it will finalise all entries in the table.
--
new :: FreeRemote c -> IO (MemoryTable c)
new freeRemote = do
  message "initialise memory table"
  ref   <- newMVar ( IM.empty )
  nrs   <- Nursery.new freeRemote
  weak  <- mkWeakMVar ref (finalise ref)
  return $! MemoryTable ref nrs weak
  where
    finalise :: MT c -> IO ()
    finalise r = do
      message "finalise memory table"
      withMVar r (mapM_ (\(RemoteArray w) -> finalize w) . IM.elems)


-- | Is the key a _valid_ member of the memory table?
--
-- Because of the possibility of collisions in the host key, this also ensures
-- that the entries still refers to valid data. If not, we run the finaliser,
-- which also deallocates the remote data and removes its entry from the table.
-- Note that it is important for the main thread to 'yield' at that point so
-- that the finaliser can run immediately.
--
-- See: [Host arrays as memory table keys]
--
{-# INLINEABLE member #-}
member :: (ArrayElt e, ArrayPtrs e ~ Ptr a)
       => MemoryTable c
       -> ArrayData e
       -> IO Bool
member MemoryTable{..} !adata = do
  key <- makeHostArray adata
  mw  <- withMVar memoryTable (\mt -> return (IM.lookup key mt))
  case mw of
    Nothing                             -- no entry in the table
      -> return False

    Just (RemoteArray w) -> do
      mv <- deRefWeak w
      case mv of
        Just _  -> return True          -- entry present and valid
        Nothing -> do                   -- entry present but invalid, refers to old (dead) data
          message ("member/finalise dead: " ++ show key)
          finalize w                    -- queue the finaliser to run
          yield                         -- yield so that the finaliser can run immediately
#ifdef ACCELERATE_INTERNAL_CHECKS
          exists <- withMVar memoryTable (\mt -> return (IM.member key mt))
          _      <- $internalCheck "member" "finaliser did not run" (not exists) (return ())
#endif
          return False


-- | Lookup the remote array corresponding to the given host-side array. This
-- assumes that all key-value pairs in the memory table refer to valid data. If
-- you are not sure, or want to check whether there is a remote array for the
-- given key, use 'member' instead.
--
-- See note: [Host arrays as memory table keys].
--
{-# INLINEABLE lookup #-}
lookup :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable b)
       => MemoryTable c
       -> ArrayData e
       -> IO (Maybe (c b))
lookup MemoryTable{..} !adata = do
  key <- makeHostArray adata
  mw  <- withMVar memoryTable (\mt -> return (IM.lookup key mt))
  case mw of
    Nothing
      -> trace ("lookup/not found: " ++ show key) $ return Nothing

    Just (RemoteArray w)
      -> do
            mv <- deRefWeak w
            case mv of
              Just v
                | Just p <- gcast v -> trace ("lookup/found: " ++ show key) $ return (Just p)
                | otherwise         -> $internalError "memory table/lookup" "type mismatch"

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
              -- we _must not_ reuse the table key computed above in the error
              -- message.
              --
              Nothing
                -> do key' <- makeHostArray adata
                      $internalError "memory table/lookup" ("dead weak pointer: " ++ show key')


-- | Convert the host array data into a memory table key.
--
-- Note: [Host arrays as memory table keys]
--
-- The raw pointer value is used as a key to the host array. This means that the
-- value is only unique as long as the array is alive, not for the entire life
-- of the program, as a StableName would be. Thus, we must be careful with its
-- use when it comes to finalisers. Consider the following situation:
--
--   1. An array is allocated on the host
--
--   2. An Accelerate computation is run. It sees that the array has no
--      corresponding device array so creates one and puts an entry in the
--      memory table.
--
--   3. Garbage collection occurs. The host side array is no longer referenced,
--      so it is deallocated. Any calls to 'deRefWeak' on the weak pointer for
--      this array will return 'Nothing'. Most importantly, the finalizer has
--      not yet run. GHC makes no guarantees about when, or even if, a finalizer
--      will run. This means there is still an entry in the memory table for
--      this array.
--
--   4. A new array is allocated host side. The allocator returns a block of
--      memory that was recently freed, the same one used for the first array.
--      This means both this new array, and the old array, have the same pointer
--      value, and thus the same 'HostArray' key.
--
--   5. Another Accelerate computation is run. It finds an entry in the memory
--      table for what it thinks is the array, but is actually the dead array.
--      It calls 'deRefWeak' on the weak pointer that has already been
--      tombstoned, and gets 'Nothing' even though the (new) key is alive,
--      because the weak pointer actually refers to the old, dead key.
--
-- Thus, if there is an existing entry in the table when we attempt to allocate
-- a new array (4), we must ensure that that entry refers to valid data.
--
{-# INLINEABLE makeHostArray #-}
makeHostArray
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a)
    => ArrayData e
    -> IO HostArray
makeHostArray !adata =
  case ptrsOfArrayData adata of
    Ptr a# -> return $! I# (addr2Int# a#)


-- | Make a weak pointer between the host side array data and the remote array.
--
{-# INLINEABLE makeRemoteArray #-}
makeRemoteArray
    :: (Typeable a, ArrayElt e, ArrayPtrs e ~ Ptr a)
    => ArrayData e
    -> c a
    -> IO ()
    -> IO (RemoteArray c)
makeRemoteArray adata v f =
  RemoteArray `fmap` mkWeak adata v (Just f)


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
    => FreeRemote c
    -> MallocRemote c a
    -> MemoryTable c
    -> ArrayData e              -- Associated host array
    -> Int                      -- The number of _elements_ in the array
    -> IO (c a)
malloc freeRemote mallocRemote !mt@MemoryTable{..} !adata !n =
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
    insert freeRemote mt adata ptr bytes
    return ptr


-- | Add a remote array to managed memory table. The remote is deallocated
-- (moved to the nursery) when the host side array is garbage collected.
--
{-# INLINEABLE insert #-}
insert
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a)
    => FreeRemote c
    -> MemoryTable c
    -> ArrayData e
    -> c a
    -> Int
    -> IO ()
insert freeRemote !MemoryTable{..} !adata !ptr !bytes =
  let Nursery _ !weakNursery    = memoryNursery
      finalise key              = delete freeRemote weakTable weakNursery key ptr bytes
  in do
    key    <- makeHostArray adata
    remote <- makeRemoteArray adata ptr (finalise key)
    message ("insert: " ++ show key)
    modifyMVar_ memoryTable $ \mt ->
      let f Nothing  = Just remote
          f (Just _) = $internalError "insert" ("duplicate key: " ++ show key)
      in
      return $! IM.alter f key mt


-- | Remove an entry from the memory table. This might stash the memory into the
-- nursery for later reuse, or actually deallocate the remote memory.
--
{-# INLINEABLE delete #-}
delete
    :: FreeRemote c
    -> Weak (MT c)
    -> Weak (NRS (c ()))
    -> HostArray
    -> c a
    -> Int
    -> IO ()
delete freeRemote !weak_mt !weak_nrs !key !remote !bytes = do
  message ("delete: " ++ show key)

  -- First check if the memory table is still active. If it is, we first need to
  -- remove this entry from the table.
  mmt   <- deRefWeak weak_mt
  case mmt of
    Nothing     -> message ("finalise/dead table: " ++ show key)
    Just r      -> modifyMVar_ r (return . IM.delete key)

  -- If the nursery if still alive, stash the data there. Otherwise, just delete
  -- it immediately.
  mnrs  <- deRefWeak weak_nrs
  case mnrs of
    Nothing     -> trace ("finalise/free: "  ++ show key) $ freeRemote remote
    Just nrs    -> trace ("finalise/stash: " ++ show key) $ Nursery.insert bytes (castRemote remote) nrs


-- | Cleanup the memory table in an attempt to create more free space on the
-- remote device. This removes all entries from the nursery, and finalises any
-- unreachable arrays.
--
{-# INLINEABLE cleanup #-}
cleanup
    :: FreeRemote c
    -> MemoryTable c
    -> IO ()
cleanup freeRemote !MemoryTable{..} = do
  let Nursery nursery _ = memoryNursery
  --
  message "table clean"
  Nursery.cleanup freeRemote nursery
  performGC
  mr    <- deRefWeak weakTable
  case mr of
    Nothing     -> return ()
    Just ref    -> withMVar ref $ \mt ->
                      forM_ (IM.elems mt) $ \(RemoteArray w) -> do
                        alive   <- isJust `fmap` deRefWeak w
                        unless alive (finalize w)
  performGC


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = Debug.traceIO Debug.dump_gc ("gc: " ++ msg) >> next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

