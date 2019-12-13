{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Array.Nursery
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Array.Nursery
  where

-- standard library
import Prelude                                                  hiding ( lookup )
import Control.Concurrent.MVar
import Data.Sequence                                            ( Seq )
import Data.IntMap                                              ( IntMap )
import System.Mem.Weak
import qualified Data.Sequence                                  as Seq
import qualified Data.Traversable                               as Seq
import qualified Data.IntMap.Strict                             as IM

-- accelerate
import qualified Data.Array.Accelerate.Debug                    as Debug


-- | The nursery is primarily designed as a place to store device memory arrays
-- that are no longer needed. Often it is quicker to reuse an existing array,
-- rather than calling into the device API in order to allocate a fresh array.
--
-- The nursery is wrapped in an MVar so that several threads may safely access
-- it concurrently.
--
data Nursery a  = Nursery {-# UNPACK #-} !(NRS a)
                          {-# UNPACK #-} !(Weak (NRS a))

type NRS a      = MVar ( IntMap (Seq a) )


-- | Create a fresh nursery
--
-- When the nursery is garbage collected, the provided function will be run on
-- each element of the map. In the context of array storage, this would be to
-- free the stashed memory.
--
{-# INLINEABLE new #-}
new :: forall a. (a -> IO ()) -> IO (Nursery a)
new delete = do
  message "initialise nursery"
  ref   <- newMVar ( IM.empty )
  weak  <- mkWeakMVar ref (cleanup delete ref)
  return $! Nursery ref weak


-- | Look up an entry in the nursery with a given key.
--
{-# INLINEABLE lookup #-}
lookup :: Int -> Nursery a -> IO (Maybe a)
lookup !key (Nursery !ref !_) =
  modifyMVar ref $ \nrs ->
    let (mv, nrs')      = IM.updateLookupWithKey f key nrs
        f _ s           = case Seq.viewl s of
                            Seq.EmptyL  -> Nothing      -- should not happen!
                            _ Seq.:< vs -> if Seq.null vs then Nothing
                                                          else Just vs
    in
    return $ case mv of
         Nothing     -> (nrs', Nothing)
         Just s      -> (nrs', Just (Seq.index s 0))


-- | Add an entry into the nursery
--
{-# INLINEABLE insert #-}
insert :: Int -> a -> NRS a -> IO ()
insert !key !val !ref =
  modifyMVar_ ref $ \nrs ->
    return $! IM.insertWith (Seq.><) key (Seq.singleton val) nrs


-- | Delete all entries from the nursery.
--
{-# INLINEABLE cleanup #-}
cleanup :: forall a. (a -> IO ()) -> NRS a -> IO ()
cleanup delete !ref = do
  message "nursery clean"
  modifyMVar_ ref (\nrs -> do mapM_ (Seq.mapM delete) (IM.elems nrs)
                              return IM.empty)


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = Debug.traceIO Debug.dump_gc ("gc: " ++ msg) >> next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

