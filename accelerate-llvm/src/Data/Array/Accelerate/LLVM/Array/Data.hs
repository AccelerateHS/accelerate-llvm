{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Array.Data
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Array.Data (

  Remote(..),
  newRemote, newRemoteAsync,
  useRemote,
  copyToRemote,
  copyToHost,
  copyToPeer,
  indexRemote,

  runIndexArray, runIndexArrayAsync,
  runArray, runArrayAsync,
  runArrays, runArraysAsync,

) where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Execute.Async

import Control.Monad                                                ( liftM, liftM2 )
import Prelude


class Async arch => Remote arch where

  -- | Allocate a new uninitialised array on the remote device.
  --
  allocateRemote :: ArrayR (Array sh e) -> sh -> Par arch (Array sh e)

  -- | Use the given immutable array on the remote device. Since the source
  -- array is immutable, the garbage collector can evict and re-upload the data
  -- as necessary without copy-back.
  --
  {-# INLINE useRemoteR #-}
  useRemoteR
      :: SingleType e
      -> Int                      -- ^ number of elements to copy
      -> ArrayData e              -- ^ array payload
      -> Par arch (FutureR arch (ArrayData e))
  useRemoteR _ _ = newFull

  -- | Upload an array from the host to the remote device.
  --
  {-# INLINE copyToRemoteR #-}
  copyToRemoteR
      :: SingleType e
      -> Int                      -- ^ number of elements to copy
      -> ArrayData e              -- ^ array payload
      -> Par arch (FutureR arch (ArrayData e))
  copyToRemoteR _ _ = newFull

  -- | Copy an array from the remote device back to the host.
  --
  {-# INLINE copyToHostR #-}
  copyToHostR
      :: SingleType e
      -> Int                      -- ^ number of elements to copy
      -> ArrayData e              -- ^ array payload
      -> Par arch (FutureR arch (ArrayData e))
  copyToHostR _ _ = newFull

  -- | Copy a section of an array between two remote instances of the same type.
  -- This may be more efficient than copying to the host and then to the second
  -- remote instance (e.g. DMA between two CUDA devices).
  --
  {-# INLINE copyToPeerR #-}
  copyToPeerR
      :: arch                     -- ^ remote device to copy to
      -> SingleType e
      -> Int                      -- ^ number of elements to copy
      -> ArrayData e              -- ^ array payload
      -> Par arch (FutureR arch (ArrayData e))
  copyToPeerR _ _ _ = newFull

  -- | Upload an immutable array from the host to the remote device,
  -- asynchronously. Since the source array is immutable, the garbage collector
  -- can evict and re-upload the data as necessary without copy-back. This may
  -- upload each array payload in a separate execution stream, thereby making us
  -- of multiple memcpy engines.
  --
  {-# INLINE useRemoteAsync #-}
  useRemoteAsync :: ArraysR arrs -> arrs -> Par arch (FutureArraysR arch arrs)
  useRemoteAsync repr arrs =
    runArraysAsync repr arrs $ \(ArrayR shr tp) arr ->
      let n = size shr (shape arr)
      in  runArrayAsync tp arr $ \m tp' ad ->
            useRemoteR tp' (n*m) ad

  -- | Upload an existing array to the remote device, asynchronously.
  --
  {-# INLINE copyToRemoteAsync #-}
  copyToRemoteAsync :: ArraysR arrs -> arrs -> Par arch (FutureArraysR arch arrs)
  copyToRemoteAsync reprs arrs =
    runArraysAsync reprs arrs $ \(ArrayR shr tp) arr ->
      let n = size shr (shape arr)
      in  runArrayAsync tp arr $ \m tp' ad ->
            copyToRemoteR tp' (n*m) ad

  -- | Copy an array from the remote device to the host, asynchronously
  --
  {-# INLINE copyToHostAsync #-}
  copyToHostAsync :: ArraysR arrs -> arrs -> Par arch (FutureArraysR arch arrs)
  copyToHostAsync reprs arrs =
    runArraysAsync reprs arrs $ \(ArrayR shr tp) arr ->
      let n = size shr (shape arr)
      in  runArrayAsync tp arr $ \m tp' ad ->
            copyToHostR tp' (n*m) ad

  -- | Copy arrays between two remote instances. This may be more efficient than
  -- copying to the host and then to the second remote instance (e.g. by DMA
  -- between the two remote devices).
  --
  {-# INLINE copyToPeerAsync #-}
  copyToPeerAsync :: arch -> ArraysR arrs -> arrs -> Par arch (FutureArraysR arch arrs)
  copyToPeerAsync peer reprs arrs =
    runArraysAsync reprs arrs $ \(ArrayR shr tp) arr ->
      let n = size shr (shape arr)
      in  runArrayAsync tp arr $ \m tp' ad ->
            copyToPeerR peer tp' (n*m) ad

  -- | Read a single element from the array at the given row-major index
  --
  {-# INLINE indexRemoteAsync #-}
  indexRemoteAsync
      :: TypeR e
      -> Array sh e
      -> Int
      -> Par arch (FutureR arch e)
  indexRemoteAsync tp (Array _ ad) i = newFull (indexArrayData tp ad i)


-- | Create a new array from its representation on the host, and upload it to
-- the remote device.
--
{-# INLINE newRemote #-}
newRemote
    :: Remote arch
    => ArrayR (Array sh e)
    -> sh
    -> (sh -> e)
    -> Par arch (Array sh e)
newRemote repr sh f =
  get =<< newRemoteAsync repr sh f


-- | Create a new array from its representation on the host, and upload it as
-- a new remote array, asynchronously.
--
{-# INLINE newRemoteAsync #-}
newRemoteAsync
    :: Remote arch
    => ArrayR (Array sh e)
    -> sh
    -> (sh -> e)
    -> Par arch (FutureR arch (Array sh e))
newRemoteAsync repr sh f =
  useRemoteAsync (TupRsingle repr) $! fromFunction repr sh f


-- | Upload an immutable array from the host to the remote device. This is
-- a synchronous operation in that it will not return until the transfer
-- completes, but the individual array payloads will be uploaded concurrently if
-- possible.
--
{-# INLINE useRemote #-}
useRemote :: Remote arch => ArraysR a -> a -> Par arch a
useRemote repr arrs =
  getArrays repr =<< useRemoteAsync repr arrs

-- | Uploading existing arrays from the host to the remote device. This is
-- synchronous with respect to the calling thread, but the individual array
-- payloads may themselves be transferred concurrently.
--
{-# INLINE copyToRemote #-}
copyToRemote :: Remote arch => ArraysR a -> a -> Par arch a
copyToRemote repr arrs =
  getArrays repr =<< copyToRemoteAsync repr arrs

-- | Copy an array from the remote device to the host. This is synchronous with
-- respect to the calling thread, but the individual array payloads may
-- themselves be transferred concurrently.
--
{-# INLINE copyToHost #-}
copyToHost :: Remote arch => ArraysR a -> a -> Par arch a
copyToHost repr arrs =
  blockArrays repr =<< copyToHostAsync repr arrs

-- | Copy arrays between two remote instances of the same type. This may be more
-- efficient than copying to the host and then to the second remote instance
-- (e.g. DMA between CUDA devices).
--
{-# INLINE copyToPeer #-}
copyToPeer :: Remote arch => arch -> ArraysR a -> a -> Par arch a
copyToPeer peer repr arrs =
  getArrays repr =<< copyToPeerAsync peer repr arrs

-- | Read a single element from the remote array at the given row-major index.
-- This is synchronous with respect to both the host and remote device.
--
{-# INLINE indexRemote #-}
indexRemote :: Remote arch => TypeR e -> Array sh e -> Int -> Par arch e
indexRemote tp arr i =
  block =<< indexRemoteAsync tp arr i


-- Helpers for traversing the Arrays data structure
-- ------------------------------------------------

-- | Read a single element from an array at the given row-major index.
--
{-# INLINE runIndexArray #-}
runIndexArray
    :: forall m sh e. Monad m
    => (forall s. ArrayData s ~ ScalarArrayData s => Int -> SingleType s -> ArrayData s -> Int -> m (ArrayData s))
    -> TypeR e
    -> Array sh e
    -> Int
    -> m e
runIndexArray worker tp (Array _ adata) i = flip (indexArrayData tp) 0 <$> indexR tp adata
  where
    indexR :: TypeR s -> ArrayData s -> m (ArrayData s)
    indexR TupRunit           !_           = return ()
    indexR (TupRpair !t1 !t2) (!ad1, !ad2) = liftM2 (,) (indexR t1 ad1) (indexR t2 ad2)
    indexR (TupRsingle t)     !ad
      | ScalarArrayDict w s <- scalarArrayDict t
      , SingleArrayDict     <- singleArrayDict s
      = worker w s ad i

{-# INLINE runIndexArrayAsync #-}
runIndexArrayAsync
    :: forall arch sh e. Async arch
    => (forall s. ArrayData s ~ ScalarArrayData s => Int -> SingleType s -> ArrayData s -> Int -> Par arch (FutureR arch (ArrayData s)))
    -> TypeR e
    -> Array sh e
    -> Int
    -> Par arch (FutureR arch e)
runIndexArrayAsync worker tp (Array _ adata) i = (flip (indexArrayData tp) 0) `liftF` indexR tp adata
  where
    indexR :: TypeR s -> ArrayData s -> Par arch (FutureR arch (ArrayData s))
    indexR TupRunit           !_           = newFull ()
    indexR (TupRpair !t1 !t2) (!ad1, !ad2) = liftF2' (,) (indexR t1 ad1) (indexR t2 ad2)
    indexR (TupRsingle t)     !ad
      | ScalarArrayDict w s <- scalarArrayDict t
      , SingleArrayDict     <- singleArrayDict s
      = worker w s ad i

    -- It is expected these transfers will be very small, so don't bother
    -- creating new execution streams for them
    liftF2' :: (a -> b -> c) -> Par arch (FutureR arch a) -> Par arch (FutureR arch b) -> Par arch (FutureR arch c)
    liftF2' f x y = do
      r  <- new
      x' <- x
      y' <- y
      put r =<< liftM2 f (get x') (get y')
      return r


-- | Generalised function to traverse the Arrays structure
--
{-# INLINE runArrays #-}
runArrays
    :: forall m arrs. Monad m
    => ArraysR arrs
    -> arrs
    -> (forall sh e. ArrayR (Array sh e) -> Array sh e -> m (Array sh e))
    -> m arrs
runArrays reprs arrs worker = runR reprs arrs
  where
    runR :: ArraysR a -> a -> m a
    runR TupRunit                   ()             = return ()
    runR (TupRsingle repr@ArrayR{}) arr            = worker repr arr
    runR (TupRpair aeR2 aeR1)       (arrs2, arrs1) = liftM2 (,) (runR aeR2 arrs2) (runR aeR1 arrs1)

{-# INLINE runArraysAsync #-}
runArraysAsync
    :: forall arch arrs. Async arch
    => ArraysR arrs
    -> arrs
    -> (forall sh e. ArrayR (Array sh e) -> Array sh e -> Par arch (FutureR arch (Array sh e)))
    -> Par arch (FutureArraysR arch arrs)
runArraysAsync reprs arrs worker = runR reprs arrs
  where
    runR :: ArraysR a -> a -> Par arch (FutureArraysR arch a)
    runR TupRunit                   ()             = return ()
    runR (TupRsingle repr@ArrayR{}) arr            = worker repr arr
    runR (TupRpair aeR2 aeR1)       (arrs2, arrs1) = (,) <$> runR aeR2 arrs2 <*> runR aeR1 arrs1


-- | Generalised function to traverse the ArrayData structure with one
-- additional argument
--
{-# INLINE runArray #-}
runArray
    :: forall m sh e. Monad m
    => TypeR e
    -> Array sh e
    -> (forall s. ArrayData s ~ ScalarArrayData s => Int -> SingleType s -> ScalarArrayData s -> m (ScalarArrayData s))
    -> m (Array sh e)
runArray tp (Array sh adata) worker = Array sh `liftM` runR tp adata
  where
    runR :: TypeR s -> ArrayData s -> m (ArrayData s)
    runR (TupRunit)         !_           = return ()
    runR (TupRpair !t2 !t1) (!ad2, !ad1) = liftM2 (,) (runR t2 ad2) (runR t1 ad1)
    runR (TupRsingle !t)    !ad
      | ScalarArrayDict w s <- scalarArrayDict t
      , SingleArrayDict     <- singleArrayDict s
      = worker w s ad

{-# INLINE runArrayAsync #-}
runArrayAsync
    :: forall arch sh e. Async arch
    => TypeR e
    -> Array sh e
    -> (forall s. ArrayData s ~ ScalarArrayData s => Int -> SingleType s -> ScalarArrayData s -> Par arch (FutureR arch (ScalarArrayData s)))
    -> Par arch (FutureR arch (Array sh e))
runArrayAsync tp (Array sh adata) worker = Array sh `liftF` runR tp adata
  where
    runR :: forall s. TypeR s -> ArrayData s -> Par arch (FutureR arch (ArrayData s))
    runR (TupRunit)         !_           = newFull ()
    runR (TupRpair !t2 !t1) (!ad2, !ad1) = liftF2 (,) (runR t2 ad2) (runR t1 ad1)
    runR (TupRsingle !t)    !ad
      | ScalarArrayDict w s <- scalarArrayDict t
      , SingleArrayDict     <- singleArrayDict s
      = worker w s ad

{-# INLINE liftF #-}
liftF :: Async arch
      => (a -> b)
      -> Par arch (FutureR arch a)
      -> Par arch (FutureR arch b)
liftF f x = do
  r  <- new
  x' <- x
  fork $ put r . f =<< get x'
  return r

{-# INLINE liftF2 #-}
liftF2 :: Async arch
       => (a -> b -> c)
       -> Par arch (FutureR arch a)
       -> Par arch (FutureR arch b)
       -> Par arch (FutureR arch c)
liftF2 f x y = do
  r  <- new
  x' <- spawn x
  y' <- spawn y
  fork $ put r =<< liftM2 f (get x') (get y')
  return r

