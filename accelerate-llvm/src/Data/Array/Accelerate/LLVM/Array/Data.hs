{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Array.Data
-- Copyright   : [2014..2018] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

  runIndexArray,
  runArray, runArrayAsync,
  runArrays, runArraysAsync,

  module Data.Array.Accelerate.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Representation         as R

import Data.Array.Accelerate.LLVM.Execute.Async

-- standard library
import Control.Monad                                                ( liftM, liftM2 )
import Data.Typeable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Prelude


class Async arch => Remote arch where

  -- | Allocate a new uninitialised array on the remote device.
  --
  allocateRemote :: (Shape sh, Elt e) => sh -> Par arch (Array sh e)

  -- | Use the given immutable array on the remote device. Since the source
  -- array is immutable, the garbage collector can evict and re-upload the data
  -- as necessary without copy-back.
  --
  useRemoteR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ number of elements to copy
      -> ArrayData e              -- ^ array payload
      -> Par arch (FutureR arch (ArrayData e))

  -- | Upload an array from the host to the remote device.
  --
  copyToRemoteR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ number of elements to copy
      -> ArrayData e              -- ^ array payload
      -> Par arch (FutureR arch (ArrayData e))

  -- | Copy an array from the remote device back to the host.
  --
  copyToHostR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ number of elements to copy
      -> ArrayData e              -- ^ array payload
      -> Par arch (FutureR arch (ArrayData e))

  -- | Copy a section of an array between two remote instances of the same type.
  -- This may be more efficient than copying to the host and then to the second
  -- remote instance (e.g. DMA between two CUDA devices).
  --
  copyToPeerR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => arch                     -- ^ remote device to copy to
      -> Int                      -- ^ number of elements to copy
      -> ArrayData e              -- ^ array payload
      -> Par arch (FutureR arch (ArrayData e))

  -- | Upload an immutable array from the host to the remote device,
  -- asynchronously. Since the source array is immutable, the garbage collector
  -- can evict and re-upload the data as necessary without copy-back. This may
  -- upload each array payload in a separate execution stream, thereby making us
  -- of multiple memcpy engines.
  --
  {-# INLINEABLE useRemoteAsync #-}
  useRemoteAsync :: Arrays arrs => arrs -> Par arch (FutureR arch arrs)
  useRemoteAsync arrs =
    runArraysAsync arrs $ \arr@(Array sh _) ->
      let n = R.size sh
      in  runArrayAsync arr $ \m ad ->
            useRemoteR (n*m) ad

  -- | Upload an existing array to the remote device, asynchronously.
  --
  {-# INLINEABLE copyToRemoteAsync #-}
  copyToRemoteAsync :: Arrays arrs => arrs -> Par arch (FutureR arch arrs)
  copyToRemoteAsync arrs =
    runArraysAsync arrs $ \arr@(Array sh _) ->
      let n = R.size sh
      in  runArrayAsync arr $ \m ad ->
            copyToRemoteR (n*m) ad

  -- | Copy an array from the remote device to the host, asynchronously
  --
  {-# INLINEABLE copyToHostAsync #-}
  copyToHostAsync :: Arrays arrs => arrs -> Par arch (FutureR arch arrs)
  copyToHostAsync arrs =
    runArraysAsync arrs $ \arr@(Array sh _) ->
      let n = R.size sh
      in  runArrayAsync arr $ \m ad ->
            copyToHostR (n*m) ad

  -- | Copy arrays between two remote instances. This may be more efficient than
  -- copying to the host and then to the second remote instance (e.g. by DMA
  -- between the two remote devices).
  --
  {-# INLINEABLE copyToPeerAsync #-}
  copyToPeerAsync :: Arrays arrs => arch -> arrs -> Par arch (FutureR arch arrs)
  copyToPeerAsync peer arrs =
    runArraysAsync arrs $ \arr@(Array sh _) ->
      let n = R.size sh
      in  runArrayAsync arr $ \m ad ->
            copyToPeerR peer (n*m) ad

  -- | Read a single element from the array at the given row-major index
  --
  indexRemoteAsync
      :: Array sh e
      -> Int
      -> Par arch (FutureR arch e)
  -- TODO: default implementation


-- | Create a new array from its representation on the host, and upload it to
-- the remote device.
--
{-# INLINEABLE newRemote #-}
newRemote
    :: (Remote arch, Shape sh, Elt e)
    => sh
    -> (sh -> e)
    -> Par arch (Array sh e)
newRemote sh f =
  get =<< newRemoteAsync sh f


-- | Create a new array from its representation on the host, and upload it as
-- a new remote array, asynchronously.
--
{-# INLINEABLE newRemoteAsync #-}
newRemoteAsync
    :: (Remote arch, Shape sh, Elt e)
    => sh
    -> (sh -> e)
    -> Par arch (FutureR arch (Array sh e))
newRemoteAsync sh f =
  useRemoteAsync $! fromFunction sh f


-- | Upload an immutable array from the host to the remote device. This is
-- a synchronous operation in that it will not return until the transfer
-- completes, but the individual array payloads will be uploaded concurrently if
-- possible.
--
{-# INLINEABLE useRemote #-}
useRemote :: (Remote arch, Arrays a) => a -> Par arch a
useRemote arrs =
  get =<< useRemoteAsync arrs

-- | Uploading existing arrays from the host to the remote device. This is
-- synchronous with respect to the calling thread, but the individual array
-- payloads may themselves be transferred concurrently.
--
{-# INLINEABLE copyToRemote #-}
copyToRemote :: (Remote arch, Arrays a) => a -> Par arch a
copyToRemote arrs =
  get =<< copyToRemoteAsync arrs

-- | Copy an array from the remote device to the host. This is synchronous with
-- respect to the calling thread, but the individual array payloads may
-- themselves be transferred concurrently.
--
{-# INLINEABLE copyToHost #-}
copyToHost :: (Remote arch, Arrays a) => a -> Par arch a
copyToHost arrs =
  block =<< copyToHostAsync arrs

-- | Copy arrays between two remote instances of the same type. This may be more
-- efficient than copying to the host and then to the second remote instance
-- (e.g. DMA between CUDA devices).
--
{-# INLINEABLE copyToPeer #-}
copyToPeer :: (Remote arch, Arrays a) => arch -> a -> Par arch a
copyToPeer peer arrs = do
  get =<< copyToPeerAsync peer arrs

-- | Read a single element from the remote array at the given row-major index.
-- This is synchronous with respect to both the host and remote device.
--
{-# INLINEABLE indexRemote #-}
indexRemote :: Remote arch => Array sh e -> Int -> Par arch e
indexRemote arr i =
  block =<< indexRemoteAsync arr i


-- Helpers for traversing the Arrays data structure
-- ------------------------------------------------

-- | Read a single element from an array at the given row-major index.
--
-- NOTE [indexArray for SIMD vector types]
--
-- These data types are stored contiguously in memory. Especially for backends
-- where data is stored in a separate memory space (i.e. GPUs) we should copy
-- all of those values in a single transaction, before unpacking them to the
-- appropriate Haskell value on the host (and/or, also store these values
-- contiguously in Haskell land). This default implementation does not do that;
-- see the implementation in the PTX backend for an example method for how to do
-- this properly.
--
{-# INLINEABLE runIndexArray #-}
runIndexArray
    :: forall m sh e. Monad m
    => (forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e) => ArrayData e -> Int -> m a)
    -> Array sh e
    -> Int
    -> m e
runIndexArray worker (Array _ adata) ix = toElt `liftM` indexR arrayElt adata ix
  where
    indexR :: ArrayEltR a -> ArrayData a -> Int -> m a
    indexR ArrayEltRunit    _  _ = return ()
    indexR ArrayEltRint     ad i = worker ad i
    indexR ArrayEltRint8    ad i = worker ad i
    indexR ArrayEltRint16   ad i = worker ad i
    indexR ArrayEltRint32   ad i = worker ad i
    indexR ArrayEltRint64   ad i = worker ad i
    indexR ArrayEltRword    ad i = worker ad i
    indexR ArrayEltRword8   ad i = worker ad i
    indexR ArrayEltRword16  ad i = worker ad i
    indexR ArrayEltRword32  ad i = worker ad i
    indexR ArrayEltRword64  ad i = worker ad i
    indexR ArrayEltRhalf    ad i = worker ad i
    indexR ArrayEltRfloat   ad i = worker ad i
    indexR ArrayEltRdouble  ad i = worker ad i
    indexR ArrayEltRchar    ad i = worker ad i
    indexR ArrayEltRcshort  ad i = CShort  `liftM` worker ad i
    indexR ArrayEltRcushort ad i = CUShort `liftM` worker ad i
    indexR ArrayEltRcint    ad i = CInt    `liftM` worker ad i
    indexR ArrayEltRcuint   ad i = CUInt   `liftM` worker ad i
    indexR ArrayEltRclong   ad i = CLong   `liftM` worker ad i
    indexR ArrayEltRculong  ad i = CULong  `liftM` worker ad i
    indexR ArrayEltRcllong  ad i = CLLong  `liftM` worker ad i
    indexR ArrayEltRcullong ad i = CULLong `liftM` worker ad i
    indexR ArrayEltRcchar   ad i = CChar   `liftM` worker ad i
    indexR ArrayEltRcschar  ad i = CSChar  `liftM` worker ad i
    indexR ArrayEltRcuchar  ad i = CUChar  `liftM` worker ad i
    indexR ArrayEltRcfloat  ad i = CFloat  `liftM` worker ad i
    indexR ArrayEltRcdouble ad i = CDouble `liftM` worker ad i
    indexR ArrayEltRbool    ad i = toBool  `liftM` worker ad i
      where
        toBool 0 = False
        toBool _ = True
    --
    indexR (ArrayEltRpair aeR1 aeR2) (AD_Pair ad1 ad2) i =
      (,) <$> indexR aeR1 ad1 i
          <*> indexR aeR2 ad2 i

    indexR (ArrayEltRvec2 r) (AD_V2 ad) i =
      let i' = 2*i
      in  V2 <$> indexR r ad i'
             <*> indexR r ad (i'+1)

    indexR (ArrayEltRvec3 r) (AD_V3 ad) i =
      let i' = 3*i
      in  V3 <$> indexR r ad i'
             <*> indexR r ad (i'+1)
             <*> indexR r ad (i'+2)

    indexR (ArrayEltRvec4 r) (AD_V4 ad) i =
      let i' = 4*i
      in  V4 <$> indexR r ad i'
             <*> indexR r ad (i'+1)
             <*> indexR r ad (i'+2)
             <*> indexR r ad (i'+3)

    indexR (ArrayEltRvec8 r) (AD_V8 ad) i =
      let i' = 8*i
      in  V8 <$> indexR r ad i'
             <*> indexR r ad (i'+1)
             <*> indexR r ad (i'+2)
             <*> indexR r ad (i'+3)
             <*> indexR r ad (i'+4)
             <*> indexR r ad (i'+5)
             <*> indexR r ad (i'+6)
             <*> indexR r ad (i'+7)

    indexR (ArrayEltRvec16 r) (AD_V16 ad) i =
      let i' = 16*i
      in  V16 <$> indexR r ad i'
              <*> indexR r ad (i'+1)
              <*> indexR r ad (i'+2)
              <*> indexR r ad (i'+3)
              <*> indexR r ad (i'+4)
              <*> indexR r ad (i'+5)
              <*> indexR r ad (i'+6)
              <*> indexR r ad (i'+7)
              <*> indexR r ad (i'+8)
              <*> indexR r ad (i'+9)
              <*> indexR r ad (i'+10)
              <*> indexR r ad (i'+11)
              <*> indexR r ad (i'+12)
              <*> indexR r ad (i'+13)
              <*> indexR r ad (i'+14)
              <*> indexR r ad (i'+15)


-- | Generalised function to traverse the Arrays structure
--
{-# INLINE runArrays #-}
runArrays
    :: forall m arrs. (Monad m, Arrays arrs)
    => arrs
    -> (forall sh e. Array sh e -> m (Array sh e))
    -> m arrs
runArrays arrs worker = toArr `liftM` runR (arrays arrs) (fromArr arrs)
  where
    runR :: ArraysR a -> a -> m a
    runR ArraysRunit             ()             = return ()
    runR ArraysRarray            arr            = worker arr
    runR (ArraysRpair aeR2 aeR1) (arrs2, arrs1) = liftM2 (,) (runR aeR2 arrs2) (runR aeR1 arrs1)

{-# INLINE runArraysAsync #-}
runArraysAsync
    :: forall arch arrs. (Async arch, Arrays arrs)
    => arrs
    -> (forall sh e. Array sh e -> Par arch (FutureR arch (Array sh e)))
    -> Par arch (FutureR arch arrs)
runArraysAsync arrs worker = toArr `liftF` runR (arrays arrs) (fromArr arrs)
  where
    runR :: ArraysR a -> a -> Par arch (FutureR arch a)
    runR ArraysRunit ()                         = newFull ()
    runR ArraysRarray arr                       = worker arr
    runR (ArraysRpair aeR2 aeR1) (arrs2, arrs1) = liftF2 (,) (runR aeR2 arrs2) (runR aeR1 arrs1)


-- | Generalised function to traverse the ArrayData structure with one
-- additional argument
--
{-# INLINE runArray #-}
runArray
    :: forall m sh e. Monad m
    => Array sh e
    -> (forall e' p. (ArrayElt e', ArrayPtrs e' ~ Ptr p, Storable p, Typeable p, Typeable e') => Int -> ArrayData e' -> m (ArrayData e'))
    -> m (Array sh e)
runArray (Array sh adata) worker = Array sh `liftM` runR arrayElt adata 1
  where
    runR :: ArrayEltR e' -> ArrayData e' -> Int -> m (ArrayData e')
    runR ArrayEltRint              ad                n = worker n ad
    runR ArrayEltRint8             ad                n = worker n ad
    runR ArrayEltRint16            ad                n = worker n ad
    runR ArrayEltRint32            ad                n = worker n ad
    runR ArrayEltRint64            ad                n = worker n ad
    runR ArrayEltRword             ad                n = worker n ad
    runR ArrayEltRword8            ad                n = worker n ad
    runR ArrayEltRword16           ad                n = worker n ad
    runR ArrayEltRword32           ad                n = worker n ad
    runR ArrayEltRword64           ad                n = worker n ad
    runR ArrayEltRhalf             ad                n = worker n ad
    runR ArrayEltRfloat            ad                n = worker n ad
    runR ArrayEltRdouble           ad                n = worker n ad
    runR ArrayEltRbool             ad                n = worker n ad
    runR ArrayEltRchar             ad                n = worker n ad
    runR ArrayEltRcshort           ad                n = worker n ad
    runR ArrayEltRcushort          ad                n = worker n ad
    runR ArrayEltRcint             ad                n = worker n ad
    runR ArrayEltRcuint            ad                n = worker n ad
    runR ArrayEltRclong            ad                n = worker n ad
    runR ArrayEltRculong           ad                n = worker n ad
    runR ArrayEltRcllong           ad                n = worker n ad
    runR ArrayEltRcullong          ad                n = worker n ad
    runR ArrayEltRcfloat           ad                n = worker n ad
    runR ArrayEltRcdouble          ad                n = worker n ad
    runR ArrayEltRcchar            ad                n = worker n ad
    runR ArrayEltRcschar           ad                n = worker n ad
    runR ArrayEltRcuchar           ad                n = worker n ad
    runR (ArrayEltRvec2 ae)        (AD_V2 ad)        n = liftM  AD_V2   (runR ae ad (n*2))
    runR (ArrayEltRvec3 ae)        (AD_V3 ad)        n = liftM  AD_V3   (runR ae ad (n*3))
    runR (ArrayEltRvec4 ae)        (AD_V4 ad)        n = liftM  AD_V4   (runR ae ad (n*4))
    runR (ArrayEltRvec8 ae)        (AD_V8 ad)        n = liftM  AD_V8   (runR ae ad (n*8))
    runR (ArrayEltRvec16 ae)       (AD_V16 ad)       n = liftM  AD_V16  (runR ae ad (n*16))
    runR (ArrayEltRpair aeR2 aeR1) (AD_Pair ad2 ad1) n = liftM2 AD_Pair (runR aeR2 ad2 n) (runR aeR1 ad1 n)
    runR ArrayEltRunit             AD_Unit           _ = return AD_Unit

{-# INLINE runArrayAsync #-}
runArrayAsync
    :: forall arch sh e. Async arch
    => Array sh e
    -> (forall e' p. (ArrayElt e', ArrayPtrs e' ~ Ptr p, Storable p, Typeable p, Typeable e') => Int -> ArrayData e' -> Par arch (FutureR arch (ArrayData e')))
    -> Par arch (FutureR arch (Array sh e))
runArrayAsync (Array sh adata) worker = Array sh `liftF` runR arrayElt adata 1
  where
    runR :: ArrayEltR e' -> ArrayData e' -> Int -> Par arch (FutureR arch (ArrayData e'))
    runR ArrayEltRint              ad                n = worker n ad
    runR ArrayEltRint8             ad                n = worker n ad
    runR ArrayEltRint16            ad                n = worker n ad
    runR ArrayEltRint32            ad                n = worker n ad
    runR ArrayEltRint64            ad                n = worker n ad
    runR ArrayEltRword             ad                n = worker n ad
    runR ArrayEltRword8            ad                n = worker n ad
    runR ArrayEltRword16           ad                n = worker n ad
    runR ArrayEltRword32           ad                n = worker n ad
    runR ArrayEltRword64           ad                n = worker n ad
    runR ArrayEltRhalf             ad                n = worker n ad
    runR ArrayEltRfloat            ad                n = worker n ad
    runR ArrayEltRdouble           ad                n = worker n ad
    runR ArrayEltRbool             ad                n = worker n ad
    runR ArrayEltRchar             ad                n = worker n ad
    runR ArrayEltRcshort           ad                n = worker n ad
    runR ArrayEltRcushort          ad                n = worker n ad
    runR ArrayEltRcint             ad                n = worker n ad
    runR ArrayEltRcuint            ad                n = worker n ad
    runR ArrayEltRclong            ad                n = worker n ad
    runR ArrayEltRculong           ad                n = worker n ad
    runR ArrayEltRcllong           ad                n = worker n ad
    runR ArrayEltRcullong          ad                n = worker n ad
    runR ArrayEltRcfloat           ad                n = worker n ad
    runR ArrayEltRcdouble          ad                n = worker n ad
    runR ArrayEltRcchar            ad                n = worker n ad
    runR ArrayEltRcschar           ad                n = worker n ad
    runR ArrayEltRcuchar           ad                n = worker n ad
    runR (ArrayEltRvec2 ae)        (AD_V2 ad)        n = liftF  AD_V2   (runR ae ad (n*2))
    runR (ArrayEltRvec3 ae)        (AD_V3 ad)        n = liftF  AD_V3   (runR ae ad (n*3))
    runR (ArrayEltRvec4 ae)        (AD_V4 ad)        n = liftF  AD_V4   (runR ae ad (n*4))
    runR (ArrayEltRvec8 ae)        (AD_V8 ad)        n = liftF  AD_V8   (runR ae ad (n*8))
    runR (ArrayEltRvec16 ae)       (AD_V16 ad)       n = liftF  AD_V16  (runR ae ad (n*16))
    runR (ArrayEltRpair aeR2 aeR1) (AD_Pair ad2 ad1) n = liftF2 AD_Pair (runR aeR2 ad2 n) (runR aeR1 ad1 n)
    runR ArrayEltRunit             AD_Unit           _ = newFull AD_Unit


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

