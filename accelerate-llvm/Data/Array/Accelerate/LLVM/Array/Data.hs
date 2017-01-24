{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Array.Data
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Array.Data (

  Remote(..),
  newRemote,
  newRemoteSubarray,   newRemoteSubarrayAsync,
  useRemote,           useRemoteAsync,
  copyToRemote,        copyToRemoteAsync,
  copyToHost,          copyToHostAsync,
  copyToPeer,          copyToPeerAsync,

  runIndexArray,
  runArrays,
  runArray,

  module Data.Array.Accelerate.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Execute.Async

-- standard library
import Control.Monad                                                ( liftM, liftM2 )
import Control.Monad.Trans
import Data.Typeable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Prelude


class Async arch => Remote arch where

  -- | Allocate a new uninitialised array on the remote device.
  --
  {-# INLINEABLE allocateRemote #-}
  allocateRemote :: (Shape sh, Elt e) => sh -> LLVM arch (Array sh e)
  allocateRemote sh = liftIO $ allocateArray sh

  -- | Use the given immutable array on the remote device. Since the source
  -- array is immutable, the allocator can evict and re-upload the data as
  -- necessary without copy-back.
  --
  {-# INLINEABLE useRemoteR #-}
  useRemoteR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ number of elements in the payload
      -> Maybe (StreamR arch)     -- ^ execute synchronously w.r.t. this execution stream
      -> ArrayData e              -- ^ array payload
      -> LLVM arch ()
  useRemoteR _ _ _ = return ()

  -- | Upload a section of an array from the host to the remote device. Only the
  -- elements between the given indices (inclusive left, exclusive right) are
  -- transferred.
  --
  {-# INLINEABLE copyToRemoteR #-}
  copyToRemoteR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ index of first element to copy
      -> Int
      -> Maybe (StreamR arch)     -- ^ execute synchronously w.r.t. this execution stream
      -> ArrayData e              -- ^ array payload
      -> LLVM arch ()
  copyToRemoteR _ _ _ _ = return ()

  -- | Upload a section of an array from the host to a separate remote array.
  -- Only the elements between the given indices (inclusive left, exclusive
  -- right) are transferred.
  --
  {-# INLINEABLE duplicateToRemoteR #-}
  duplicateToRemoteR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ index of first element to copy
      -> Int
      -> Maybe (StreamR arch)     -- ^ execute synchronously w.r.t. this execution stream
      -> ArrayData e              -- ^ source
      -> ArrayData e              -- ^ destination
      -> LLVM arch ()
  duplicateToRemoteR _ _ _ _ _ = return ()

  -- | Upload a 2D section of an array from the host to a separate remote array.
  -- Only the elements between the given indices (inclusive top-left, exclusive
  -- bottom-right) are transferred.
  --
  {-# INLINEABLE duplicateToRemote2DR #-}
  duplicateToRemote2DR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => (Int,Int)                -- ^ index of top-left corner
      -> (Int,Int)                -- ^ index of bottomr-right corner
      -> Int                      -- ^ the pitch (width) of the source array
      -> Maybe (StreamR arch)     -- ^ execute synchronously w.r.t. this execution stream
      -> ArrayData e              -- ^ source
      -> ArrayData e              -- ^ destination
      -> LLVM arch ()
  duplicateToRemote2DR _ _ _ _ _ _ = return ()

  -- | Copy a section of an array from the remote device back to the host. The
  -- elements between the given indices (inclusive left, exclusive right) are
  -- transferred.,v
  --
  {-# INLINEABLE copyToHostR #-}
  copyToHostR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ index of the first element to copy
      -> Int
      -> Maybe (StreamR arch)     -- ^ execute synchronously w.r.t. this execution stream
      -> ArrayData e              -- ^ array payload
      -> LLVM arch ()
  copyToHostR _ _ _ _ = return ()

  -- | Copy a section of an array between two remote instances of the same type.
  -- This may be more efficient than copying to the host and then to the second
  -- remote instance (e.g. DMA between two CUDA devices). The elements between
  -- the given indices (inclusive left, exclusive right) are transferred.
  --
  {-# INLINEABLE copyToPeerR #-}
  copyToPeerR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ index of the first element to copy
      -> Int
      -> arch                     -- ^ remote device to copy to
      -> Maybe (StreamR arch)     -- ^ execute synchronously w.r.t. this execution stream
      -> ArrayData e              -- ^ array payload
      -> LLVM arch ()
  copyToPeerR _ _ _ _ _ = return ()

  -- | Read a single element from the array at a given row-major index
  --
  {-# INLINEABLE indexRemote #-}
  indexRemote :: Array sh e -> Int -> LLVM arch e
  indexRemote (Array _ adata) i = return . toElt $! unsafeIndexArrayData adata i


-- | Create a new array from its representation on the host, and upload it to
-- a new remote array.
--
{-# INLINEABLE newRemote #-}
newRemote
    :: (Remote arch, Shape sh, Elt e)
    => sh
    -> (sh -> e)
    -> LLVM arch (Array sh e)
newRemote sh f =
  useRemote $! fromFunction sh f


-- | Upload an immutable array from the host to the remote device. This is
-- a synchronous operation in that it will not return until the transfer
-- completes, but the individual array payloads will be uploaded concurrently if
-- possible.
--
{-# INLINEABLE useRemote #-}
useRemote :: (Remote arch, Arrays arrs) => arrs -> LLVM arch arrs
useRemote arrs = do
  AsyncR _ a <- async (useRemoteAsync arrs)
  get a


-- | Upload an immutable array from the host to the remote device,
-- asynchronously. This will upload each array payload in a separate execution
-- stream, thereby making us of multiple memcpy engines (where available).
--
{-# INLINEABLE useRemoteAsync #-}
useRemoteAsync
    :: (Remote arch, Arrays arrs)
    => arrs
    -> StreamR arch
    -> LLVM arch (AsyncR arch arrs)
useRemoteAsync arrs stream = do
  arrs' <- runArrays arrs $ \arr@Array{} ->
    let n = size (shape arr)
    in  runArray arr $ \ad -> do
          s <- fork
          useRemoteR n (Just s) ad
          after stream =<< checkpoint s
          join s
          return ad
  --
  event  <- checkpoint stream   -- TLM: Assuming that adding events to a stream counts as things to wait for
  return $! AsyncR event arrs'


-- | Uploading existing arrays from the host to the remote device. This is
-- synchronous with respect to the calling thread, but the individual array
-- payloads may themselves be transferred concurrently.
--
{-# INLINEABLE copyToRemote #-}
copyToRemote :: (Remote arch, Arrays a) => a -> LLVM arch a
copyToRemote arrs = do
  AsyncR _ a <- async (copyToRemoteAsync arrs)
  get a


-- | Upload an existing array to the remote device, asynchronously.
--
{-# INLINEABLE copyToRemoteAsync #-}
copyToRemoteAsync
    :: (Remote arch, Arrays a)
    => a
    -> StreamR arch
    -> LLVM arch (AsyncR arch a)
copyToRemoteAsync arrs stream = do
  arrs' <- runArrays arrs $ \arr@Array{} ->
    let n = size (shape arr)
    in  runArray arr $ \ad -> do
          s <- fork
          copyToRemoteR 0 n (Just s) ad
          after stream =<< checkpoint s
          join s
          return ad
  --
  event  <- checkpoint stream
  return $! AsyncR event arrs'


-- | Create a new remote vector from a section of an existing vector,
-- asynchronously
--
{-# INLINEABLE newRemoteSubarray #-}
newRemoteSubarray :: (Remote arch, Shape sh, Elt e, sh :<= DIM2)
                  => sh           -- ^ starting index
                  -> sh           -- ^ extent
                  -> Array sh e   -- ^ source
                  -> LLVM arch (Array sh e)
newRemoteSubarray start n src = do
  AsyncR _ a <- async (newRemoteSubarrayAsync start n src)
  get a


-- | Create a new remote vector from a section of an existing vector,
-- asynchronously
--
{-# INLINEABLE newRemoteSubarrayAsync #-}
newRemoteSubarrayAsync :: forall arch sh e. (Remote arch, Shape sh, Elt e, sh :<= DIM2)
                       => sh           -- ^ starting index
                       -> sh           -- ^ number of elements
                       -> Array sh e   -- ^ source
                       -> StreamR arch
                       -> LLVM arch (AsyncR arch (Array sh e))
newRemoteSubarrayAsync start sh src stream = do
  dst <- allocateRemote sh
  runArray2 src dst $ \sr de -> do
    s <- fork
    transfer s sr de
    after stream =<< checkpoint s
    join s
  --
  event  <- checkpoint stream
  return $! AsyncR event dst
  where
    transfer :: (Typeable e', Typeable a, ArrayElt e', Storable a, ArrayPtrs e' ~ Ptr a)
             => StreamR arch
             -> ArrayData e'
             -> ArrayData e'
             -> LLVM arch ()
    transfer s sr de =
      case (maximumRank :: sh :<=: DIM2) of
        RankZ                     -> duplicateToRemoteR 0 1 (Just s) sr de
        RankSnoc RankZ            -> do
          let Z:.i = start
              Z:.n = sh
          duplicateToRemoteR i n (Just s) sr de
        RankSnoc (RankSnoc RankZ) -> do
          let Z:.y:.x          = start
              Z:.height:.width = sh
              Z:._:.pitch      = shape src
          duplicateToRemote2DR (y,x) (height,width) pitch (Just s) sr de
        _                         -> error "absurd"


-- | Copy an array from the remote device to the host. This is synchronous with
-- respect to the calling thread, but the individual array payloads may
-- themselves be transferred concurrently.
--
{-# INLINEABLE copyToHost #-}
copyToHost :: (Remote arch, Arrays a) => a -> LLVM arch a
copyToHost arrs = do
  AsyncR _ a <- async (copyToHostAsync arrs)
  get a


-- | Copy an array from the remote device to the host, asynchronously
--
{-# INLINEABLE copyToHostAsync #-}
copyToHostAsync
    :: (Remote arch, Arrays a)
    => a
    -> StreamR arch
    -> LLVM arch (AsyncR arch a)
copyToHostAsync arrs stream = do
  arrs' <- runArrays arrs $ \arr@Array{} ->
    let n = size (shape arr)
    in  runArray arr $ \ad -> do
          s <- fork
          copyToHostR 0 n (Just s) ad
          after stream =<< checkpoint s
          join s
          return ad
  --
  event  <- checkpoint stream
  return $! AsyncR event arrs'


-- | Copy arrays between two remote instances of the same type. This may be more
-- efficient than copying to the host and then to the second remote instance
-- (e.g. DMA between CUDA devices).
--
{-# INLINEABLE copyToPeer #-}
copyToPeer :: (Remote arch, Arrays a) => arch -> a -> LLVM arch a
copyToPeer peer arrs = do
  AsyncR _ a <- async (copyToPeerAsync peer arrs)
  get a


-- | As 'copyToPeer', asynchronously.
--
{-# INLINEABLE copyToPeerAsync #-}
copyToPeerAsync
    :: (Remote arch, Arrays a)
    => arch
    -> a
    -> StreamR arch
    -> LLVM arch (AsyncR arch a)
copyToPeerAsync peer arrs stream = do
  arrs' <- runArrays arrs $ \arr@Array{} ->
    let n = size (shape arr)
    in  runArray arr $ \ad -> do
          s <- fork
          copyToPeerR 0 n peer (Just s) ad
          after stream =<< checkpoint s
          join s
          return ad
  --
  event  <- checkpoint stream
  return $! AsyncR event arrs'


-- Helpers for traversing the Arrays data structure
-- ------------------------------------------------

-- |Read a single element from an array at the given row-major index.
--
{-# INLINEABLE runIndexArray #-}
runIndexArray
    :: forall m sh e. Monad m
    => (forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e) => ArrayData e -> Int -> m a)
    -> Array sh e
    -> Int
    -> m e
runIndexArray worker (Array _ adata) i = toElt `liftM` indexR arrayElt adata
  where
    indexR :: ArrayEltR a -> ArrayData a -> m a
    indexR ArrayEltRunit             _  = return ()
    indexR (ArrayEltRpair aeR1 aeR2) ad = liftM2 (,) (indexR aeR1 (fstArrayData ad))
                                                     (indexR aeR2 (sndArrayData ad))
    --
    indexR ArrayEltRint              ad = worker ad i
    indexR ArrayEltRint8             ad = worker ad i
    indexR ArrayEltRint16            ad = worker ad i
    indexR ArrayEltRint32            ad = worker ad i
    indexR ArrayEltRint64            ad = worker ad i
    indexR ArrayEltRword             ad = worker ad i
    indexR ArrayEltRword8            ad = worker ad i
    indexR ArrayEltRword16           ad = worker ad i
    indexR ArrayEltRword32           ad = worker ad i
    indexR ArrayEltRword64           ad = worker ad i
    indexR ArrayEltRfloat            ad = worker ad i
    indexR ArrayEltRdouble           ad = worker ad i
    indexR ArrayEltRchar             ad = worker ad i
    indexR ArrayEltRcshort           ad = CShort  `liftM` worker ad i
    indexR ArrayEltRcushort          ad = CUShort `liftM` worker ad i
    indexR ArrayEltRcint             ad = CInt    `liftM` worker ad i
    indexR ArrayEltRcuint            ad = CUInt   `liftM` worker ad i
    indexR ArrayEltRclong            ad = CLong   `liftM` worker ad i
    indexR ArrayEltRculong           ad = CULong  `liftM` worker ad i
    indexR ArrayEltRcllong           ad = CLLong  `liftM` worker ad i
    indexR ArrayEltRcullong          ad = CULLong `liftM` worker ad i
    indexR ArrayEltRcchar            ad = CChar   `liftM` worker ad i
    indexR ArrayEltRcschar           ad = CSChar  `liftM` worker ad i
    indexR ArrayEltRcuchar           ad = CUChar  `liftM` worker ad i
    indexR ArrayEltRcfloat           ad = CFloat  `liftM` worker ad i
    indexR ArrayEltRcdouble          ad = CDouble `liftM` worker ad i
    indexR ArrayEltRbool             ad = toBool  `liftM` worker ad i
      where
        toBool 0 = False
        toBool _ = True


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


-- | Generalised function to traverse the ArrayData structure with one
-- additional argument
--
{-# INLINE runArray #-}
runArray
    :: forall m sh e. Monad m
    => Array sh e
    -> (forall e' p. (ArrayElt e', ArrayPtrs e' ~ Ptr p, Storable p, Typeable p, Typeable e') => ArrayData e' -> m (ArrayData e'))
    -> m (Array sh e)
runArray (Array sh adata) worker = Array sh `liftM` runR arrayElt adata
  where
    runR :: ArrayEltR e' -> ArrayData e' -> m (ArrayData e')
    runR ArrayEltRunit             AD_Unit           = return AD_Unit
    runR (ArrayEltRpair aeR2 aeR1) (AD_Pair ad2 ad1) = liftM2 AD_Pair (runR aeR2 ad2) (runR aeR1 ad1)
    --
    runR ArrayEltRint              ad                = worker ad
    runR ArrayEltRint8             ad                = worker ad
    runR ArrayEltRint16            ad                = worker ad
    runR ArrayEltRint32            ad                = worker ad
    runR ArrayEltRint64            ad                = worker ad
    runR ArrayEltRword             ad                = worker ad
    runR ArrayEltRword8            ad                = worker ad
    runR ArrayEltRword16           ad                = worker ad
    runR ArrayEltRword32           ad                = worker ad
    runR ArrayEltRword64           ad                = worker ad
    runR ArrayEltRfloat            ad                = worker ad
    runR ArrayEltRdouble           ad                = worker ad
    runR ArrayEltRbool             ad                = worker ad
    runR ArrayEltRchar             ad                = worker ad
    runR ArrayEltRcshort           ad                = worker ad
    runR ArrayEltRcushort          ad                = worker ad
    runR ArrayEltRcint             ad                = worker ad
    runR ArrayEltRcuint            ad                = worker ad
    runR ArrayEltRclong            ad                = worker ad
    runR ArrayEltRculong           ad                = worker ad
    runR ArrayEltRcllong           ad                = worker ad
    runR ArrayEltRcullong          ad                = worker ad
    runR ArrayEltRcfloat           ad                = worker ad
    runR ArrayEltRcdouble          ad                = worker ad
    runR ArrayEltRcchar            ad                = worker ad
    runR ArrayEltRcschar           ad                = worker ad
    runR ArrayEltRcuchar           ad                = worker ad


-- | Generalised function to traverse two ArrayData structures with one
-- additional argument
--
{-# INLINE runArray2 #-}
runArray2
    :: forall m sh sh' e. Monad m
    => Array sh e
    -> Array sh' e
    -> (forall e' p. (ArrayElt e', ArrayPtrs e' ~ Ptr p, Storable p, Typeable p, Typeable e') => ArrayData e' -> ArrayData e' -> m ())
    -> m ()
runArray2 (Array _ adata1) (Array _ adata2) worker = runR arrayElt adata1 adata2
  where
    runR :: ArrayEltR e' -> ArrayData e' -> ArrayData e' -> m ()
    runR ArrayEltRunit             AD_Unit           AD_Unit = return ()
    runR (ArrayEltRpair aeR2 aeR1) (AD_Pair ad2 ad1) (AD_Pair ad2' ad1')
      = runR aeR2 ad2 ad2' >> runR aeR1 ad1 ad1' >> return ()
    --
    runR ArrayEltRint              ad                ad' = worker ad ad'
    runR ArrayEltRint8             ad                ad' = worker ad ad'
    runR ArrayEltRint16            ad                ad' = worker ad ad'
    runR ArrayEltRint32            ad                ad' = worker ad ad'
    runR ArrayEltRint64            ad                ad' = worker ad ad'
    runR ArrayEltRword             ad                ad' = worker ad ad'
    runR ArrayEltRword8            ad                ad' = worker ad ad'
    runR ArrayEltRword16           ad                ad' = worker ad ad'
    runR ArrayEltRword32           ad                ad' = worker ad ad'
    runR ArrayEltRword64           ad                ad' = worker ad ad'
    runR ArrayEltRfloat            ad                ad' = worker ad ad'
    runR ArrayEltRdouble           ad                ad' = worker ad ad'
    runR ArrayEltRbool             ad                ad' = worker ad ad'
    runR ArrayEltRchar             ad                ad' = worker ad ad'
    runR ArrayEltRcshort           ad                ad' = worker ad ad'
    runR ArrayEltRcushort          ad                ad' = worker ad ad'
    runR ArrayEltRcint             ad                ad' = worker ad ad'
    runR ArrayEltRcuint            ad                ad' = worker ad ad'
    runR ArrayEltRclong            ad                ad' = worker ad ad'
    runR ArrayEltRculong           ad                ad' = worker ad ad'
    runR ArrayEltRcllong           ad                ad' = worker ad ad'
    runR ArrayEltRcullong          ad                ad' = worker ad ad'
    runR ArrayEltRcfloat           ad                ad' = worker ad ad'
    runR ArrayEltRcdouble          ad                ad' = worker ad ad'
    runR ArrayEltRcchar            ad                ad' = worker ad ad'
    runR ArrayEltRcschar           ad                ad' = worker ad ad'
    runR ArrayEltRcuchar           ad                ad' = worker ad ad'
