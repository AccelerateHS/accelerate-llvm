{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Array.Data
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Array.Data (

  Remote(..),
  newRemote,
  copyToRemote, copyToRemoteAsync,
  copyToHost,   copyToHostAsync,
  copyToPeer,   copyToPeerAsync,

  runIndexArray,
  runArrays,
  runArray,

  module Data.Array.Accelerate.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation               ( size )
import Data.Array.Accelerate.Array.Sugar                        hiding ( size )

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Execute.Async

-- standard library
import Control.Monad
import Control.Monad.Trans
import Data.Typeable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


class Async arch => Remote arch where

  -- | Allocate a new uninitialised array on the remote device.
  --
  {-# INLINEABLE allocateRemote #-}
  allocateRemote :: (Shape sh, Elt e) => sh -> LLVM arch (Array sh e)
  allocateRemote sh = liftIO $ allocateArray sh

  -- | Upload a section of an array from the host to the remote device. Only the
  -- elements between the given indices (inclusive left, exclusive right) are
  -- transferred.
  --
  {-# INLINEABLE copyToRemoteR #-}
  copyToRemoteR :: Int -> Int -> Maybe (StreamR arch) -> Array sh e -> LLVM arch ()
  copyToRemoteR _ _ _ _ = return ()

  -- | Copy a section of an array from the remote device back to the host. The
  -- elements between the given indices (inclusive left, exclusive right) are
  -- transferred.
  --
  {-# INLINEABLE copyToHostR #-}
  copyToHostR :: Int -> Int -> Maybe (StreamR arch) -> Array sh e -> LLVM arch ()
  copyToHostR _ _ _ _ = return ()

  -- | Copy a section of an array between two remote instances of the same type.
  -- This may be more efficient than copying to the host and then to the second
  -- remote instance (e.g. DMA between two CUDA devices). The elements between
  -- the given indices (inclusive left, exclusive right) are transferred.
  --
  {-# INLINEABLE copyToPeerR #-}
  copyToPeerR :: Int -> Int -> arch -> Maybe (StreamR arch) -> Array sh e -> LLVM arch ()
  copyToPeerR _ _ _ _ _ = return ()

  -- | Read a single element from the array at a given row-major index
  --
  {-# INLINEABLE indexRemote #-}
  indexRemote :: Array sh e -> Int -> LLVM arch e
  indexRemote (Array _ adata) i = return . toElt $! unsafeIndexArrayData adata i


-- CPP hackery to generate the cases where we dispatch to the worker function handling
-- elementary types.
--
-- TODO: Turn me into Template Haskell so that I can be exported and reused.
--
#define mkPrimDispatch(dispatcher,worker)                                       \
; dispatcher ArrayEltRint     = worker                                          \
; dispatcher ArrayEltRint8    = worker                                          \
; dispatcher ArrayEltRint16   = worker                                          \
; dispatcher ArrayEltRint32   = worker                                          \
; dispatcher ArrayEltRint64   = worker                                          \
; dispatcher ArrayEltRword    = worker                                          \
; dispatcher ArrayEltRword8   = worker                                          \
; dispatcher ArrayEltRword16  = worker                                          \
; dispatcher ArrayEltRword32  = worker                                          \
; dispatcher ArrayEltRword64  = worker                                          \
; dispatcher ArrayEltRfloat   = worker                                          \
; dispatcher ArrayEltRdouble  = worker                                          \
; dispatcher ArrayEltRbool    = worker                                          \
; dispatcher ArrayEltRchar    = worker                                          \
; dispatcher ArrayEltRcshort  = worker                                          \
; dispatcher ArrayEltRcushort = worker                                          \
; dispatcher ArrayEltRcint    = worker                                          \
; dispatcher ArrayEltRcuint   = worker                                          \
; dispatcher ArrayEltRclong   = worker                                          \
; dispatcher ArrayEltRculong  = worker                                          \
; dispatcher ArrayEltRcllong  = worker                                          \
; dispatcher ArrayEltRcullong = worker                                          \
; dispatcher ArrayEltRcfloat  = worker                                          \
; dispatcher ArrayEltRcdouble = worker                                          \
; dispatcher ArrayEltRcchar   = worker                                          \
; dispatcher ArrayEltRcschar  = worker                                          \
; dispatcher ArrayEltRcuchar  = worker                                          \
; dispatcher _                = error "mkPrimDispatcher: not primitive"



-- |Create a new array from its representation on the host, and upload it to a
-- new remote array.
--
{-# INLINEABLE newRemote #-}
newRemote
    :: (Remote arch, Shape sh, Elt e)
    => sh
    -> (sh -> e)
    -> LLVM arch (Array sh e)
newRemote sh f =
  copyToRemote $! newArray sh f


-- | Uploading existing arrays from the host to the remote device
--
{-# INLINEABLE copyToRemote #-}
copyToRemote :: (Remote arch, Arrays a) => a -> LLVM arch a
copyToRemote arrs = do
  runArrays (\arr@(Array sh _) -> copyToRemoteR 0 (size sh) Nothing arr) arrs
  return arrs

-- | Upload an existing array to the remote device, asynchronously.
--
{-# INLINEABLE copyToRemoteAsync #-}
copyToRemoteAsync :: (Remote arch, Arrays a) => StreamR arch -> a -> LLVM arch (AsyncR arch a)
copyToRemoteAsync stream arrs = do
  async stream $ do runArrays (\arr@(Array sh _) -> copyToRemoteR 0 (size sh) (Just stream) arr) arrs
                    return arrs


-- | Copy an array from the remote device to the host.
--
{-# INLINEABLE copyToHost #-}
copyToHost :: (Remote arch, Arrays a) => a -> LLVM arch a
copyToHost arrs = do
  runArrays (\arr@(Array sh _) -> copyToHostR 0 (size sh) Nothing arr) arrs
  return arrs

-- | Copy an array from the remote device to the host, asynchronously
--
{-# INLINEABLE copyToHostAsync #-}
copyToHostAsync :: (Remote arch, Arrays a) => StreamR arch -> a -> LLVM arch (AsyncR arch a)
copyToHostAsync stream arrs = do
  async stream $ do runArrays (\arr@(Array sh _) -> copyToHostR 0 (size sh) (Just stream) arr) arrs
                    return arrs

-- | Copy arrays between two remote instances of the same type. This may be more
-- efficient than copying to the host and then to the second remote instance
-- (e.g. DMA between CUDA devices).
--
{-# INLINEABLE copyToPeer #-}
copyToPeer :: (Remote arch, Arrays a) => arch -> a -> LLVM arch a
copyToPeer peer arrs = do
  runArrays (\arr@(Array sh _) -> copyToPeerR 0 (size sh) peer Nothing arr) arrs
  return arrs

-- | As 'copyToPeer', asynchronously.
--
{-# INLINEABLE copyToPeerAsync #-}
copyToPeerAsync :: (Remote arch, Arrays a) => arch -> StreamR arch -> a -> LLVM arch (AsyncR arch a)
copyToPeerAsync peer stream arrs =
  async stream $ do runArrays (\arr@(Array sh _) -> copyToPeerR 0 (size sh) peer (Just stream) arr) arrs
                    return arrs


-- Helpers for traversing the Arrays data structure
-- ------------------------------------------------

-- |Read a single element from an array at the given row-major index.
--
{-# INLINEABLE runIndexArray #-}
runIndexArray
    :: forall m sh e. Monad m
    => (forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a) => ArrayData e -> Int -> m a)
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
    => (forall sh e. Array sh e -> m ())
    -> arrs
    -> m ()
runArrays worker arrs = runR (arrays arrs) (fromArr arrs)
  where
    runR :: ArraysR a -> a -> m ()
    runR ArraysRunit             ()             = return ()
    runR ArraysRarray            arr            = worker arr
    runR (ArraysRpair aeR1 aeR2) (arrs1, arrs2) = runR aeR1 arrs1 >> runR aeR2 arrs2


-- | Generalised function to traverse the ArrayData structure with one
-- additional argument

{-# INLINE runArray #-}
runArray
    :: forall m sh e. Monad m
    => (forall e' p. (ArrayElt e', ArrayPtrs e' ~ Ptr p, Storable p, Typeable p) => ArrayData e' -> m ())
    -> Array sh e
    -> m ()
runArray worker (Array _ adata) = runR arrayElt adata
  where
    runR :: ArrayEltR e' -> ArrayData e' -> m ()
    runR ArrayEltRunit             _  = return ()
    runR (ArrayEltRpair aeR1 aeR2) ad = runR aeR1 (fstArrayData ad) >>
                                        runR aeR2 (sndArrayData ad)
    runR aer                       ad = runW aer ad
    --
    runW :: ArrayEltR e' -> ArrayData e' -> m ()
    mkPrimDispatch(runW, worker)

