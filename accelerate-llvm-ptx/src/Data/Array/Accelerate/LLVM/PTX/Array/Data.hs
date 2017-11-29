{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Array.Data
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Array.Data (

  module Data.Array.Accelerate.LLVM.Array.Data,
  module Data.Array.Accelerate.LLVM.PTX.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Unique                       ( UniqueArray(..) )
import Data.Array.Accelerate.Lifetime                           ( Lifetime(..) )
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.State
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim      as Prim

-- standard library
import Control.Applicative
import Control.Monad.State                                      ( liftIO, gets )
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Prelude


-- Instance of remote array memory management for the PTX target
--
instance Remote PTX where

  {-# INLINEABLE allocateRemote #-}
  allocateRemote !sh = do
    arr <- liftIO $ allocateArray sh
    runArray arr (\ad -> Prim.mallocArray (size sh) ad >> return ad)

  {-# INLINEABLE useRemoteR #-}
  useRemoteR !n !mst !ad = do
    case mst of
      Nothing -> Prim.useArray         n ad
      Just st -> Prim.useArrayAsync st n ad

  {-# INLINEABLE copyToRemoteR #-}
  copyToRemoteR !from !to !mst !ad = do
    case mst of
      Nothing -> Prim.pokeArrayR         from to ad
      Just st -> Prim.pokeArrayAsyncR st from to ad

  {-# INLINEABLE copyToHostR #-}
  copyToHostR !from !to !mst !ad = do
    case mst of
      Nothing -> Prim.peekArrayR         from to ad
      Just st -> Prim.peekArrayAsyncR st from to ad

  {-# INLINEABLE copyToPeerR #-}
  copyToPeerR !from !to !dst !mst !ad = do
    case mst of
      Nothing -> Prim.copyArrayPeerR      (ptxContext dst) (ptxMemoryTable dst)    from to ad
      Just st -> Prim.copyArrayPeerAsyncR (ptxContext dst) (ptxMemoryTable dst) st from to ad

  {-# INLINEABLE indexRemote #-}
  indexRemote arr i =
    runIndexArray Prim.indexArray arr i


-- | Copy an array from the remote device to the host. Although the Accelerate
-- program is hyper-strict and will evaluate the computation as soon as any part
-- of it is demanded, the individual array payloads are copied back to the host
-- _only_ as they are demanded by the Haskell program. This has several
-- consequences:
--
--   1. If the device has multiple memcpy engines, only one will be used. The
--      transfers are however associated with a non-default stream.
--
--   2. Using 'seq' to force an Array to head-normal form will initiate the
--      computation, but not transfer the results back to the host. Requesting
--      an array element or using 'deepseq' to force to normal form is required
--      to actually transfer the data.
--
copyToHostLazy
    :: Arrays arrs
    => arrs
    -> LLVM PTX arrs
copyToHostLazy arrs = do
  ptx   <- gets llvmTarget
  liftIO $ runArrays arrs $ \(Array sh adata) ->
    let
        n :: Int
        n = R.size sh

        peekR :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
              => ArrayData e
              -> UniqueArray a
              -> IO (UniqueArray a)
        peekR ad (UniqueArray uid (Lifetime ref weak fp)) = do
          fp' <- unsafeInterleaveIO $
            evalPTX ptx $ do
              s <- fork
              copyToHostR 0 n (Just s) ad
              e <- checkpoint s
              block e
              join s
              return fp
          return $ UniqueArray uid (Lifetime ref weak fp')

        runR :: ArrayEltR e -> ArrayData e -> IO (ArrayData e)
        runR ArrayEltRunit              AD_Unit          = return AD_Unit
        runR (ArrayEltRpair aeR2 aeR1) (AD_Pair ad2 ad1) = AD_Pair    <$> runR aeR2 ad2 <*> runR aeR1 ad1
        runR ArrayEltRint           ad@(AD_Int ua)       = AD_Int     <$> peekR ad ua
        runR ArrayEltRint8          ad@(AD_Int8 ua)      = AD_Int8    <$> peekR ad ua
        runR ArrayEltRint16         ad@(AD_Int16 ua)     = AD_Int16   <$> peekR ad ua
        runR ArrayEltRint32         ad@(AD_Int32 ua)     = AD_Int32   <$> peekR ad ua
        runR ArrayEltRint64         ad@(AD_Int64 ua)     = AD_Int64   <$> peekR ad ua
        runR ArrayEltRword          ad@(AD_Word ua)      = AD_Word    <$> peekR ad ua
        runR ArrayEltRword8         ad@(AD_Word8 ua)     = AD_Word8   <$> peekR ad ua
        runR ArrayEltRword16        ad@(AD_Word16 ua)    = AD_Word16  <$> peekR ad ua
        runR ArrayEltRword32        ad@(AD_Word32 ua)    = AD_Word32  <$> peekR ad ua
        runR ArrayEltRword64        ad@(AD_Word64 ua)    = AD_Word64  <$> peekR ad ua
        runR ArrayEltRcshort        ad@(AD_CShort ua)    = AD_CShort  <$> peekR ad ua
        runR ArrayEltRcushort       ad@(AD_CUShort ua)   = AD_CUShort <$> peekR ad ua
        runR ArrayEltRcint          ad@(AD_CInt ua)      = AD_CInt    <$> peekR ad ua
        runR ArrayEltRcuint         ad@(AD_CUInt ua)     = AD_CUInt   <$> peekR ad ua
        runR ArrayEltRclong         ad@(AD_CLong ua)     = AD_CLong   <$> peekR ad ua
        runR ArrayEltRculong        ad@(AD_CULong ua)    = AD_CULong  <$> peekR ad ua
        runR ArrayEltRcllong        ad@(AD_CLLong ua)    = AD_CLLong  <$> peekR ad ua
        runR ArrayEltRcullong       ad@(AD_CULLong ua)   = AD_CULLong <$> peekR ad ua
        runR ArrayEltRfloat         ad@(AD_Float ua)     = AD_Float   <$> peekR ad ua
        runR ArrayEltRdouble        ad@(AD_Double ua)    = AD_Double  <$> peekR ad ua
        runR ArrayEltRcfloat        ad@(AD_CFloat ua)    = AD_CFloat  <$> peekR ad ua
        runR ArrayEltRcdouble       ad@(AD_CDouble ua)   = AD_CDouble <$> peekR ad ua
        runR ArrayEltRbool          ad@(AD_Bool ua)      = AD_Bool    <$> peekR ad ua
        runR ArrayEltRchar          ad@(AD_Char ua)      = AD_Char    <$> peekR ad ua
        runR ArrayEltRcchar         ad@(AD_CChar ua)     = AD_CChar   <$> peekR ad ua
        runR ArrayEltRcschar        ad@(AD_CSChar ua)    = AD_CSChar  <$> peekR ad ua
        runR ArrayEltRcuchar        ad@(AD_CUChar ua)    = AD_CUChar  <$> peekR ad ua
    in
    Array sh <$> runR arrayElt adata


-- | Clone an array into a newly allocated array on the device.
--
cloneArrayAsync
    :: (Shape sh, Elt e)
    => Stream
    -> Array sh e
    -> LLVM PTX (Array sh e)
cloneArrayAsync stream arr@(Array _ src) = do
  out@(Array _ dst) <- allocateRemote sh
  copyR arrayElt src dst
  return out
  where
    sh  = shape arr
    n   = size sh

    copyR :: ArrayEltR e -> ArrayData e -> ArrayData e -> LLVM PTX ()
    copyR ArrayEltRunit             _   _   = return ()
    copyR (ArrayEltRpair aeR1 aeR2) ad1 ad2 = copyR aeR1 (fstArrayData ad1) (fstArrayData ad2) >>
                                              copyR aeR2 (sndArrayData ad1) (sndArrayData ad2)
    --
    copyR ArrayEltRint              ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRint8             ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRint16            ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRint32            ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRint64            ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRword             ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRword8            ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRword16           ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRword32           ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRword64           ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRfloat            ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRdouble           ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRbool             ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRchar             ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcshort           ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcushort          ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcint             ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcuint            ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRclong            ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRculong           ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcllong           ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcullong          ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcfloat           ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcdouble          ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcchar            ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcschar           ad1 ad2 = copyPrim ad1 ad2
    copyR ArrayEltRcuchar           ad1 ad2 = copyPrim ad1 ad2

    copyPrim
        :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Storable a, Typeable a)
        => ArrayData e
        -> ArrayData e
        -> LLVM PTX ()
    copyPrim a1 a2 = Prim.copyArrayAsync stream n a1 a2

