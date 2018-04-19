{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Array.Data
-- Copyright   : [2014..2018] Trevor L. McDonell
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
import Data.Array.Accelerate.Error
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
import Control.Monad.Reader
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Prelude


-- | Remote memory management for the PTX target. Data can be copied
-- asynchronously using multiple execution engines whenever possible.
--
instance Remote PTX where
  {-# INLINEABLE allocateRemote   #-}
  {-# INLINEABLE indexRemoteAsync #-}
  {-# INLINEABLE useRemoteR       #-}
  {-# INLINEABLE copyToHostR      #-}
  {-# INLINEABLE copyToRemoteR    #-}
  allocateRemote !sh = do
    let !n = size sh
    arr <- liftIO $ allocateArray sh  -- shadow array on the host
    runArray arr (\m ad -> Prim.mallocArray (n*m) ad >> return ad)

  indexRemoteAsync  = runIndexArrayAsync Prim.indexArrayAsync
  useRemoteR        = Prim.useArrayAsync
  copyToHostR       = Prim.peekArrayAsync
  copyToRemoteR     = Prim.pokeArrayAsync
  copyToPeerR       = $internalError "copyToPeerR" "not supported yet"


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
{-# INLINEABLE copyToHostLazy #-}
copyToHostLazy
    :: Arrays arrs
    => arrs
    -> LLVM PTX arrs
copyToHostLazy arrs = do
  ptx   <- gets llvmTarget
  liftIO $ runArrays arrs $ \(Array sh adata) ->
    let
        peekR :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
              => ArrayData e
              -> UniqueArray a
              -> Int
              -> IO (UniqueArray a)
        peekR ad (UniqueArray uid (Lifetime ref weak fp)) n = do
          fp' <- unsafeInterleaveIO $
            evalPTX ptx $ do
              r  <- evalPar $ Prim.peekArrayAsync n ad
              !_ <- liftIO  $ wait r
              return fp
          --
          return $ UniqueArray uid (Lifetime ref weak fp')  -- not directly reusing 'fp'

        runR :: ArrayEltR e -> ArrayData e -> Int -> IO (ArrayData e)
        runR ArrayEltRunit              AD_Unit          _ = return AD_Unit
        runR (ArrayEltRpair aeR2 aeR1) (AD_Pair ad2 ad1) n = AD_Pair    <$> runR aeR2 ad2 n <*> runR aeR1 ad1 n
        runR (ArrayEltRvec2 aeR)       (AD_V2 ad)        n = AD_V2      <$> runR aeR ad (n*2)
        runR (ArrayEltRvec3 aeR)       (AD_V3 ad)        n = AD_V3      <$> runR aeR ad (n*3)
        runR (ArrayEltRvec4 aeR)       (AD_V4 ad)        n = AD_V4      <$> runR aeR ad (n*4)
        runR (ArrayEltRvec8 aeR)       (AD_V8 ad)        n = AD_V8      <$> runR aeR ad (n*8)
        runR (ArrayEltRvec16 aeR)      (AD_V16 ad)       n = AD_V16     <$> runR aeR ad (n*16)
        --
        runR ArrayEltRint           ad@(AD_Int ua)       n = AD_Int     <$> peekR ad ua n
        runR ArrayEltRint8          ad@(AD_Int8 ua)      n = AD_Int8    <$> peekR ad ua n
        runR ArrayEltRint16         ad@(AD_Int16 ua)     n = AD_Int16   <$> peekR ad ua n
        runR ArrayEltRint32         ad@(AD_Int32 ua)     n = AD_Int32   <$> peekR ad ua n
        runR ArrayEltRint64         ad@(AD_Int64 ua)     n = AD_Int64   <$> peekR ad ua n
        runR ArrayEltRword          ad@(AD_Word ua)      n = AD_Word    <$> peekR ad ua n
        runR ArrayEltRword8         ad@(AD_Word8 ua)     n = AD_Word8   <$> peekR ad ua n
        runR ArrayEltRword16        ad@(AD_Word16 ua)    n = AD_Word16  <$> peekR ad ua n
        runR ArrayEltRword32        ad@(AD_Word32 ua)    n = AD_Word32  <$> peekR ad ua n
        runR ArrayEltRword64        ad@(AD_Word64 ua)    n = AD_Word64  <$> peekR ad ua n
        runR ArrayEltRcshort        ad@(AD_CShort ua)    n = AD_CShort  <$> peekR ad ua n
        runR ArrayEltRcushort       ad@(AD_CUShort ua)   n = AD_CUShort <$> peekR ad ua n
        runR ArrayEltRcint          ad@(AD_CInt ua)      n = AD_CInt    <$> peekR ad ua n
        runR ArrayEltRcuint         ad@(AD_CUInt ua)     n = AD_CUInt   <$> peekR ad ua n
        runR ArrayEltRclong         ad@(AD_CLong ua)     n = AD_CLong   <$> peekR ad ua n
        runR ArrayEltRculong        ad@(AD_CULong ua)    n = AD_CULong  <$> peekR ad ua n
        runR ArrayEltRcllong        ad@(AD_CLLong ua)    n = AD_CLLong  <$> peekR ad ua n
        runR ArrayEltRcullong       ad@(AD_CULLong ua)   n = AD_CULLong <$> peekR ad ua n
        runR ArrayEltRhalf          ad@(AD_Half ua)      n = AD_Half    <$> peekR ad ua n
        runR ArrayEltRfloat         ad@(AD_Float ua)     n = AD_Float   <$> peekR ad ua n
        runR ArrayEltRdouble        ad@(AD_Double ua)    n = AD_Double  <$> peekR ad ua n
        runR ArrayEltRcfloat        ad@(AD_CFloat ua)    n = AD_CFloat  <$> peekR ad ua n
        runR ArrayEltRcdouble       ad@(AD_CDouble ua)   n = AD_CDouble <$> peekR ad ua n
        runR ArrayEltRbool          ad@(AD_Bool ua)      n = AD_Bool    <$> peekR ad ua n
        runR ArrayEltRchar          ad@(AD_Char ua)      n = AD_Char    <$> peekR ad ua n
        runR ArrayEltRcchar         ad@(AD_CChar ua)     n = AD_CChar   <$> peekR ad ua n
        runR ArrayEltRcschar        ad@(AD_CSChar ua)    n = AD_CSChar  <$> peekR ad ua n
        runR ArrayEltRcuchar        ad@(AD_CUChar ua)    n = AD_CUChar  <$> peekR ad ua n
    in
    Array sh <$> runR arrayElt adata (R.size sh)


{-# INLINE runIndexArrayAsync #-}
runIndexArrayAsync
    :: (forall e' p. (ArrayElt e', ArrayPtrs e' ~ Ptr p, Storable p, Typeable p, Typeable e') => ArrayData e' -> Int -> Int -> Par PTX (Future (ArrayData e')))
    -> Array sh e
    -> Int
    -> Par PTX (Future e)
runIndexArrayAsync worker (Array _ adata) i = (toElt . flip unsafeIndexArrayData 0) `liftF` runR arrayElt adata 1
  where
    runR :: ArrayEltR e' -> ArrayData e' -> Int -> Par PTX (Future (ArrayData e'))
    runR ArrayEltRint              ad                n = worker ad i n
    runR ArrayEltRint8             ad                n = worker ad i n
    runR ArrayEltRint16            ad                n = worker ad i n
    runR ArrayEltRint32            ad                n = worker ad i n
    runR ArrayEltRint64            ad                n = worker ad i n
    runR ArrayEltRword             ad                n = worker ad i n
    runR ArrayEltRword8            ad                n = worker ad i n
    runR ArrayEltRword16           ad                n = worker ad i n
    runR ArrayEltRword32           ad                n = worker ad i n
    runR ArrayEltRword64           ad                n = worker ad i n
    runR ArrayEltRhalf             ad                n = worker ad i n
    runR ArrayEltRfloat            ad                n = worker ad i n
    runR ArrayEltRdouble           ad                n = worker ad i n
    runR ArrayEltRbool             ad                n = worker ad i n
    runR ArrayEltRchar             ad                n = worker ad i n
    runR ArrayEltRcshort           ad                n = worker ad i n
    runR ArrayEltRcushort          ad                n = worker ad i n
    runR ArrayEltRcint             ad                n = worker ad i n
    runR ArrayEltRcuint            ad                n = worker ad i n
    runR ArrayEltRclong            ad                n = worker ad i n
    runR ArrayEltRculong           ad                n = worker ad i n
    runR ArrayEltRcllong           ad                n = worker ad i n
    runR ArrayEltRcullong          ad                n = worker ad i n
    runR ArrayEltRcfloat           ad                n = worker ad i n
    runR ArrayEltRcdouble          ad                n = worker ad i n
    runR ArrayEltRcchar            ad                n = worker ad i n
    runR ArrayEltRcschar           ad                n = worker ad i n
    runR ArrayEltRcuchar           ad                n = worker ad i n
    runR (ArrayEltRvec2 ae)        (AD_V2 ad)        n = liftF  AD_V2   (runR ae ad (n*2))
    runR (ArrayEltRvec3 ae)        (AD_V3 ad)        n = liftF  AD_V3   (runR ae ad (n*3))
    runR (ArrayEltRvec4 ae)        (AD_V4 ad)        n = liftF  AD_V4   (runR ae ad (n*4))
    runR (ArrayEltRvec8 ae)        (AD_V8 ad)        n = liftF  AD_V8   (runR ae ad (n*8))
    runR (ArrayEltRvec16 ae)       (AD_V16 ad)       n = liftF  AD_V16  (runR ae ad (n*16))
    runR (ArrayEltRpair aeR2 aeR1) (AD_Pair ad2 ad1) n = liftF2 AD_Pair (runR aeR2 ad2 n) (runR aeR1 ad1 n)
    runR ArrayEltRunit             AD_Unit           _ = newFull AD_Unit

    -- Don't create a new execution stream for these sub transfers
    liftF :: (a -> b) -> Par PTX (Future a) -> Par PTX (Future b)
    liftF f x = do
      r  <- new
      x' <- x
      put r . f =<< get x'
      return r

    liftF2 :: (a -> b -> c) -> Par PTX (Future a) -> Par PTX (Future b) -> Par PTX (Future c)
    liftF2 f x y = do
      r  <- new
      x' <- x
      y' <- y
      put r =<< liftM2 f (get x') (get y')
      return r


{--
-- | Clone an array into a newly allocated array on the device.
--
cloneArrayAsync
    :: (Shape sh, Elt e)
    => Stream
    -> Array sh e
    -> LLVM PTX (Array sh e)
cloneArrayAsync stream arr@(Array _ src) = do
  out@(Array _ dst) <- allocateRemote sh
  copyR arrayElt src dst (size sh)
  return out
  where
    sh  = shape arr

    copyR :: ArrayEltR e -> ArrayData e -> ArrayData e -> Int -> LLVM PTX ()
    copyR ArrayEltRunit             _   _   _ = return ()
    copyR (ArrayEltRpair aeR1 aeR2) ad1 ad2 n = copyR aeR1 (fstArrayData ad1) (fstArrayData ad2) n >>
                                                copyR aeR2 (sndArrayData ad1) (sndArrayData ad2) n
    --
    copyR (ArrayEltRvec2 aeR)  (AD_V2 ad1)  (AD_V2 ad2)  n = copyR aeR ad1 ad2 (n*2)
    copyR (ArrayEltRvec3 aeR)  (AD_V3 ad1)  (AD_V3 ad2)  n = copyR aeR ad1 ad2 (n*3)
    copyR (ArrayEltRvec4 aeR)  (AD_V4 ad1)  (AD_V4 ad2)  n = copyR aeR ad1 ad2 (n*4)
    copyR (ArrayEltRvec8 aeR)  (AD_V8 ad1)  (AD_V8 ad2)  n = copyR aeR ad1 ad2 (n*8)
    copyR (ArrayEltRvec16 aeR) (AD_V16 ad1) (AD_V16 ad2) n = copyR aeR ad1 ad2 (n*16)
    --
    copyR ArrayEltRint              ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRint8             ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRint16            ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRint32            ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRint64            ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRword             ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRword8            ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRword16           ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRword32           ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRword64           ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRhalf             ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRfloat            ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRdouble           ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRbool             ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRchar             ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcshort           ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcushort          ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcint             ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcuint            ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRclong            ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRculong           ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcllong           ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcullong          ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcfloat           ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcdouble          ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcchar            ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcschar           ad1 ad2 n = copyPrim ad1 ad2 n
    copyR ArrayEltRcuchar           ad1 ad2 n = copyPrim ad1 ad2 n

    copyPrim
        :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Storable a, Typeable a)
        => ArrayData e
        -> ArrayData e
        -> Int
        -> LLVM PTX ()
    copyPrim !a1 !a2 !m = Prim.copyArrayAsync stream m a1 a2
--}

