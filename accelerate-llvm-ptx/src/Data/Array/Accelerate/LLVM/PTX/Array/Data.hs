{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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
import qualified Data.Array.Accelerate.Array.Representation         as R

import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.State
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim          as Prim
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Event       as Event

-- standard library
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State                                          ( liftIO, gets )
import Data.IORef
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Prelude

import GHC.Int                                                      ( Int(..) )


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
    arr    <- liftIO $ allocateArray sh  -- shadow array on the host
    liftPar $ runArray arr (\m ad -> Prim.mallocArray (n*m) ad >> return ad)

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
    => Future arrs
    -> Par PTX arrs
copyToHostLazy (Future ref) = do
  ptx   <- gets llvmTarget
  arrs  <- liftIO $ do
    ivar  <- readIORef ref
    arrs  <- case ivar of -- peek at the underlying array
               Full a         -> return a
               Pending _ _ a  -> return a
               Empty          -> $internalError "copyToHostLazy" "blocked on an IVar"
    --
    runArrays arrs $ \(Array sh adata) ->
      let
          peekR :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
                => ArrayData e
                -> Int
                -> IO (ArrayData e)
          peekR ad n =
            unsafeInterleaveIO . evalPTX ptx . evalPar $ do
              liftIO $ do
                ivar' <- readIORef ref
                case ivar' of
                  Pending event _ _ -> do
                    Event.block event
                    writeIORef ref (Full arrs)
                  _                 -> return ()

              block =<< Prim.peekArrayAsync n ad

          runR :: ArrayEltR e -> ArrayData e -> Int -> IO (ArrayData e)
          runR ArrayEltRunit    !_  !_ = return AD_Unit
          runR ArrayEltRint     !ad !n = peekR ad n
          runR ArrayEltRint8    !ad !n = peekR ad n
          runR ArrayEltRint16   !ad !n = peekR ad n
          runR ArrayEltRint32   !ad !n = peekR ad n
          runR ArrayEltRint64   !ad !n = peekR ad n
          runR ArrayEltRword    !ad !n = peekR ad n
          runR ArrayEltRword8   !ad !n = peekR ad n
          runR ArrayEltRword16  !ad !n = peekR ad n
          runR ArrayEltRword32  !ad !n = peekR ad n
          runR ArrayEltRword64  !ad !n = peekR ad n
          runR ArrayEltRhalf    !ad !n = peekR ad n
          runR ArrayEltRfloat   !ad !n = peekR ad n
          runR ArrayEltRdouble  !ad !n = peekR ad n
          runR ArrayEltRbool    !ad !n = peekR ad n
          runR ArrayEltRchar    !ad !n = peekR ad n
          --
          runR (ArrayEltRpair !aeR2 !aeR1) (AD_Pair ad2 ad1) !n = AD_Pair   <$> runR aeR2 ad2 n <*> runR aeR1 ad1 n
          runR (ArrayEltRvec  !aeR)        (AD_Vec w# !ad)   !n = AD_Vec w# <$> runR aeR ad (I# w# * n)
      in
      Array sh <$> runR arrayElt adata (R.size sh)
  --
  return arrs


-- | Clone an array into a newly allocated array on the device.
--
cloneArrayAsync
    :: forall sh e. (Shape sh, Elt e)
    => Array sh e
    -> Par PTX (Future (Array sh e))
cloneArrayAsync arr@(Array _ src) = do
  Array _ dst <- allocateRemote sh  :: Par PTX (Array sh e)
  Array (fromElt sh) `liftF` copyR arrayElt src dst (size sh)
  where
    sh  = shape arr

    copyR :: ArrayEltR s -> ArrayData s -> ArrayData s -> Int -> Par PTX (Future (ArrayData s))
    copyR ArrayEltRunit    !_   !_   !_ = newFull AD_Unit
    copyR ArrayEltRint     !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRint8    !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRint16   !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRint32   !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRint64   !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRword    !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRword8   !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRword16  !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRword32  !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRword64  !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRhalf    !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRfloat   !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRdouble  !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRbool    !ad1 !ad2 !n = copyPrim ad1 ad2 n
    copyR ArrayEltRchar    !ad1 !ad2 !n = copyPrim ad1 ad2 n
    --
    copyR (ArrayEltRpair !aeR1 !aeR2) !ad1 !ad2 !n =
      liftF2 AD_Pair (copyR aeR1 (fstArrayData ad1) (fstArrayData ad2) n)
                     (copyR aeR2 (sndArrayData ad1) (sndArrayData ad2) n)
    --
    copyR (ArrayEltRvec !aeR) (AD_Vec w# !ad1) (AD_Vec _ !ad2) !n =
      AD_Vec w# `liftF` copyR aeR ad1 ad2 (I# w# * n)

    copyPrim
        :: (ArrayElt s, ArrayPtrs s ~ Ptr a, Typeable s, Storable a, Typeable a)
        => ArrayData s
        -> ArrayData s
        -> Int
        -> Par PTX (Future (ArrayData s))
    copyPrim !a1 !a2 !m = Prim.copyArrayAsync m a1 a2

    liftF :: Async arch
          => (a -> b)
          -> Par arch (FutureR arch a)
          -> Par arch (FutureR arch b)
    liftF f x = do
      r  <- new
      x' <- x
      put r . f =<< get x'  -- don't create a new execution stream for this
      return r

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

