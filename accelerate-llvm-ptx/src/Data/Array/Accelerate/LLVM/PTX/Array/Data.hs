{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Array.Data
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Array.Data (

  module Data.Array.Accelerate.LLVM.Array.Data,
  module Data.Array.Accelerate.LLVM.PTX.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                            as S hiding ( Any )
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import qualified Data.Array.Accelerate.Array.Representation         as R

import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.State
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim          as Prim

-- standard library
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State                                          ( gets )
import Data.IORef
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Prelude

import GHC.Int                                                      ( Int(..) )
#if __GLASGOW_HASKELL__ >= 806
import Text.Printf
import GHC.Exts.Heap
import qualified Data.Array.Accelerate.Debug                        as Debug
#elif UNBOXED_TUPLES
import Util                                                         ( ghciTablesNextToCode )
import GHC.Exts
import GHCi.InfoTable
#include "rts/storage/ClosureTypes.h"
#endif


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
    let !n = S.size sh
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
    :: ArraysR arrs
    -> FutureArraysR PTX arrs
    -> Par PTX arrs
copyToHostLazy ArraysRunit () = return ()
copyToHostLazy (ArraysRpair r1 r2) (f1, f2) = do
  a1 <- copyToHostLazy r1 f1
  a2 <- copyToHostLazy r2 f2
  return (a1, a2)
copyToHostLazy (ArraysRarray) future@(Future ref) = do
  ptx <- gets llvmTarget
  liftIO $ do
    ivar <- readIORef ref
    (Array sh adata) <- case ivar of
      Full a        -> return a
      Pending _ _ a -> return a
      Empty         -> $internalError "copyToHostLazy" "blocked on an IVar"
    -- Note: [Lazy device-host transfers]
    --
    -- This needs must be non-strict at the leaves of the datatype (that
    -- is, the UniqueArray pointers). This means we can traverse the
    -- ArrayData constructors (in particular, the spine defined by Unit
    -- and Pair) until we reach the array we care about, without forcing
    -- the other fields.
    --
    -- https://github.com/AccelerateHS/accelerate/issues/437
    --
    -- Furthermore, we only want to transfer the data if the host pointer
    -- is currently unevaluated. This situation can occur for example if
    -- the argument to 'use' or 'unit' is returned as part of the result
    -- of a 'run'. Peek at GHC's underlying closure representation and
    -- check whether the pointer is a thunk, and only initiate the
    -- transfer if so.
    --
    -- XXX: The current approach of checking the heap representation
    -- (function 'evaluated' below) is not particularly reliable. We
    -- should improve this situation somehow.
    --    -- TLM 2019-06-06
    --

    let
      peekR :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
            => ArrayData e
            -> UniqueArray a
            -> Int
            -> IO (UniqueArray a)
      peekR ad (UniqueArray uid (Lifetime lft weak fp)) n = unsafeInterleaveIO $ do
        ok  <- evaluated fp
        fp' <- if ok
                  then return fp
                  else unsafeInterleaveIO . evalPTX ptx . evalPar $ do
                        !_ <- get future -- stream synchronisation
                        !_ <- block =<< Prim.peekArrayAsync n ad
                        return fp
        --
        return $ UniqueArray uid (Lifetime lft weak fp')

      -- Check that the argument is evaluated to normal form. In particular
      -- we are only concerned with the array payload (ForeignPtr).
      --
      evaluated :: a -> IO Bool
#if __GLASGOW_HASKELL__ >= 806
      evaluated x = do
        c <- getClosureData x
        case c of
          ThunkClosure{}        -> return False
          ArrWordsClosure{}     -> return True                      -- ByteArray#
          ConstrClosure{..}     -> and <$> mapM evaluated ptrArgs   -- a data constructor
          BlackholeClosure{..}  -> evaluated indirectee             -- evaluated by another thread (check if this is just an indirection?)
          MutVarClosure{..}     -> evaluated var                    -- in case this is not a PlainForeignPtr
          _                     -> do
            Debug.when Debug.verbose $
              Debug.traceIO Debug.dump_gc $
                printf "copyToHostLazy encountered: %s" (show c)
            return False
#else
#if UNBOXED_TUPLES
      evaluated x =
        case unpackClosure# x of
          (# iptr, ptrs, _ #) -> do
              let iptr0 = Ptr iptr
              let iptr1
                    | ghciTablesNextToCode = iptr0
                    | otherwise            = iptr0 `plusPtr` negate wORD_SIZE
              itbl <- peekItbl iptr1
              case tipe itbl of
                ARR_WORDS                                     -> return True
                i | i >= THUNK  && i < THUNK_SELECTOR         -> return False
                i | i >= CONSTR && i <= CONSTR_NOCAF          -> and <$> mapM evaluated (dumpArray# ptrs)
                i | i == MUT_VAR_CLEAN || i == MUT_VAR_DIRTY  -> evaluated (headArray# ptrs)
                BLACKHOLE                                     -> evaluated (headArray# ptrs)
                _                                             -> return False

      wORD_SIZE = sizeOf (undefined::Word)

      -- using indexArray# makes sure that the 'Any' is looked up without
      -- evaluating the value itself. This is not possible with Data.Array.!
      --
      dumpArray# :: Array# Any -> [Any]
      dumpArray# arr# = go 0#
        where
          l#    = sizeofArray# arr#
          go i# = case i# <# l# of
                    0# -> []
                    _  -> case indexArray# arr# i# of
                            (# h #) -> h : go (i# +# 1#)

      headArray# :: Array# Any -> Any
      headArray# arr# =
        case indexArray# arr# 0# of
          (# h #) -> h
#else
      evaluated _ =
        return False
#endif
#endif

      runR :: ArrayEltR e -> ArrayData e -> Int -> IO (ArrayData e)
      runR ArrayEltRunit               AD_Unit             !_ = return AD_Unit
      runR (ArrayEltRpair !aeR2 !aeR1) (AD_Pair !ad2 !ad1) !n = AD_Pair   <$> runR aeR2 ad2 n <*> runR aeR1 ad1 n
      runR (ArrayEltRvec  !aeR)        (AD_Vec w# !ad)     !n = AD_Vec w# <$> runR aeR ad (I# w# * n)
      --
      runR ArrayEltRint    ad@(AD_Int    ua) !n = AD_Int    <$> peekR ad ua n
      runR ArrayEltRint8   ad@(AD_Int8   ua) !n = AD_Int8   <$> peekR ad ua n
      runR ArrayEltRint16  ad@(AD_Int16  ua) !n = AD_Int16  <$> peekR ad ua n
      runR ArrayEltRint32  ad@(AD_Int32  ua) !n = AD_Int32  <$> peekR ad ua n
      runR ArrayEltRint64  ad@(AD_Int64  ua) !n = AD_Int64  <$> peekR ad ua n
      runR ArrayEltRword   ad@(AD_Word   ua) !n = AD_Word   <$> peekR ad ua n
      runR ArrayEltRword8  ad@(AD_Word8  ua) !n = AD_Word8  <$> peekR ad ua n
      runR ArrayEltRword16 ad@(AD_Word16 ua) !n = AD_Word16 <$> peekR ad ua n
      runR ArrayEltRword32 ad@(AD_Word32 ua) !n = AD_Word32 <$> peekR ad ua n
      runR ArrayEltRword64 ad@(AD_Word64 ua) !n = AD_Word64 <$> peekR ad ua n
      runR ArrayEltRhalf   ad@(AD_Half   ua) !n = AD_Half   <$> peekR ad ua n
      runR ArrayEltRfloat  ad@(AD_Float  ua) !n = AD_Float  <$> peekR ad ua n
      runR ArrayEltRdouble ad@(AD_Double ua) !n = AD_Double <$> peekR ad ua n
      runR ArrayEltRbool   ad@(AD_Bool   ua) !n = AD_Bool   <$> peekR ad ua n
      runR ArrayEltRchar   ad@(AD_Char   ua) !n = AD_Char   <$> peekR ad ua n
    
    Array sh <$> runR arrayElt adata (R.size sh)

-- | Clone an array into a newly allocated array on the device.
--
cloneArrayAsync
    :: forall sh e. (Shape sh, Elt e)
    => Array sh e
    -> Par PTX (Future (Array sh e))
cloneArrayAsync arr@(Array _ src) = do
  Array _ dst <- allocateRemote sh  :: Par PTX (Array sh e)
  Array (fromElt sh) `liftF` copyR arrayElt src dst (S.size sh)
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

