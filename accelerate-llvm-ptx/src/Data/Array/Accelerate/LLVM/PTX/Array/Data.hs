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

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.State
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim          as Prim

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State                                          ( gets )
import Data.IORef
import System.IO.Unsafe
import Prelude

#if __GLASGOW_HASKELL__ >= 806
import Text.Printf
import GHC.Exts.Heap                                                hiding ( size )
import qualified Data.Array.Accelerate.Debug                        as Debug
#elif UNBOXED_TUPLES
import Util                                                         ( ghciTablesNextToCode )
import GHC.Exts
import GHCi.InfoTable
import Foreign.Ptr                                                  ( plusPtr )
import Foreign.Storable                                             ( sizeOf )
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
  allocateRemote repr@(ArrayR shr tp) !sh = do
    let !n = size shr sh
    arr    <- liftIO $ allocateArray repr sh  -- shadow array on the host
    liftPar $ runArray tp arr (\m t ad -> Prim.mallocArray t (n*m) ad >> return ad)

  indexRemoteAsync  = runIndexArrayAsync Prim.indexArrayAsync
  useRemoteR        = Prim.useArrayAsync
  copyToHostR       = Prim.peekArrayAsync
  copyToRemoteR     = Prim.pokeArrayAsync
  copyToPeerR       = internalError "not supported yet"


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
    :: HasCallStack
    => ArraysR arrs
    -> FutureArraysR PTX arrs
    -> Par PTX arrs
copyToHostLazy TupRunit () = return ()
copyToHostLazy (TupRpair r1 r2) (f1, f2) = do
  a1 <- copyToHostLazy r1 f1
  a2 <- copyToHostLazy r2 f2
  return (a1, a2)
copyToHostLazy (TupRsingle (ArrayR shr tp)) future@(Future ref) = do
  ptx <- gets llvmTarget
  liftIO $ do
    ivar <- readIORef ref
    (Array sh adata) <- case ivar of
      Full a        -> return a
      Pending _ _ a -> return a
      Empty         -> internalError "blocked on an IVar"

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
      peekR :: SingleType e
            -> ArrayData e
            -> Int
            -> IO (ArrayData e)
      peekR t ad n
        | SingleArrayDict                        <- singleArrayDict t
        , UniqueArray uid (Lifetime lft weak fp) <- ad
        = unsafeInterleaveIO $ do
          ok  <- evaluated fp
          fp' <- if ok
                    then return fp
                    else unsafeInterleaveIO . evalPTX ptx . evalPar $ do
                          !_ <- get future -- stream synchronisation
                          !_ <- block =<< Prim.peekArrayAsync t n ad
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

      runR :: TypeR e -> ArrayData e -> Int -> IO (ArrayData e)
      runR TupRunit           !()          !_ = return ()
      runR (TupRpair !t1 !t2) (!ad1, !ad2) !n = (,) <$> runR t1 ad1 n <*> runR t2 ad2 n
      runR (TupRsingle !t)    !ad          !n = case t of
        SingleScalarType s                     -> peekR s ad n
        VectorScalarType (VectorType w s)
          | SingleArrayDict <- singleArrayDict s -> peekR s ad (n * w)

    Array sh <$> runR tp adata (size shr sh)

-- | Clone an array into a newly allocated array on the device.
--
cloneArrayAsync
    :: ArrayR (Array sh e)
    -> Array sh e
    -> Par PTX (Future (Array sh e))
cloneArrayAsync repr@(ArrayR shr tp) arr@(Array _ src) = do
  Array _ dst <- allocateRemote repr sh
  Array sh `liftF` copyR tp src dst (size shr sh)
  where
    sh  = shape arr

    copyR :: TypeR s -> ArrayData s -> ArrayData s -> Int -> Par PTX (Future (ArrayData s))
    copyR TupRunit           !_          !_            !_ = newFull ()
    copyR (TupRpair !t1 !t2) !(ad1, ad2) !(ad1', ad2') !n = liftF2 (,) (copyR t1 ad1 ad1' n)
                                                                       (copyR t2 ad2 ad2' n)
    copyR (TupRsingle !t)    !ad         !ad'          !n = case t of
      SingleScalarType s                     -> copyPrim s ad ad' n
      VectorScalarType (VectorType w s)
        | SingleArrayDict <- singleArrayDict s -> copyPrim s ad ad' (n * w)

    copyPrim
        :: SingleType s
        -> ArrayData s
        -> ArrayData s
        -> Int
        -> Par PTX (Future (ArrayData s))
    copyPrim !s !a1 !a2 !m = Prim.copyArrayAsync s m a1 a2

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

