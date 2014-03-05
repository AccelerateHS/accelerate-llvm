{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Array.Data
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Array.Data (

  Remote(..),
  runUseArray, runIndexArray,

  runArrayData1,

  module Data.Array.Accelerate.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation               ( size )
import Data.Array.Accelerate.Array.Sugar                        ( Array(..), Shape, toElt )

import Data.Array.Accelerate.LLVM.State

-- standard library
import Control.Applicative
import Control.Monad
import Data.Typeable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


class Remote arch where

  -- | Upload an existing array from the host to the remote device.
  --
  {-# INLINEABLE useArray #-}
  useArray      :: Shape sh
                => Array sh e
                -> LLVM arch ()
  useArray _ = return ()

  -- | Read a single element from the remote array at a given row-major index
  --
  {-# INLINEABLE indexArray #-}
  indexArray    :: Array sh e
                -> Int
                -> LLVM arch e
  indexArray (Array _ adata) i = return . toElt $ unsafeIndexArrayData adata i


-- CPP hackery to generate the cases where we dispatch to the worker function handling
-- elementary types.
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


-- |Upload an existing array from the host
--
{-# INLINEABLE runUseArray #-}
runUseArray
    :: (forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a) => ArrayData e -> Int -> IO ())
    -> Array sh e
    -> IO ()
runUseArray worker arr@(Array sh _) = runArrayData1 worker arr (size sh)


-- |Read a single element from an array at the given row-major index.
--
{-# INLINEABLE runIndexArray #-}
runIndexArray
    :: (forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a) => ArrayData e -> Int -> IO a)
    -> Array sh e
    -> Int
    -> IO e
runIndexArray worker (Array _ adata) i = toElt `liftM` indexR arrayElt adata
  where
    indexR :: ArrayEltR a -> ArrayData a -> IO a
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
    indexR ArrayEltRcshort           ad = CShort  <$> worker ad i
    indexR ArrayEltRcushort          ad = CUShort <$> worker ad i
    indexR ArrayEltRcint             ad = CInt    <$> worker ad i
    indexR ArrayEltRcuint            ad = CUInt   <$> worker ad i
    indexR ArrayEltRclong            ad = CLong   <$> worker ad i
    indexR ArrayEltRculong           ad = CULong  <$> worker ad i
    indexR ArrayEltRcllong           ad = CLLong  <$> worker ad i
    indexR ArrayEltRcullong          ad = CULLong <$> worker ad i
    indexR ArrayEltRcchar            ad = CChar   <$> worker ad i
    indexR ArrayEltRcschar           ad = CSChar  <$> worker ad i
    indexR ArrayEltRcuchar           ad = CUChar  <$> worker ad i
    indexR ArrayEltRcfloat           ad = CFloat  <$> worker ad i
    indexR ArrayEltRcdouble          ad = CDouble <$> worker ad i
    indexR ArrayEltRbool             ad = toBool  <$> worker ad i
      where
        toBool 0 = False
        toBool _ = True


-- | Generalised functions to traverse the array data struct

{-# INLINE runArrayData1 #-}
runArrayData1
    :: forall sh e a. (forall e' p. (ArrayElt e', ArrayPtrs e' ~ Ptr p, Storable p, Typeable p) => ArrayData e' -> a -> IO ())
    -> Array sh e
    -> a
    -> IO ()
runArrayData1 worker (Array _ adata) a = runR arrayElt adata
  where
    runR :: ArrayEltR e' -> ArrayData e' -> IO ()
    runR ArrayEltRunit             _  = return ()
    runR (ArrayEltRpair aeR1 aeR2) ad = runR aeR1 (fstArrayData ad) >>
                                        runR aeR2 (sndArrayData ad)
    runR aer                       ad = runW aer ad a
    --
    runW :: ArrayEltR e' -> ArrayData e' -> a -> IO ()
    mkPrimDispatch(runW, worker)

