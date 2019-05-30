{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Array.Data
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Array.Data (

  module Data.Array.Accelerate.LLVM.Array.Data,
  cloneArray,

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.Native.Execute.Async              ()  -- Async Native
import Data.Array.Accelerate.LLVM.Native.Target

-- standard library
import Control.Monad.Trans
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import GHC.Int                                                      ( Int(..) )


-- | Data instance for arrays in the native backend. We assume a shared-memory
-- machine, and just manipulate the underlying Haskell array directly.
--
instance Remote Native where
  {-# INLINE allocateRemote #-}
  allocateRemote = liftIO . allocateArray


-- | Copy an array into a newly allocated array. This uses 'memcpy'.
--
cloneArray :: (Shape sh, Elt e) => Array sh e -> LLVM Native (Array sh e)
cloneArray arr@(Array _ src) = liftIO $ do
  out@(Array _ dst) <- allocateArray sh
  copyR arrayElt src dst 1
  return out
  where
    sh  = shape arr
    n   = size sh

    copyR :: ArrayEltR e -> ArrayData e -> ArrayData e -> Int -> IO ()
    copyR ArrayEltRunit    !_   !_   !_ = return ()
    copyR ArrayEltRint     !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRint8    !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRint16   !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRint32   !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRint64   !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRword    !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRword8   !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRword16  !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRword32  !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRword64  !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRhalf    !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRfloat   !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRdouble  !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRbool    !ad1 !ad2 !i = copyPrim ad1 ad2 i
    copyR ArrayEltRchar    !ad1 !ad2 !i = copyPrim ad1 ad2 i
    --
    copyR (ArrayEltRpair !aeR1 !aeR2) !ad1 !ad2 !i = do
      copyR aeR1 (fstArrayData ad1) (fstArrayData ad2) i
      copyR aeR2 (sndArrayData ad1) (sndArrayData ad2) i
    --
    copyR (ArrayEltRvec !aeR) (AD_Vec w# !ad1) (AD_Vec _ !ad2) !i =
      copyR aeR ad1 ad2 (I# w# * i)

    copyPrim :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a) => ArrayData e -> ArrayData e -> Int -> IO ()
    copyPrim !a1 !a2 !i = do
      let p1 = ptrsOfArrayData a1
          p2 = ptrsOfArrayData a2
      memcpy (castPtr p2) (castPtr p1) (i * n * sizeOf (undefined :: a))


-- Standard C functions
-- --------------------

memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy p q s = c_memcpy p q (fromIntegral s) >> return ()

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

