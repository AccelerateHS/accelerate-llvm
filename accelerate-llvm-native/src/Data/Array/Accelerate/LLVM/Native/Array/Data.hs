{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Array.Data
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Native.Execute.Async ()

-- standard library
import Control.Monad.Trans
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.Storable


-- | Data instance for arrays in the native backend. We assume a shared-memory
-- machine, and just manipulate the underlying Haskell array directly.
--
instance Remote Native


-- | Copy an array into a newly allocated array. This uses 'memcpy'.
--
cloneArray :: (Shape sh, Elt e) => Array sh e -> LLVM Native (Array sh e)
cloneArray arr@(Array _ src) = liftIO $ do
  out@(Array _ dst)    <- allocateArray sh
  copyR arrayElt src dst
  return out
  where
    sh                  = shape arr
    n                   = size sh

    copyR :: ArrayEltR e -> ArrayData e -> ArrayData e -> IO ()
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

    copyPrim :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a) => ArrayData e -> ArrayData e -> IO ()
    copyPrim a1 a2 = do
      let p1 = ptrsOfArrayData a1
          p2 = ptrsOfArrayData a2
      memcpy (castPtr p2) (castPtr p1) (n * sizeOf (undefined :: a))


-- Standard C functions
-- --------------------

memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy p q s = c_memcpy p q (fromIntegral s) >> return ()

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

