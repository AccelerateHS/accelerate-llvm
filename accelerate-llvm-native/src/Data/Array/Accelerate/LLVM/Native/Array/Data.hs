{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Array.Data
-- Copyright   : [2014..2020] The Accelerate Team
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

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.Native.Execute.Async              ()  -- Async Native
import Data.Array.Accelerate.LLVM.Native.Target

import Control.Monad.Trans
import Foreign.Ptr


-- | Data instance for arrays in the native backend. We assume a shared-memory
-- machine, and just manipulate the underlying Haskell array directly.
--
instance Remote Native where
  {-# INLINE allocateRemote #-}
  allocateRemote repr = liftIO . allocateArray repr


-- | Copy an array into a newly allocated array. This uses 'memcpy'.
--
cloneArray :: ArrayR (Array sh e) -> Array sh e -> LLVM Native (Array sh e)
cloneArray repr (Array sh src) = liftIO $ do
  out@(Array _ dst) <- allocateArray repr sh
  copyR (arrayRtype repr) src dst
  return out
  where
    n = size (arrayRshape repr) sh

    copyR :: TypeR e -> ArrayData e -> ArrayData e -> IO ()
    copyR TupRunit          !_          !_          = return ()
    copyR (TupRsingle t)    !ad1        !ad2        = copyPrim t ad1 ad2
    copyR (TupRpair !t !t') (ad1, ad1') (ad2, ad2') = do
      copyR t  ad1  ad2
      copyR t' ad1' ad2'

    copyPrim :: ScalarType e -> ArrayData e -> ArrayData e -> IO ()
    copyPrim !tp !a1 !a2
      | ScalarArrayDict{} <- scalarArrayDict tp = do
      let p1 = unsafeUniqueArrayPtr a1
          p2 = unsafeUniqueArrayPtr a2
      memcpy (castPtr p2) (castPtr p1) (n * bytesElt (TupRsingle tp))


-- Standard C functions
-- --------------------

memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy p q s = c_memcpy p q (fromIntegral s) >> return ()

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

