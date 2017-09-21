{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.IORef.Storable
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A mutable reference in the 'IO' monad.
--

module Data.IORef.Storable (

  IORef,

  newIORef,
  readIORef,
  writeIORef,
  modifyIORef,

) where

import Foreign.ForeignPtr
import Foreign.Storable
import GHC.ForeignPtr


-- | A pointer to a mutable object. Unlike 'Data.IORef.IORef', this is just
-- a pointer to the value, rather than a reference containing a boxed object.
-- This means that the value is stored strictly, and is only one pointer away.
--
data IORef a = IORef {-# UNPACK #-} !(ForeignPtr a)


-- | Build a new 'IORef'
--
newIORef :: Storable a => a -> IO (IORef a)
newIORef v = do
  fp <- mallocPlainForeignPtr
  withForeignPtr fp $ \p -> poke p v
  return (IORef fp)

-- | Read the value of the 'IORef'
--
readIORef :: Storable a => IORef a -> IO a
readIORef (IORef fp) = withForeignPtr fp peek

-- | Write a new value into the 'IORef'
--
writeIORef :: Storable a => IORef a -> a -> IO ()
writeIORef (IORef fp) v = withForeignPtr fp $ \p -> poke p v

-- | Mutate the contents of an 'IORef'
--
modifyIORef :: Storable a => IORef a -> (a -> a) -> IO ()
modifyIORef ref f = readIORef ref >>= writeIORef ref . f

