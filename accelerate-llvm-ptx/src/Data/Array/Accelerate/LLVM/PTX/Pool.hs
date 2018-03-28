{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Pool
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Pool (

  Pool,
  create, with, take, put,
  unsafeWith,

) where

import Control.Concurrent.MVar
import Control.Exception
import Data.Maybe
import System.IO.Unsafe
import Prelude                                                      hiding ( take )

#if __GLASGOW_HASKELL__ >= 800
import Data.List.NonEmpty                                           ( NonEmpty(..) )
#endif


-- | An item pool
--
data Pool a = Pool {-# UNPACK #-} !(MVar (NonEmpty a))


-- | Create a new pooled resource containing the given items
--
create :: [a] -> IO (Pool a)
create []     = Pool <$> newEmptyMVar
create (x:xs) = Pool <$> newMVar (x :| xs)


-- | Execute an operation using an item from the pool. Like 'take', the function
-- blocks until one becomes available.
--
with :: Pool a -> (a -> IO b) -> IO b
with pool action =
  bracket (take pool) (put pool) action

unsafeWith :: Pool a -> (a -> b) -> b
unsafeWith pool action =
  unsafePerformIO $ with pool (pure . action)


-- | Take an item from the pool. This will block until one is available.
--
take :: Pool a -> IO a
take (Pool ref) = do
  x :| xs <- takeMVar ref -- blocking
  case xs of
    []     -> return ()   -- leave the pool empty; subsequent 'take's will block
    (a:as) -> mask_ $ do r <- tryTakeMVar ref
                         case r of
                           Nothing        -> putMVar ref (a :| as)
                           Just (b :| bs) -> putMVar ref (a :| b : bs ++ as)
  return x


-- | Return an item back to the pool for later reuse. This should be
-- a non-blocking operation.
--
put :: Pool a -> a -> IO ()
put (Pool ref) a =
  mask_ $ do
    it <- tryTakeMVar ref
    case it of
      Just (b :| bs) -> putMVar ref (a :| b : bs)
      Nothing        -> putMVar ref (a :| [])


#if __GLASGOW_HASKELL__ < 800
-- | Non-empty (and non-strict) list type.
--
infixr 5 :|
data NonEmpty a = a :| [a]
  deriving ( Eq, Ord, Show, Read )
#endif

