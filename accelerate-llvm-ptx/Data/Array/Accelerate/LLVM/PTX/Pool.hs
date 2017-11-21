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

) where

import Data.Array.Accelerate.Error

import Data.Maybe
import Control.Concurrent.MVar
import Control.Exception
import Prelude                                                      hiding ( take )


-- | An item pool
--
data Pool a = Pool {-# UNPACK #-} !(MVar [a])


-- | Create a new pooled resource containing the given items
--
create :: [a] -> IO (Pool a)
create it = Pool <$> newMVar it


-- | Execute an operation using an item from the pool. Like 'take', the function
-- blocks until one becomes available.
--
with :: Pool a -> (a -> IO b) -> IO b
with pool action =
  bracket (take pool) (put pool) action

-- | Take an item from the pool. This will block until one is available.
--
take :: Pool a -> IO a
take (Pool ref) = do
  ds <- takeMVar ref
  case ds of
    []       -> $internalError "take" "assumption violated"
    [d]      -> return d  -- leave the pool empty
    (d:rest) -> do
                   -- XXX: another thread may have returned a context to
                   -- the pool since we emptied it above.
                   mask_ $ do
                     it <- tryTakeMVar ref
                     case it of
                       Nothing   -> putMVar ref rest
                       Just this -> putMVar ref (this ++ rest)
                   return d

-- | Return an item back to the pool for later reuse. This should be
-- a non-blocking operation.
--
put :: Pool a -> a -> IO ()
put (Pool ref) d =
  mask_ $ do
    it <- tryTakeMVar ref
    case it of
      Just ds -> putMVar ref (d:ds)
      Nothing -> putMVar ref [d]

