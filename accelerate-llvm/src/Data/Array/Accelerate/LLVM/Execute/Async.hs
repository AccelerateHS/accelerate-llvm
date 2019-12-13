{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Async
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Async
  where

import Data.Array.Accelerate.Array.Sugar 

class Monad (Par arch) => Async arch where

  -- | The monad parallel computations will be executed in. Presumably a stack
  -- with the LLVM monad at the base.
  --
  data Par arch :: * -> *

  -- | Parallel computations can communicate via futures.
  --
  type FutureR arch :: * -> *

  -- | Create a new (empty) promise, to be fulfilled at some future point.
  --
  new :: Par arch (FutureR arch a)

  -- | The future is here. Multiple 'put's to the same future are not allowed
  -- and (presumably) result in a runtime error.
  --
  put :: FutureR arch a -> a -> Par arch ()

  -- | Read the value stored in a future, once it is available. It is _not_
  -- required that this is a blocking operation on the host, only that it is
  -- blocking with respect to computations on the remote device.
  --
  get :: FutureR arch a -> Par arch a

  -- | Fork a computation to happen in parallel. The forked computation may
  -- exchange values with other computations using Futures.
  --
  fork :: Par arch () -> Par arch ()

  -- | Read a value stored in a future, once it is available. This is blocking
  -- with respect to both the host and remote device.
  --
  {-# INLINEABLE block #-}
  block :: FutureR arch a -> Par arch a
  block = get

  -- | Evaluate a computation in a new thread/context. This might be implemented
  -- more efficiently than the default implementation.
  --
  {-# INLINEABLE spawn #-}
  spawn :: Par arch a -> Par arch a
  spawn m = do
    r <- new
    fork $ put r =<< m
    get r

  -- | Create a new "future" where the value is available immediately. This
  -- might be implemented more efficiently than the default implementation.
  --
  {-# INLINEABLE newFull #-}
  newFull :: a -> Par arch (FutureR arch a)
  newFull a = do
    r <- new
    put r a
    return r

data FutureArraysRepr arch a repr where
  FutureArraysReprNil       :: FutureArraysRepr arch ()            ()
  FutureArraysReprArray     :: FutureArraysRepr arch (Array sh e) (FutureR arch (Array sh e))
  FutureArraysReprPair      :: FutureArraysRepr arch a             a'
                            -> FutureArraysRepr arch b             b'
                            -> FutureArraysRepr arch (a, b)        (a', b')

class FutureArrays a where
  type FutureArraysR arch a
  futureArraysRepr :: FutureArraysRepr arch a (FutureArraysR arch a)

instance FutureArrays () where
  type FutureArraysR arch () = ()
  futureArraysRepr = FutureArraysReprNil

instance FutureArrays (Array sh e) where
  type FutureArraysR arch (Array sh e) = FutureR arch (Array sh e)
  futureArraysRepr = FutureArraysReprArray

instance (FutureArrays a, FutureArrays b) => FutureArrays (a, b) where
  type FutureArraysR arch (a, b) = (FutureArraysR arch a, FutureArraysR arch b)
  futureArraysRepr = FutureArraysReprPair (futureArraysRepr @a) (futureArraysRepr @b)

getArrays :: Async arch => ArraysR a -> FutureArraysR arch a -> Par arch a
getArrays ArraysRarray        a        = get a
getArrays ArraysRunit         _        = return ()
getArrays (ArraysRpair r1 r2) (a1, a2) = (,) <$> getArrays r1 a1 <*> getArrays r2 a2

blockArrays :: Async arch => ArraysR a -> FutureArraysR arch a -> Par arch a
blockArrays ArraysRarray        a        = block a
blockArrays ArraysRunit         _        = return ()
blockArrays (ArraysRpair r1 r2) (a1, a2) = (,) <$> blockArrays r1 a1 <*> blockArrays r2 a2

-- | Create new (empty) promises for a structure of arrays, to be fulfilled
-- at some future point. Note that the promises in the structure may all be
-- fullfilled at different moments.
--
newArrays :: Async arch => ArraysR a -> Par arch (FutureArraysR arch a)
newArrays ArraysRunit               = return ()
newArrays ArraysRarray              = new
newArrays (ArraysRpair repr1 repr2) = (,) <$> newArrays repr1 <*> newArrays repr2
