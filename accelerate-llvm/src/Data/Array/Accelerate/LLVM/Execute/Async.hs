{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Async
-- Copyright   : [2014..2018] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Async
  where


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
  spawn :: Par arch (FutureR arch a) -> Par arch (FutureR arch a)
  spawn m = do
    r <- new              -- Future (Future a)  |:
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

