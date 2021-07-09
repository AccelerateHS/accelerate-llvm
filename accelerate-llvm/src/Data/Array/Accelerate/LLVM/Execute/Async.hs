{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Async
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Async
  where

import Data.Array.Accelerate.LLVM.State                             ( LLVM )
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Type

import Control.Monad.Trans                                          ( MonadIO )
import Data.Kind
import GHC.Stack


class (Monad (Par arch), MonadIO (Par arch)) => Async arch where

  -- | The monad parallel computations will be executed in. Presumably a stack
  -- with the LLVM monad at the base.
  --
  data Par arch :: Type -> Type

  -- | Parallel computations can communicate via futures.
  --
  type FutureR arch :: Type -> Type

  -- | Create a new (empty) promise, to be fulfilled at some future point.
  --
  new :: HasCallStack => Par arch (FutureR arch a)

  -- | The future is here. Multiple 'put's to the same future are not allowed
  -- and (presumably) result in a runtime error.
  --
  put :: HasCallStack => FutureR arch a -> a -> Par arch ()

  -- | Read the value stored in a future, once it is available. It is _not_
  -- required that this is a blocking operation on the host, only that it is
  -- blocking with respect to computations on the remote device.
  --
  get :: HasCallStack => FutureR arch a -> Par arch a

  -- | Fork a computation to happen in parallel. The forked computation may
  -- exchange values with other computations using Futures.
  --
  fork :: HasCallStack => Par arch () -> Par arch ()

  -- | Lift an operation from the base LLVM monad into the Par monad
  --
  liftPar :: HasCallStack => LLVM arch a -> Par arch a

  -- | Read a value stored in a future, once it is available. This is blocking
  -- with respect to both the host and remote device.
  --
  {-# INLINEABLE block #-}
  block :: HasCallStack => FutureR arch a -> Par arch a
  block = get

  -- | Evaluate a computation in a new thread/context. This might be implemented
  -- more efficiently than the default implementation.
  --
  {-# INLINEABLE spawn #-}
  spawn :: HasCallStack => Par arch a -> Par arch a
  spawn m = do
    r <- new
    fork $ put r =<< m
    get r

  -- | Create a new "future" where the value is available immediately. This
  -- might be implemented more efficiently than the default implementation.
  --
  {-# INLINEABLE newFull #-}
  newFull :: HasCallStack => a -> Par arch (FutureR arch a)
  newFull a = do
    r <- new
    put r a
    return r

type family FutureArraysR arch arrs where
  FutureArraysR arch ()           = ()
  FutureArraysR arch (a, b)       = (FutureArraysR arch a, FutureArraysR arch b)
  FutureArraysR arch (Array sh e) = FutureR arch (Array sh e)

getArrays :: Async arch => ArraysR a -> FutureArraysR arch a -> Par arch a
getArrays (TupRsingle ArrayR{}) a        = get a
getArrays TupRunit              _        = return ()
getArrays (TupRpair r1 r2)      (a1, a2) = (,) <$> getArrays r1 a1 <*> getArrays r2 a2

blockArrays :: Async arch => ArraysR a -> FutureArraysR arch a -> Par arch a
blockArrays (TupRsingle ArrayR{}) a        = block a
blockArrays TupRunit              _        = return ()
blockArrays (TupRpair r1 r2)      (a1, a2) = (,) <$> blockArrays r1 a1 <*> blockArrays r2 a2

-- | Create new (empty) promises for a structure of arrays, to be fulfilled
-- at some future point. Note that the promises in the structure may all be
-- fullfilled at different moments.
--
newArrays :: Async arch => ArraysR a -> Par arch (FutureArraysR arch a)
newArrays TupRunit               = return ()
newArrays (TupRsingle ArrayR{})  = new
newArrays (TupRpair repr1 repr2) = (,) <$> newArrays repr1 <*> newArrays repr2

