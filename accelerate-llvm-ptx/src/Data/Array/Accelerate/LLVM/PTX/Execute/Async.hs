{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Async
-- Copyright   : [2014..2018] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Async (

  module Data.Array.Accelerate.LLVM.Execute.Async,
  module Data.Array.Accelerate.LLVM.PTX.Execute.Async,

) where

-- accelerate
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Event                 ( Event )
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream                ( Stream )
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Event       as Event
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Stream      as Stream

-- standard library
import Control.Monad.State
import Control.Monad.Reader
import Data.IORef


-- | Evaluate a parallel computation
--
{-# INLINE evalPar #-}
evalPar :: Par PTX a -> LLVM PTX a
evalPar p = do
  s <- Stream.create
  r <- runReaderT (runPar p) s
  return r


-- Implementation
-- --------------

data Future a = Future {-# UNPACK #-} !(IORef (IVar a))

data IVar a
    = Full !a
    | Pending {-# UNPACK #-} !Event !a
    | Empty


instance Async PTX where
  type FutureR PTX = Future

  newtype Par PTX a = Par { runPar :: ReaderT Stream (LLVM PTX) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadReader Stream, MonadState PTX )

  {-# INLINEABLE new     #-}
  {-# INLINEABLE newFull #-}
  new       = Future <$> liftIO (newIORef Empty)
  newFull v = Future <$> liftIO (newIORef (Full v))

  {-# INLINEABLE spawn #-}
  spawn m = do
    s' <- liftPar Stream.create
    r  <- local (const s') m
    liftIO (Stream.destroy s')
    return r

  {-# INLINEABLE fork #-}
  fork m = do
    s' <- liftPar (Stream.create)
    () <- local (const s') m
    liftIO (Stream.destroy s')

  -- When we call 'put' the actual work may not have been evaluated yet; get
  -- a new event in the current execution stream and once that is filled we can
  -- transition the IVar to Full.
  --
  {-# INLINEABLE put #-}
  put (Future ref) v = do
    stream <- ask
    event  <- liftPar (Event.waypoint stream)
    ready  <- liftIO  (Event.query event)
    liftIO . modifyIORef' ref $ \case
      Empty -> if ready then Full v
                        else Pending event v
      _     -> $internalError "put" "multiple put"

  -- Get the value of Future. Since the actual cross-stream synchronisation
  -- happens on the device, we should never have to block/reschedule the main
  -- thread waiting on a value; if we get an empty IVar at this point, something
  -- has gone wrong.
  --
  {-# INLINEABLE get #-}
  get (Future ref) = do
    stream <- ask
    liftIO  $ do
      ivar <- readIORef ref
      case ivar of
        Full v          -> return v
        Pending event v -> do
          ready <- Event.query event
          if ready
            then writeIORef ref (Full v)
            else Event.after event stream
          return v
        Empty           -> $internalError "get" "blocked on an IVar"

  {-# INLINEABLE block #-}
  block = liftIO . wait


-- | Lift an operation from the base LLVM monad into the Par monad
--
{-# INLINE liftPar #-}
liftPar :: LLVM PTX a -> Par PTX a
liftPar = Par . lift

-- | Block the calling _host_ thread until the value offered by the future is
-- available.
--
{-# INLINEABLE wait #-}
wait :: Future a -> IO a
wait (Future ref) = do
  ivar <- readIORef ref
  case ivar of
    Full v          -> return v
    Pending event v -> do
      Event.block event
      writeIORef ref (Full v)
      return v
    Empty           -> $internalError "wait" "blocked on an IVar"

