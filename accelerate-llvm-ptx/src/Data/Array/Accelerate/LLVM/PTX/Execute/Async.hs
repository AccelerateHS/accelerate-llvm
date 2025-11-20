{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Async
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Async (

  module Data.Array.Accelerate.LLVM.Execute.Async,
  module Data.Array.Accelerate.LLVM.PTX.Execute.Async,

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Event                 ( Event )
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream                ( Stream )
import Data.Array.Accelerate.LLVM.PTX.Link.Object                   ( FunctionTable )
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Event       as Event
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Stream      as Stream

import Control.Monad.Reader
import Data.IORef


-- | Evaluate a parallel computation
--
{-# INLINE evalPar #-}
evalPar :: Par PTX a -> LLVM PTX a
evalPar p = do
  s <- Stream.create
  r <- runReaderT (runPar p) (s, Nothing)
  return r


type ParState = (Stream, Maybe (Lifetime FunctionTable))

ptxStream :: ParState -> Stream
ptxStream = fst

ptxKernel :: ParState -> Maybe (Lifetime FunctionTable)
ptxKernel = snd


-- Implementation
-- --------------

data Future a = Future {-# UNPACK #-} !(IORef (IVar a))

data IVar a
    = Full !a
    | Pending {-# UNPACK #-} !Event !(IO ()) !a
    | Empty !(IO ())


askParState :: Par PTX ParState
askParState = Par ask

asksParState :: (ParState -> a) -> Par PTX a
asksParState f = Par (asks f)

localParState :: (ParState -> ParState) -> Par PTX a -> Par PTX a
localParState f (Par m) = Par (local f m)

instance MonadReader PTX (Par PTX) where
  ask = Par (lift ask)
  local f (Par (ReaderT g)) = Par (ReaderT (\parstate -> local f (g parstate)))

instance Async PTX where
  type FutureR PTX = Future

  newtype Par PTX a = Par { runPar :: ReaderT ParState (LLVM PTX) a }
    deriving ( Functor, Applicative, Monad, MonadIO )

  {-# INLINEABLE new     #-}
  {-# INLINEABLE newFull #-}
  new       = Future <$> liftIO (newIORef (Empty (return ())))
  newFull v = Future <$> liftIO (newIORef (Full v))

  {-# INLINEABLE spawn #-}
  spawn m = do
    s' <- liftPar Stream.create
    r  <- localParState (const (s', Nothing)) m
    liftIO (Stream.destroy s')
    return r

  {-# INLINEABLE fork #-}
  fork m = do
    s' <- liftPar (Stream.create)
    () <- localParState (const (s', Nothing)) m
    liftIO (Stream.destroy s')

  -- When we call 'put' the actual work may not have been evaluated yet; get
  -- a new event in the current execution stream and once that is filled we can
  -- transition the IVar to Full.
  --
  {-# INLINEABLE put #-}
  put (Future ref) v = do
    stream <- asksParState ptxStream
    kernel <- asksParState ptxKernel
    event  <- liftPar (Event.waypoint stream)
    ready  <- liftIO  (Event.query event)
    let cleanupK = case kernel of
                     Just k -> touchLifetime k
                     Nothing -> return ()
    liftIO . atomicModifyIORef' ref $ \case
      Empty cleanup -> if ready then (Full v, ())
                                else (Pending event (cleanup >> cleanupK) v, ())
      _     -> internalError "multiple put"

  -- Get the value of Future. Since the actual cross-stream synchronisation
  -- happens on the device, we should never have to block/reschedule the main
  -- thread waiting on a value; if we get an empty IVar at this point, something
  -- has gone wrong.
  --
  {-# INLINEABLE get #-}
  get (Future ref) = do
    stream <- asksParState ptxStream
    liftIO  $ do
      ivar <- readIORef ref
      case ivar of
        Full v            -> return v
        Pending event cleanup v -> do
          ready <- Event.query event
          if ready
            then do
              writeIORef ref (Full v)
              cleanup
            else
              Event.after event stream
          return v
        Empty _         -> internalError "blocked on an IVar"

  {-# INLINEABLE block #-}
  block = liftIO . wait

  {-# INLINE liftPar #-}
  liftPar = Par . lift


-- | Block the calling _host_ thread until the value offered by the future is
-- available.
--
{-# INLINEABLE wait #-}
wait :: Future a -> IO a
wait (Future ref) = do
  ivar <- readIORef ref
  case ivar of
    Full v            -> return v
    Pending event cleanup v -> do
      Event.block event
      writeIORef ref (Full v)
      cleanup
      return v
    Empty _         -> internalError "blocked on an IVar"

{-# INLINEABLE putCleanup #-}
putCleanup :: HasCallStack => FutureR PTX a -> IO () -> a -> Par PTX ()
putCleanup (Future ref) cleanup v = do
  stream <- asksParState ptxStream
  kernel <- asksParState ptxKernel
  event  <- liftPar (Event.waypoint stream)
  ready  <- liftIO  (Event.query event)
  let cleanupK = case kernel of
                   Just k -> touchLifetime k
                   Nothing -> return ()
  liftIO . atomicModifyIORef' ref $ \case
    Empty cleanup2 -> if ready then (Full v, ())
                               else (Pending event (cleanup2 >> cleanup >> cleanupK) v, ())
    _     -> internalError "multiple put"

{-# INLINEABLE addCleanup #-}
addCleanup :: HasCallStack => FutureR PTX a -> IO () -> Par PTX ()
addCleanup (Future ref) cleanup = liftIO $ do
  toRunNow <- atomicModifyIORef' ref $ \case
    Full v -> (Full v, cleanup)
    Pending event cleanup2 v -> (Pending event (cleanup2 >> cleanup) v, return ())
    Empty cleanup2 -> (Empty (cleanup2 >> cleanup), return ())
  toRunNow

