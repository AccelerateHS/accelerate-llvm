{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
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

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State


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

data Future a = Future {-# UNPACK #-} !(MVar (IVar a))

data IVar a
    = Full !a
    | Pending {-# UNPACK #-} !Event !(Maybe (Lifetime FunctionTable)) ![Future ()] !a
    | Empty ![Future ()]


instance Async PTX where
  type FutureR PTX = Future

  newtype Par PTX a = Par { runPar :: ReaderT ParState (LLVM PTX) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadReader ParState, MonadState PTX, MonadThrow, MonadCatch, MonadMask )

  {-# INLINEABLE new     #-}
  {-# INLINEABLE newFull #-}
  new       = Future <$> liftIO (newMVar (Empty []))
  newFull v = Future <$> liftIO (newMVar (Full v))

  {-# INLINEABLE spawn #-}
  spawn m = do
    s' <- liftPar Stream.create
    r  <- local (const (s', Nothing)) m
    liftIO (Stream.destroy s')
    return r

  {-# INLINEABLE fork #-}
  fork m = do
    s' <- liftPar Stream.create
    () <- local (const (s', Nothing)) m
    liftIO (Stream.destroy s')

  -- When we call 'put' the actual work may not have been evaluated yet; get
  -- a new event in the current execution stream and once that is filled we can
  -- transition the IVar to Full.
  --
  {-# INLINEABLE put #-}
  put (Future ref) v = do
    stream <- asks ptxStream
    kernel <- asks ptxKernel
    event  <- liftPar (Event.waypoint stream)
    liftIO $ do
      ready <- Event.query event
      ivar  <- readMVar ref
      case ivar of
        Empty statusHandles ->
          if ready then do
            modifyMVar_ ref $ const $ pure $ Full v
            signalCompletion statusHandles
          else
            modifyMVar_ ref $ const $ pure $ Pending event kernel statusHandles v
        _ -> internalError "multiple put"

  -- Get the value of Future. Since the actual cross-stream synchronisation
  -- happens on the device, we should never have to block/reschedule the main
  -- thread waiting on a value; if we get an empty IVar at this point, something
  -- has gone wrong.
  --
  {-# INLINEABLE get #-}
  get fut@(Future ref) = do
    stream <- asks ptxStream
    liftIO  $ do
      ivar <- readMVar ref
      case ivar of
        Full v            -> return v
        Pending event _ _ v -> do
          ready <- Event.query event
          if ready then
            completePending fut
          else do
            Event.after event stream
            return v
        Empty _         -> internalError "blocked on an IVar"

  {-# INLINEABLE block #-}
  block = liftIO . wait

  {-# INLINE liftPar #-}
  liftPar = Par . lift

  {-# INLINE statusHandle #-}

  statusHandle (Future ref) =
    liftIO $ modifyMVar ref $ \case
      Full v                      -> (Full v,) . Future <$> newMVar (Full ())
      Empty statusHandles         -> do
        emptyFut <- Future <$> newMVar (Empty [])
        pure (Empty (emptyFut:statusHandles), emptyFut)
      Pending e k statusHandles v -> do
        pendingFut <- Future <$> newMVar (Pending e k [] ())
        pure (Pending e k (pendingFut:statusHandles) v, pendingFut)

  {-# INLINE poll #-}

  poll fut@(Future ref) = liftIO $ do
    ivar <- readMVar ref
    case ivar of
      Full v ->
        return (Just v)
      Pending event _ _ _ -> do
        ready <- Event.query event
        if ready then
          Just <$> completePending fut
        else
          pure Nothing
      _ ->
        return Nothing

-- | Block the calling _host_ thread until the value offered by the future is
-- available.
--
{-# INLINEABLE wait #-}
wait :: Future a -> IO a
wait fut@(Future ref) = do
  ivar <- readMVar ref
  case ivar of
    Full v ->
      return v
    Pending event _ _ _-> do
      Event.block event
      completePending fut
    Empty _ ->
      internalError "blocked on an IVar"

signalCompletion :: [Future ()] -> IO ()
signalCompletion = mapM_ $ \(Future ref) -> modifyMVar_ ref $ const $ pure $ Full ()

completePending :: Future a -> IO a
completePending (Future ref) =
  modifyMVar ref $ \case
    Pending _ k statusHandles v -> do
      signalCompletion statusHandles
      maybe (pure ()) touchLifetime k
      pure (Full v, v)
    _ ->
      internalError "Expected (Pending ...)"