{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Async
-- Copyright   : [2014..2018] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Async (

  Async(..), Future(..), IVar(..),
  evalPar,

) where

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.State

-- standard library
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Sequence                                                ( Seq )
import qualified Data.Sequence                                      as Seq


-- | Evaluate a parallel computation
--
evalPar :: Par Native a -> LLVM Native a
evalPar work = do
  queue   <- liftIO newEmptyMVar
  result  <- liftIO newEmptyMVar
  runReaderT (runContT (runPar work) (liftIO . putMVar result)) queue
  liftIO $ takeMVar result


-- Implementation
-- --------------

data Future a = Future {-# UNPACK #-} !(IORef (IVar a))

data IVar a
    = Full !a
    | Blocked (Seq (a -> IO ()))
    | Empty

type Schedule = MVar (Seq (Par Native ()))

instance Async Native where
  type FutureR Native  = Future
  newtype Par Native a = Par { runPar :: ContT () (ReaderT Schedule (LLVM Native)) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadCont, MonadReader Schedule, MonadState Native )

  {-# INLINEABLE new     #-}
  {-# INLINEABLE newFull #-}
  new       = Future <$> liftIO (newIORef Empty)
  newFull v = Future <$> liftIO (newIORef (Full v))

  {-# INLINEABLE get #-}
  get (Future ref) =
    callCC $ \k -> do
      queue  <- ask
      next   <- liftIO . atomicModifyIORef' ref $ \case
                  Empty      -> (Blocked (Seq.singleton (pushL queue . k)), reschedule)
                  Blocked ks -> (Blocked (ks Seq.|>      pushL queue . k),  reschedule)
                  Full a     -> (Full a,                                    return a)
      next

  {-# INLINEABLE put #-}
  put (Future ref) v =
    liftIO $ do
      ks <- atomicModifyIORef' ref $ \case
              Empty      -> (Full v, Seq.empty)
              Blocked ks -> (Full v, ks)
              _          -> $internalError "put" "multiple put"
      mapM_ ($ v) ks

  {-# INLINEABLE fork #-}
  fork child = do
    queue <- ask
    callCC $ \parent -> do
      liftIO $ pushL queue (parent ())
      child
      reschedule

  {-# INLINEABLE spawn #-}
  spawn = id

  {-# INLINEABLE liftPar #-}
  liftPar = Par . lift . lift


-- The reschedule loop waits for new work to become available as a result of
-- a thread calling 'put' on an IVar with blocked continuations.
--
-- Assuming that this queue is not under contention, and thus is acceptable to
-- use a pure data structure in an MVar. If there is contention here, we can
-- switch to a lock-free queue, but then we need to poll the end looking for new
-- work, which consumes cycles which could be used by the worker threads.
--
reschedule :: Par Native a
reschedule = Par $ ContT loop
  where
    loop :: unused -> ReaderT Schedule (LLVM Native) ()
    loop unused = do
      queue <- ask
      work  <- liftIO $ popR queue
      runContT (runPar work) (\_ -> loop unused)

pushL :: MVar (Seq a) -> a -> IO ()
pushL ref a =
  mask_ $ do
    ma <- tryTakeMVar ref
    case ma of
      Nothing -> putMVar ref (Seq.singleton a)
      Just as -> putMVar ref (a Seq.<| as)

popR :: MVar (Seq a) -> IO a
popR ref = do
  q <- takeMVar ref
  case Seq.viewr q of
    Seq.EmptyR  -> popR ref   -- should be impossible
    as Seq.:> a -> putMVar ref as >> return a

