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
  evalPar, liftPar, putIO,

) where

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.State

-- standard library
import Control.Concurrent
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Data.Concurrent.Queue.MichaelScott
import Data.IORef


-- | Evaluate a parallel computation
--
evalPar :: Par Native a -> LLVM Native a
evalPar work = do
  queue   <- liftIO newQ
  result  <- liftIO newEmptyMVar
  runReaderT (runContT (runPar work) (liftIO . putMVar result)) queue
  liftIO $ takeMVar result


-- Implementation
-- --------------

data Future a = Future {-# UNPACK #-} !(IORef (IVar a))
type Schedule = LinkedQueue (Par Native ())

data IVar a
    = Full !a
    | Blocked [a -> IO ()]
    | Empty

instance Async Native where
  type FutureR Native  = Future
  newtype Par Native a = Par { runPar :: ContT () (ReaderT Schedule (LLVM Native)) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadCont, MonadReader Schedule, MonadState Native )

  {-# INLINE new     #-}
  {-# INLINE newFull #-}
  new       = Future <$> liftIO (newIORef Empty)
  newFull v = Future <$> liftIO (newIORef (Full v))

  {-# INLINE get #-}
  get (Future ref) =
    callCC $ \k -> do
      queue  <- ask
      next   <- liftIO . atomicModifyIORef' ref $ \case
                  Empty      -> (Blocked [pushL queue . k],      reschedule)
                  Blocked ks -> (Blocked (pushL queue . k : ks), reschedule)
                  Full a     -> (Full a,                         return a)
      next

  {-# INLINE put #-}
  put future ref = liftIO (putIO future ref)

  {-# INLINE fork #-}
  fork child = do
    queue <- ask
    callCC $ \parent -> do
      liftIO $ pushL queue (parent ())
      child
      reschedule

  {-# INLINE spawn #-}
  spawn = id


-- | Lift and operation from the base LLVM monad into the Par monad
--
{-# INLINE liftPar #-}
liftPar :: LLVM Native a -> Par Native a
liftPar = Par . lift . lift

-- | The value represented by a future is now available. This version in IO is
-- callable by scheduler threads.
--
{-# INLINE putIO #-}
putIO :: Future a -> a -> IO ()
putIO (Future ref) v = do
  ks <- atomicModifyIORef' ref $ \case
          Empty      -> (Full v, [])
          Blocked ks -> (Full v, ks)
          _          -> $internalError "put" "multiple put"
  mapM_ ($ v) (reverse ks)


-- The reschedule loop checks for new work to become available as a result of
-- a thread calling 'put' on an IVar with blocked continuations.
--
{-# INLINE reschedule #-}
reschedule :: Par Native a
reschedule = Par $ ContT loop
  where
    loop :: unused -> ReaderT Schedule (LLVM Native) ()
    loop unused = do
      queue <- ask
      mwork <- liftIO $ tryPopR queue
      case mwork of
        Just work -> runContT (runPar work) (\_ -> loop unused)
        Nothing   -> liftIO yield >> loop unused

-- pushL :: MVar (Seq a) -> a -> IO ()
-- pushL ref a =
--   mask_ $ do
--     ma <- tryTakeMVar ref
--     case ma of
--       Nothing -> putMVar ref (Seq.singleton a)
--       Just as -> putMVar ref (a Seq.<| as)

-- popR :: MVar (Seq a) -> IO a
-- popR ref = do
--   q <- takeMVar ref
--   case Seq.viewr q of
--     Seq.EmptyR  -> popR ref   -- should be impossible
--     as Seq.:> a -> putMVar ref as >> return a

