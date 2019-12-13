{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Async
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Async (

  Async(..), Future(..), IVar(..), getArrays,
  evalPar, liftPar, putIO,

) where

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.Native.Execute.Scheduler
import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.State

-- standard library
import Control.Concurrent
import Control.Monad.Cont
import Control.Monad.State
import Data.IORef
import Data.Sequence                                                ( Seq )
import qualified Data.Sequence                                      as Seq


-- | Evaluate a parallel computation
--
-- The worker threads execute the computation, while the calling thread
-- effectively sleeps waiting for the result.
--
{-# INLINEABLE evalPar #-}
evalPar :: Par Native a -> LLVM Native a
evalPar work = do
  result <- liftIO newEmptyMVar
  runContT (runPar work) (liftIO . putMVar result)
  liftIO $ takeMVar result

  -- XXX: Running the initial computation on the worker threads can lead to the
  -- workers becoming blocked, possibly waiting for the result MVars to be
  -- filled from previous (lazily evaluated) computations (speculation). This
  -- happened for example with the code from Issue255, when extracting the
  -- result at index > number of worker threads.
  --
  -- liftIO  $ do
  --   schedule (workers native)
  --     Job { jobTasks = Seq.singleton $ evalLLVM native (runContT (runPar work) (liftIO . putMVar result))
  --         , jobDone  = Nothing
  --         }
  --   takeMVar result


-- Implementation
-- --------------

data Future a = Future {-# UNPACK #-} !(IORef (IVar a))

data IVar a
    = Full    !a
    | Blocked !(Seq (a -> IO ()))
    | Empty

instance Async Native where
  type FutureR Native  = Future
  newtype Par Native a = Par { runPar :: ContT () (LLVM Native) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadCont, MonadState Native )

  {-# INLINE new     #-}
  {-# INLINE newFull #-}
  new       = Future <$> liftIO (newIORef Empty)
  newFull v = Future <$> liftIO (newIORef (Full v))

  {-# INLINE fork  #-}
  {-# INLINE spawn #-}
  fork  = id
  spawn = id

  {-# INLINE get #-}
  get (Future ref) =
    callCC $ \k -> do
      native <- gets llvmTarget
      next   <- liftIO . atomicModifyIORef' ref $ \case
                  Empty      -> (Blocked (Seq.singleton (evalParIO native . k)), reschedule)
                  Blocked ks -> (Blocked (ks Seq.|>      evalParIO native . k),  reschedule)
                  Full a     -> (Full a,                                         return a)
      next

  {-# INLINE put #-}
  put future ref = do
    Native{..} <- gets llvmTarget
    liftIO (putIO workers future ref)


-- | Lift an operation from the base LLVM monad into the Par monad
--
{-# INLINE liftPar #-}
liftPar :: LLVM Native a -> Par Native a
liftPar = Par . lift

-- | Evaluate a continuation
--
{-# INLINE evalParIO #-}
evalParIO :: Native -> Par Native () -> IO ()
evalParIO native@Native{..} work =
  evalLLVM native (runContT (runPar work) return)

-- | The value represented by a future is now available. Push any blocked
-- continuations to the worker threads.
--
{-# INLINEABLE putIO #-}
putIO :: Workers -> Future a -> a -> IO ()
putIO workers (Future ref) v = do
  ks <- atomicModifyIORef' ref $ \case
          Empty      -> (Full v, Seq.empty)
          Blocked ks -> (Full v, ks)
          _          -> $internalError "put" "multiple put"
  --
  schedule workers Job { jobTasks = fmap ($ v) ks
                       , jobDone  = Nothing
                       }

-- | The worker threads should search for other work to execute
--
{-# INLINE reschedule #-}
reschedule :: Par Native a
reschedule = Par $ ContT (\_ -> return ())


-- reschedule :: Par Native a
-- reschedule = Par $ ContT (const loop)
--   where
--     loop :: ReaderT Schedule (LLVM Native) ()
--     loop = do
--       queue <- ask
--       mwork <- liftIO $ tryPopR queue
--       case mwork of
--         Just work -> runContT (runPar work) (const loop)
--         Nothing   -> liftIO yield >> loop

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

