{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Pool
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Pool (

  Pool,
  create, with, take, put,
  unsafeWith,

) where

import Control.Concurrent.MVar
import Control.Exception
import Data.Maybe
import System.IO.Unsafe
import Prelude                                                      hiding ( take )

import Data.Sequence                                                ( Seq )
import qualified Data.Sequence                                      as Seq


-- | An item pool
--
-- Based on 'Control.Concurrent.QSem'
--
data Pool a = Pool {-# UNPACK #-} !(MVar ([a], Seq (MVar a)))

-- The semaphore state (as, bs):
--
-- * as   the currently available resources
--
-- * bs   is the queue of blocked threads, stored in FIFO order. New threads are
--        queued onto the right, and threads are woken up from the left.
--
-- A blocked thread is represented by an empty (MVar a). To unblock the thread,
-- we give it a resource via its MVar.
--
-- A thread can deque itself by also putting () into the MVar, which it must do
-- if it receives an exception while blocked in 'take'. This means that when
-- unblocking a thread in 'put' we must first check whether the MVar is already
-- full; the MVar lock on the semaphore itself resolves race conditions between
-- put and a thread attempting to deque itself.
--

-- | Build a new pool with the supplied initial quantity.
--
create :: [a] -> IO (Pool a)
create initial =
  Pool <$> newMVar (initial, Seq.empty)

-- | Wait for a unit of the resource to become available, and run the supplied
-- action given that resource.
--
with :: Pool a -> (a -> IO b) -> IO b
with pool action =
  bracket (take pool) (put pool) action

unsafeWith :: Pool a -> (a -> b) -> b
unsafeWith pool action =
  unsafePerformIO $ with pool (evaluate . action)


-- | Wait for an item from the pool to become available.
--
take :: Pool a -> IO a
take (Pool ref) =
  mask_ $ do
    (r, bs) <- takeMVar ref
    case r of
      [] -> do
        b <- newEmptyMVar
        putMVar ref (r, bs Seq.|> b)
        wait b

      (a:as) -> do
        putMVar ref (as, bs)
        return a
  where
    wait b =
      takeMVar b `catch` \(e :: SomeException) ->
        uninterruptibleMask_ $ do  -- Note [signal interruptible]
          r  <- takeMVar ref
          ma <- tryTakeMVar b
          r' <- case ma of
                  Just a  -> signal a r               -- make sure we don't lose the resource
                  Nothing -> do putMVar b (throw e)   -- unblock the thread??
                                return r
          putMVar ref r'
          throwIO e


-- | Return a unit of the resource to the pool.
--
put :: Pool a -> a -> IO ()
put (Pool ref) a =
  uninterruptibleMask_ $ do   -- Note [signal interruptible]
    r  <- takeMVar ref
    r' <- signal a r
    putMVar ref r'


-- Note [signal interruptible]
--
-- If we have:
--
-- > bracket take put (...)
--
-- and an exception arrives at the put, then we must not lose the resource. The
-- put is masked by bracket, but taking the MVar might block, and so it would be
-- interruptible. Hence we need an uninterruptibleMask here.
--
signal :: a -> ([a], Seq (MVar a)) -> IO ([a], Seq (MVar a))
signal a (as, blocked) =
  if null as
    then loop blocked           -- there may be waiting threads; wake one up
    else return (a:as, blocked) -- nobody waiting
  where
    loop blocked' =
      case Seq.viewl blocked' of
        Seq.EmptyL  -> return ([a], Seq.empty)
        b Seq.:< bs -> do
          r <- tryPutMVar b a
          if r then return ([], bs)     -- we woke up a thread
               else loop bs             -- already unblocked; drop from the queue

{--
-- | An item pool
--
data Pool a = Pool {-# UNPACK #-} !(MVar (NonEmpty a))


-- | Create a new pooled resource containing the given items
--
create :: [a] -> IO (Pool a)
create []     = Pool <$> newEmptyMVar
create (x:xs) = Pool <$> newMVar (x :| xs)


-- | Execute an operation using an item from the pool. Like 'take', the function
-- blocks until one becomes available.
--
with :: Pool a -> (a -> IO b) -> IO b
with pool action =
  bracket (take pool) (put pool) action

unsafeWith :: Pool a -> (a -> b) -> b
unsafeWith pool action =
  unsafePerformIO $ with pool (pure . action)


-- | Take an item from the pool. This will block until one is available.
--
take :: Pool a -> IO a
take (Pool ref) = do
  x :| xs <- takeMVar ref -- blocking
  case xs of
    []     -> return ()   -- leave the pool empty; subsequent 'take's will block
    (a:as) -> mask_ $ do r <- tryTakeMVar ref
                         case r of
                           Nothing        -> putMVar ref (a :| as)
                           Just (b :| bs) -> putMVar ref (a :| b : bs ++ as)
  return x


-- | Return an item back to the pool for later reuse. This should be
-- a non-blocking operation.
--
put :: Pool a -> a -> IO ()
put (Pool ref) a =
  mask_ $ do
    it <- tryTakeMVar ref
    case it of
      Just (b :| bs) -> putMVar ref (a :| b : bs)
      Nothing        -> putMVar ref (a :| [])


#if __GLASGOW_HASKELL__ < 800
-- | Non-empty (and non-strict) list type.
--
infixr 5 :|
data NonEmpty a = a :| [a]
  deriving ( Eq, Ord, Show, Read )
#endif
--}

