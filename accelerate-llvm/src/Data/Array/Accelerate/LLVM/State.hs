{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.State
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.State
  where

-- library
import Control.Concurrent                               ( forkIO, threadDelay )
import Control.Monad.Catch                              ( MonadCatch, MonadThrow, MonadMask )
import Control.Monad.State                              ( StateT, MonadState, evalStateT )
import Control.Monad.Trans                              ( MonadIO )
import Prelude


-- Execution state
-- ===============

-- | The LLVM monad, for executing array computations. This consists of a stack
-- for the LLVM execution context as well as the per-execution target specific
-- state 'target'.
--
newtype LLVM target a = LLVM { runLLVM :: StateT target IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState target, MonadThrow, MonadCatch, MonadMask)

-- | Extract the execution state: 'gets llvmTarget'
--
llvmTarget :: t -> t
llvmTarget = id

-- | Evaluate the given target with an LLVM context
--
evalLLVM :: t -> LLVM t a -> IO a
evalLLVM target acc =
  evalStateT (runLLVM acc) target


-- | Make sure the GC knows that we want to keep this thing alive forever.
--
-- We may want to introduce some way to actually shut this down if, for example,
-- the object has not been accessed in a while (whatever that means).
--
-- Broken in ghci-7.6.1 Mac OS X due to bug #7299.
--
keepAlive :: a -> IO a
keepAlive x = forkIO (caffeine x) >> return x
  where
    caffeine hit = do threadDelay (5 * 1000 * 1000) -- microseconds = 5 seconds
                      caffeine hit

