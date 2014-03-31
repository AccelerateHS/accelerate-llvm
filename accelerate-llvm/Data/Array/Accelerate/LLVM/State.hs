{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.State
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.State
  where

-- llvm-general
import qualified LLVM.General.Context                   as LLVM

-- library
import Control.Applicative                              ( Applicative )
import Control.Concurrent                               ( forkIO, threadDelay )
import Control.Monad.Reader                             ( ReaderT, MonadReader, runReaderT )
import Control.Monad.State                              ( StateT, MonadState, evalStateT )
import Control.Monad.Trans                              ( MonadIO )


-- Execution state
-- ===============

-- | The LLVM monad, for executing array computations. This consists of a stack
-- for the LLVM execution context as well as the per-execution target specific
-- state 'target'.
--
newtype LLVM target a = LLVM { runLLVM :: ReaderT LLVM.Context (StateT target IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader LLVM.Context, MonadState target)

-- | Extract the execution state: 'gets llvmTarget'
--
llvmTarget :: t -> t
llvmTarget = id

-- | Extract the LLVM context: 'gets llvmContext'
--
llvmContext :: LLVM.Context -> LLVM.Context
llvmContext = id


-- | Evaluate the given target with an LLVM context
--
evalLLVM :: t -> LLVM t a -> IO a
evalLLVM target acc =
  LLVM.withContext $ \ctx ->
    evalStateT (runReaderT (runLLVM acc) ctx) target


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

