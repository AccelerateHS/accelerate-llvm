{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.State
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.State
  where

-- accelerate
import Data.Array.Accelerate.LLVM.Target.ClangInfo

-- llvm-pretty
import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty.PP as LP

-- standard library
import Control.Monad.Catch                              ( MonadCatch, MonadThrow, MonadMask )
import Control.Monad.Reader                             ( ReaderT(..), MonadReader, runReaderT, ask, local )
import Control.Monad.Trans                              ( MonadIO, lift )
import Prelude


-- Execution state
-- ===============

-- | The LLVM monad, for executing array computations. This consists of a stack
-- for the LLVM execution context as well as the per-execution target specific
-- state 'target'.
--
newtype LLVM target a = LLVM { runLLVM :: ReaderT LP.LLVMVer (ReaderT target IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- not derived because the LLVMVer reader masks this one
instance MonadReader target (LLVM target) where
  ask = LLVM (lift ask)
  local f (LLVM (ReaderT g)) = LLVM (ReaderT (local f . g))

-- | Extract the execution state: 'asks llvmTarget'
--
llvmTarget :: t -> t
llvmTarget = id

-- | Evaluate the given target with an LLVM context
--
evalLLVM :: t -> LLVM t a -> IO a
evalLLVM target acc =
  case llvmverFromTuple hostLLVMVersion of
    Just version -> runReaderT (runReaderT (runLLVM acc) version) target
    Nothing -> fail "accelerate-llvm: Could not determine LLVM version from Clang output"

getLLVMVer :: LLVM target LP.LLVMVer
getLLVMVer = LLVM ask

-- | This is a valid implementation of @withRunInIO@ in
-- unliftio-core:Control.Monad.IO.Unlift(MonadUnliftIO); it's not an instance
-- to avoid a dependency.
unliftIOLLVM :: ((forall a. LLVM target a -> IO a) -> IO b) -> LLVM target b
unliftIOLLVM f = LLVM (ReaderT (\llvmver -> ReaderT (\target -> f (run llvmver target))))
  where
    run :: LP.LLVMVer -> target -> LLVM target a -> IO a
    run llvmver target (LLVM m) = runReaderT (runReaderT m llvmver) target


-- -- | Make sure the GC knows that we want to keep this thing alive forever.
-- --
-- -- We may want to introduce some way to actually shut this down if, for example,
-- -- the object has not been accessed in a while (whatever that means).
-- --
-- -- Broken in ghci-7.6.1 Mac OS X due to bug #7299.
-- --
-- keepAlive :: a -> IO a
-- keepAlive x = forkIO (caffeine x) >> return x
--   where
--     caffeine hit = do threadDelay (5 * 1000 * 1000) -- microseconds = 5 seconds
--                       caffeine hit

