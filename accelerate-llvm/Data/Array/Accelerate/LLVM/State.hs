{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.State
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
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
import Control.Monad.Reader                             ( ReaderT, MonadReader, runReaderT )
import Control.Monad.State                              ( StateT, MonadState, evalStateT )
import Control.Monad.Trans                              ( MonadIO )


-- Execution state
-- ===============

-- | The LLVM monad, for executing array computations
--
newtype LLVM t a = LLVM { runLLVM :: ReaderT Context (StateT (State t) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context, MonadState (State t))

-- | The state for executing accelerate array computations using LLVM. This
-- consists of a stack of (read only) device properties and execution context,
-- as well as mutable state for device memory and kernel object code.
--
data State t = State {
    -- memory table, kernel table, etc.
    llvmTarget          :: t
  }

data Context = Context {
    llvmContext         :: LLVM.Context
  }


evalLLVM :: t -> LLVM t a -> IO a
evalLLVM target acc =
  LLVM.withContext $ \ctx ->
    evalStateT (runReaderT (runLLVM acc) (Context ctx)) (State target)


