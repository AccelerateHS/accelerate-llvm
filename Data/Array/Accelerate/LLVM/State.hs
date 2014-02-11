{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.State
-- Copyright   :
-- License     :
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
import Control.Exception                                ( SomeException, bracket_, catch )
import Control.Monad.Reader                             ( ReaderT, MonadReader, runReaderT )
import Control.Monad.State                              ( StateT, MonadState, evalStateT )
import Control.Monad.Trans                              ( MonadIO )
import System.IO.Unsafe                                 ( unsafePerformIO )

#include "accelerate.h"


-- Execution state
-- ===============

-- | The LLVM monad, for executing array computations
--
newtype LLVM a = LLVM { runLLVM :: ReaderT Context (StateT State IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context, MonadState State)

-- | The state for executing accelerate array computations using LLVM. This
-- consists of a stack of (read only) device properties and execution context,
-- as well as mutable state for device memory and kernel object code.
--
data State = State {
    -- memory table, kernel table, etc.
  }

data Context = Context {
    llvmContext         :: LLVM.Context
  }


evalLLVM :: Context -> LLVM a -> IO a
evalLLVM ctx acc =
  LLVM.withContext $ \lc ->
    let setup    = return ()
        teardown = return ()
        action   = evalStateT (runReaderT (runLLVM acc) (ctx { llvmContext = lc })) theState
    in
    bracket_ setup teardown action
    `catch`
    \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: SomeException))


-- Top-level mutable state
-- -----------------------
--
-- It is important to keep some information alive for the entire run of the
-- program, not just a single execution. These tokens use unsafePerformIO to
-- ensure they are executed only once, and reused for subsequent invocations.
--
{-# NOINLINE theState #-}
theState :: State
theState = unsafePerformIO $ do return State

