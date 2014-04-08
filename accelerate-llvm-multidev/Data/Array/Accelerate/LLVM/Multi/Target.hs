{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.Target
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Multi.Target (

  module Data.Array.Accelerate.LLVM.Target,
  module Data.Array.Accelerate.LLVM.Multi.Target,

) where

-- accelerate
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Target
import Data.Array.Accelerate.LLVM.PTX.Target                    ( PTX )
import Data.Array.Accelerate.LLVM.Native.Target                 ( Native )

-- standard library
import Control.Monad.Reader
import Control.Monad.State

#include "accelerate.h"


-- | The multi-device target is a collection of several manifest targets; in
-- this case the PTX generating GPU backend as well as the native backend for
-- execution on the host CPU. Thus, we can execute a given Accelerate operation
-- with either or both of these target backends.
--
data Multi = Multi {
    ptxTarget           :: {-# UNPACK #-} !PTX
  , nativeTarget        :: {-# UNPACK #-} !Native
  }

instance Target Multi where
  data ExecutableR Multi = MultiR {
          ptxExecutable    :: ExecutableR PTX
        , nativeExecutable :: ExecutableR Native
        }

  targetTriple _     = INTERNAL_ERROR(error) "targetTriple" "I am an abstract target"
  targetDataLayout _ = INTERNAL_ERROR(error) "targetDataLayout" "I am an abstract target"


with :: LLVM t a -> (Multi -> t) -> LLVM Multi a
with action f = do
  target <- gets f
  ctx    <- asks llvmContext
  liftIO $ evalStateT (runReaderT (runLLVM action) ctx) target

