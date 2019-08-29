{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.Target
-- Copyright   : [2014..2015] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Multi.Target
  where

-- accelerate
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.PTX.Internal                  ( PTX )
import Data.Array.Accelerate.LLVM.Native.Internal               ( Native )

import Control.Parallel.Meta.Worker

-- standard library
import Control.Monad.State


-- | The multi-device target is a collection of several manifest targets; in
-- this case the PTX generating GPU backend as well as the native backend for
-- execution on the host CPU. Thus, we can execute a given Accelerate operation
-- with either or both of these target backends.
--
data Multi = Multi {
    ptxTarget           :: {-# UNPACK #-} !PTX
  , nativeTarget        :: {-# UNPACK #-} !Native
  , monitorGang         :: {-# UNPACK #-} !Gang
  }


with :: LLVM t a -> (Multi -> t) -> LLVM Multi a
with action f = do
  target <- gets f
  liftIO $ evalStateT (runLLVM action) target

