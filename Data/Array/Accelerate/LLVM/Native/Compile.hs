{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile
  where

-- llvm-general
import LLVM.General
import LLVM.General.ExecutionEngine
import LLVM.General.AST.Name

-- accelerate
import Data.Array.Accelerate.LLVM.State

-- standard library
import Control.Monad.Trans
import Control.Monad.Reader
import Foreign.Ptr

#include "accelerate.h"


-- | Compile an LLVM module to native target, and return function pointers to
-- the named functions within the module.
--
compileForMCJIT :: Module -> [Name] -> LLVM [FunPtr ()]
compileForMCJIT mdl f = do
  ctx <- asks llvmContext
  liftIO $ withMCJIT ctx opt code fptr fins $ \jit ->
      withModuleInEngine jit mdl $ \exe ->
        forM f $ fmap check . getFunction exe
  where
    opt  = Just 3        -- optimisation level
    code = Nothing       -- code model (default)
    fptr = Nothing       -- disable frame pointer elimination?
    fins = Just True     -- use fast instruction selection?

    check Nothing  = INTERNAL_ERROR(error) "compileForMCJIT" "unknown function"
    check (Just p) = p

