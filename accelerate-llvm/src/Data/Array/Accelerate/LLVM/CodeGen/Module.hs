{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Module
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Module
  where

import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty as LLVM

import Data.Map.Strict                                    ( Map )


-- | A compiled module consists of a number of global functions (kernels). The
-- module additionally includes a map from the callable function definitions to
-- the metadata for that function.
--
data Module arch aenv a
  = Module { unModule       :: LLVM.Module
           , moduleMetadata :: Map LLVM.Symbol (KernelMetadata arch)
           }

-- | A fully-instantiated skeleton is a [collection of] kernel(s) that can be compiled
-- by LLVM into a global function that we can execute.
--
data Kernel arch aenv a
  = Kernel { unKernel       :: LLVM.Define
           , kernelMetadata :: KernelMetadata arch
           }

-- | Kernels can be annotated with extra target-specific information
--
data family KernelMetadata arch

