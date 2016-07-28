{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Module
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Module
  where

-- llvm-general
import qualified LLVM.General.AST                               as LLVM


-- | A compiled module consists of a number of global functions (kernels)
--
data Module arch aenv a = Module { unModule :: LLVM.Module }

-- | A fully-instantiated skeleton is a [collection of] kernel(s) that can be compiled
-- by LLVM into a global function that we can execute.
--
data Kernel arch aenv a = Kernel { unKernel :: LLVM.Global }

