-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Module
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--


module Data.Array.Accelerate.LLVM.CodeGen.Module
  where

-- llvm-general
import qualified LLVM.General.AST                               as AST


-- | A compiled module consists of a number of global functions (kernels)
--
data Module t aenv a = Module { unModule :: AST.Module }

-- | A fully-instantiated skeleton is a kernel that can be compiled by LLVM into
-- a global function that we can execute.
--
-- The data type, rather than type synonym, is required to fix the phantom type
-- parameters, which is useful during code generation.
--
data Kernel t aenv a = Kernel { unKernel :: AST.Global }


