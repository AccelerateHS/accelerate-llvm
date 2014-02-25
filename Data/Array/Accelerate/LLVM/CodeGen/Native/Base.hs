{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Native.Base
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Native.Base
  where

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.Global

-- accelerate
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad


-- Create a complete kernel function by running the code generation process
-- specified in the final parameter.
--
makeKernel :: Name -> [Parameter] -> CodeGen () -> CodeGen [Kernel t aenv a]
makeKernel kernel param body = do
  code <- body >> createBlocks
  return [ Kernel $ functionDefaults
             { returnType  = VoidType
             , name        = kernel
             , parameters  = (param, False)
             , basicBlocks = code
             } ]




