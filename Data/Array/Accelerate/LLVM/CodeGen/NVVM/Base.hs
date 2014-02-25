{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.NVVM.Base
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.NVVM.Base
  where

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.Global

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape )

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

-- standard library
import Control.Monad


-- Standard CUDA thread and grid identifiers
--
blockDim, gridDim, threadIdx, blockIdx :: Operand
blockDim  = local "blockDim.x"
gridDim   = local "gridDim.x"
threadIdx = local "threadIdx.x"
blockIdx  = local "blockIdx.x"

-- The total number of elements in the given array. The first argument is a
-- dummy to fix the types. Note that the output operand is truncated to a 32-bit
-- int.
--
shapeSize :: Shape sh => Array sh e -> Name -> CodeGen Operand
shapeSize arr base =
  let sh = arrayShape arr base
  in  trunc int32 =<< foldM (mul int) (constOp $ num int 1) (map local sh)

-- The size of the thread grid.
--
gridSize :: CodeGen Operand
gridSize = mul int32 blockDim gridDim


makeKernel :: Name -> [Parameter] -> CodeGen () -> CodeGen [Kernel t aenv a]
makeKernel kernel param body = do
  code <- body >> createBlocks
  addMetadata "nvvm.annotations" [ Just $ global kernel
                                 , Just $ MetadataStringOperand "kernel"
                                 , Just $ constOp (num int32 1) ]
  return [ Kernel $ functionDefaults
             { returnType  = VoidType
             , name        = kernel
             , parameters  = (param, False)
             , basicBlocks = code
             } ]

