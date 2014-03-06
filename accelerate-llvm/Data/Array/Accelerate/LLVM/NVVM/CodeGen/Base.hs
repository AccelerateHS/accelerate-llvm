{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Base
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.CodeGen.Base
  where

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.Attribute
import LLVM.General.AST.Global                                  as G
import LLVM.General.AST.CallingConvention

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape )
import Data.Array.Accelerate.Type

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
blockDim, gridDim, threadIdx, blockIdx, warpSize :: CodeGen Operand
blockDim  = specialPTXReg "llvm.nvvm.read.ptx.sreg.ntid.x"
gridDim   = specialPTXReg "llvm.nvvm.read.ptx.sreg.nctaid.x"
threadIdx = specialPTXReg "llvm.nvvm.read.ptx.sreg.tid.x"
blockIdx  = specialPTXReg "llvm.nvvm.read.ptx.sreg.ctaid.x"
warpSize  = specialPTXReg "llvm.nvvm.read.ptx.sreg.warpsize"

specialPTXReg :: Name -> CodeGen Operand
specialPTXReg reg =
  call reg (typeOf (int32 :: IntegralType Int32)) [] [NoUnwind, ReadNone]


-- Thread barriers
--
__syncthreads :: CodeGen ()
__syncthreads =
  let fn        = "llvm.nvvm.barrier0"
      attrs     = [NoUnwind, ReadOnly]
      decl      = functionDefaults { name = fn, returnType = VoidType, G.functionAttributes = attrs }
  in do
    declare decl
    do_ $ Call False C [] (Right (global fn)) [] attrs []


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
gridSize = do
  ncta  <- gridDim
  nt    <- blockDim
  mul int32 ncta nt

-- Create a complete kernel function by running the code generation sequence
-- specified at the final parameter. The function is annotated as being a
-- __global__ kernel function.
--
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

