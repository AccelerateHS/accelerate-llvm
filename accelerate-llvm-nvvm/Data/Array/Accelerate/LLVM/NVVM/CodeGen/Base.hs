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
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Attribute
import LLVM.General.AST.Global                                  as G
import LLVM.General.AST.CallingConvention

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Elt, eltType )
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


-- The size of the thread grid
--
-- > gridDim.x * blockDim.x
--
gridSize :: CodeGen Operand
gridSize = do
  ncta  <- gridDim
  nt    <- blockDim
  mul int32 ncta nt


-- The global thread index
--
-- > blockDim.x * blockIdx.x + threadIdx.x
--
globalThreadIdx :: CodeGen Operand
globalThreadIdx = do
  ntid  <- blockDim
  ctaid <- blockIdx
  tid   <- threadIdx

  u     <- mul int32 ntid ctaid
  v     <- add int32 tid u
  return v


-- Thread barriers
--
__syncthreads :: CodeGen ()
__syncthreads =
  let fn        = "llvm.nvvm.barrier0"
      attrs     = [NoUnwind]
      decl      = functionDefaults { name = fn, returnType = VoidType, G.functionAttributes = attrs }
  in do
    declare decl
    do_ $ Call True C [] (Right (global fn)) [] attrs []


-- External declaration in shared memory address space. This is used to access
-- memory allocated dynamically by the CUDA driver. This results in the
-- following global declaration:
--
-- > @__shared__ = external addrspace(3) global [0 x i8]
--
initialiseSharedMemory :: CodeGen ()
initialiseSharedMemory =
  declare $ globalVariableDefaults
    { addrSpace = AddrSpace 3
    , G.type'   = ArrayType 0 (IntegerType 8)
    , G.name    = "__shared__"
    }


-- Generate names for dynamically allocated __shared__ memory.
--
sharedMem
    :: Elt e
    => e                        -- dummy to fix the type of the shared array
    -> Operand                  -- how many elements of shared memory to reserve for each type
    -> CodeGen [Name]
sharedMem dummy nelt =
  let ty        = llvmOfTupleType (eltType dummy)
      offset    = constOp (num int32 0) : repeat nelt

      shared1 :: Operand -> Operand -> Type -> CodeGen Operand
      shared1 a i t = do
        p <- instr $ GetElementPtr False a [i] []
        s <- instr $ BitCast p (PointerType t (AddrSpace 3)) []
        return s

      shared _ _      []     = return []
      shared _ []     _      = return []
      shared a (i:is) (t:ts) = do
        a'@(LocalReference n)   <- shared1 a  i  t
        ns                      <- shared  a' is ts
        return (n:ns)
  in
  shared (global "__shared__") offset ty


-- The total number of elements in the given array. The first argument is a
-- dummy to fix the types. Note that the output operand is truncated to a 32-bit
-- integer.
--
shapeSize :: Rvalue a => [a] -> CodeGen Operand
shapeSize []     = return $ constOp (num int32 1)
shapeSize [x]    = trunc int32 (rvalue x)
shapeSize (x:xs) = trunc int32 =<< foldM (mul int) (rvalue x) (map rvalue xs)


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

