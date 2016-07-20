{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
-- Copyright   : [2014..2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Base (

  -- Thread identifiers
  blockDim, gridDim, threadIdx, blockIdx, warpSize,
  gridSize, globalThreadIdx,
  gangParam,

  -- Other intrinsics
  laneId, warpId,
  laneMask_eq, laneMask_lt, laneMask_le, laneMask_gt, laneMask_ge,

  -- Barriers and synchronisation
  __syncthreads,
  __threadfence_block, __threadfence_grid,

  -- Shared memory
  sharedMem,

  -- Kernel definitions
  makeOpenAcc,

) where

import Prelude                                                          as P
import Control.Monad                                                    ( void )

-- llvm
import LLVM.General.AST.Type.AddrSpace
import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Global
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Metadata
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Representation
import qualified LLVM.General.AST.AddrSpace                             as LLVM
import qualified LLVM.General.AST.Global                                as LLVM
import qualified LLVM.General.AST.Name                                  as LLVM
import qualified LLVM.General.AST.Type                                  as LLVM

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Array.Sugar                                ( Elt, Vector, eltType )

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                    as A
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Downcast
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.PTX.Target                            ( PTX )


-- Thread identifiers
-- ------------------

-- | Read the builtin registers that store CUDA thread and grid identifiers
--
-- <https://github.com/llvm-mirror/llvm/blob/master/include/llvm/IR/IntrinsicsNVVM.td>
--
specialPTXReg :: Label -> CodeGen (IR Int32)
specialPTXReg f =
  call (Body type' f) [NoUnwind, ReadNone]

blockDim, gridDim, threadIdx, blockIdx, warpSize :: CodeGen (IR Int32)
blockDim    = specialPTXReg "llvm.nvvm.read.ptx.sreg.ntid.x"
gridDim     = specialPTXReg "llvm.nvvm.read.ptx.sreg.nctaid.x"
threadIdx   = specialPTXReg "llvm.nvvm.read.ptx.sreg.tid.x"
blockIdx    = specialPTXReg "llvm.nvvm.read.ptx.sreg.ctaid.x"
warpSize    = specialPTXReg "llvm.nvvm.read.ptx.sreg.warpsize"

laneId :: CodeGen (IR Int32)
laneId      = specialPTXReg "llvm.ptx.read.laneid"

laneMask_eq, laneMask_lt, laneMask_le, laneMask_gt, laneMask_ge :: CodeGen (IR Int32)
laneMask_eq = specialPTXReg "llvm.ptx.read.lanemask.eq"
laneMask_lt = specialPTXReg "llvm.ptx.read.lanemask.lt"
laneMask_le = specialPTXReg "llvm.ptx.read.lanemask.le"
laneMask_gt = specialPTXReg "llvm.ptx.read.lanemask.gt"
laneMask_ge = specialPTXReg "llvm.ptx.read.lanemask.ge"

-- | NOTE: The special register %warpid as volatile value and is not guaranteed
--         to be constant over the lifetime of a thread or thread block.
--
-- http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#sm-id-and-warp-id
--
-- http://docs.nvidia.com/cuda/parallel-thread-execution/index.html#special-registers-warpid
--
-- We might consider passing in the (constant) warp size from device properties,
-- so that the division can be optimised to a shift.
--
warpId :: CodeGen (IR Int32)
warpId = do
  tid <- threadIdx
  ws  <- warpSize
  A.quot integralType tid ws

_warpId :: CodeGen (IR Int32)
_warpId = specialPTXReg "llvm.ptx.read.warpid"


-- | The size of the thread grid
--
-- > gridDim.x * blockDim.x
--
gridSize :: CodeGen (IR Int32)
gridSize = do
  ncta  <- gridDim
  nt    <- blockDim
  mul numType ncta nt


-- | The global thread index
--
-- > blockDim.x * blockIdx.x + threadIdx.x
--
globalThreadIdx :: CodeGen (IR Int32)
globalThreadIdx = do
  ntid  <- blockDim
  ctaid <- blockIdx
  tid   <- threadIdx
  --
  u     <- mul numType ntid ctaid
  v     <- add numType tid u
  return v


-- | Generate function parameters that will specify the first and last (linear)
-- index of the array this kernel should evaluate.
--
gangParam :: (IR Int32, IR Int32, [LLVM.Parameter])
gangParam =
  let t         = scalarType
      start     = "ix.start"
      end       = "ix.end"
  in
  (local t start, local t end, [ scalarParameter t start, scalarParameter t end ] )


-- Barriers and synchronisation
-- ----------------------------

-- | Call a builtin CUDA synchronisation intrinsic
--
barrier :: Label -> CodeGen ()
barrier f = void $ call (Body VoidType f) [NoUnwind, ReadNone]


-- | Wait until all threads in the thread block have reached this point and all
-- global and shared memory accesses made by these threads prior to
-- __syncthreads() are visible to all threads in the block.
--
-- <http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#synchronization-functions>
--
__syncthreads :: CodeGen ()
__syncthreads = barrier "llvm.nvvm.barrier0"


-- | Ensure that all writes to shared and global memory before the call to
-- __threadfence_block() are observed by all threads in the *block* of the
-- calling thread as occurring before all writes to shared and global memory
-- made by the calling thread after the call.
--
-- <http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#memory-fence-functions>
--
__threadfence_block :: CodeGen ()
__threadfence_block = barrier "llvm.nvvm.membar.cta"


-- | As __threadfence_block(), but the synchronisation is for *all* thread blocks.
-- In CUDA this is known simply as __threadfence().
--
__threadfence_grid :: CodeGen ()
__threadfence_grid = barrier "llvm.nvvm.membar.gl"


-- Shared memory
-- -------------

-- External declaration in shared memory address space. This must be declared in
-- order to access memory allocated dynamically by the CUDA driver. This results
-- in the following global declaration:
--
-- > @__shared__ = external addrspace(3) global [0 x i8]
--
initialiseSharedMemory :: CodeGen (Operand (Ptr Word8))
initialiseSharedMemory = do
  declare $ LLVM.globalVariableDefaults
    { LLVM.addrSpace = LLVM.AddrSpace 3
    , LLVM.type'     = LLVM.ArrayType 0 (LLVM.IntegerType 8)
    , LLVM.name      = LLVM.Name "__shared__"
    }
  return $ ConstantOperand $ GlobalReference type' "__shared__"


-- Declared a new dynamically allocated array in the __shared__ memory space
-- with enough space to contain the given number of elements.
--
sharedMem
    :: forall e int. (Elt e, IsIntegral int)
    => IR int                                 -- number of array elements
    -> Maybe (IR int)                         -- #bytes of shared memory the have already been allocated for this kernel (default: zero)
    -> CodeGen (IRArray (Vector e))
sharedMem n@(op integralType -> n') moffset = do
  let
      go :: TupleType s -> Operand (Ptr Word8) -> CodeGen (Operand (Ptr Word8), Operands s)
      go UnitTuple       p = return (p, OP_Unit)
      go (SingleTuple t) p = do
        -- TLM: bitcasts are no-op instructions. While having casts both in and
        --      out in order to satisfy the types is somewhat inelegant, they
        --      cost us no cycles (at runtime; the optimiser does work removing
        --      adjacent casts)
        s <- instr' $ PtrCast (PtrPrimType t as) p
        q <- instr' $ GetElementPtr s [n']
        r <- instr' $ PtrCast (PtrPrimType scalarType as) q
        -- This is a hack because we can't easily create an 'EltRepr (Ptr a)'
        -- type and associated encoding with operands, since we don't have the
        -- appropriate TupleType proof object.
        let s' = case s of
                   LocalReference _ (Name x)   -> LocalReference (PrimType (ScalarPrimType t)) (Name x)
                   LocalReference _ (UnName x) -> LocalReference (PrimType (ScalarPrimType t)) (UnName x)
                   _                           -> $internalError "sharedMem" "unexpected global reference"
        return (r, ir' t s')
      go (PairTuple t2 t1) p = do
        (p1, ad1) <- go t1 p
        (p2, ad2) <- go t2 p1
        return $ (p2, OP_Pair ad2 ad1)

      as      = AddrSpace 3
      zero    = scalar scalarType 0
      offset  = maybe zero (op integralType) moffset
  --
  smem   <- initialiseSharedMemory
  ptr    <- instr' $ GetElementPtr smem [offset]
  IR sz  <- A.fromIntegral integralType (numType :: NumType Int) n
  (_,ad) <- go (eltType (undefined::e)) ptr
  return $ IRArray { irArrayShape = IR $ OP_Pair OP_Unit sz
                   , irArrayData  = IR ad
                   }


-- Global kernel definitions
-- -------------------------

-- | Create a single kernel program
--
makeOpenAcc :: Label -> [LLVM.Parameter] -> CodeGen () -> CodeGen (IROpenAcc PTX aenv a)
makeOpenAcc name param kernel = do
  body <- makeKernel name param kernel
  return $ IROpenAcc [body]

-- | Create a complete kernel function by running the code generation process
-- specified in the final parameter.
--
makeKernel :: Label -> [LLVM.Parameter] -> CodeGen () -> CodeGen (Kernel PTX aenv a)
makeKernel name@(Label l) param kernel = do
  _    <- kernel
  code <- createBlocks
  addMetadata "nvvm.annotations"
    [ Just . MetadataOperand       $ ConstantOperand (GlobalReference VoidType (Name l))
    , Just . MetadataStringOperand $ "kernel"
    , Just . MetadataOperand       $ scalar scalarType (1::Int)
    ]
  return . Kernel $ LLVM.functionDefaults
    { LLVM.returnType  = LLVM.VoidType
    , LLVM.name        = downcast name
    , LLVM.parameters  = (param, False)
    , LLVM.basicBlocks = code
    }

