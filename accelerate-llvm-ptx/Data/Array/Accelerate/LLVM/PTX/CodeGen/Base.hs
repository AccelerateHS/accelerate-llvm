{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
-- Copyright   : [2014..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Base (

  -- Types
  DeviceProperties, KernelMetadata(..),

  -- Thread identifiers
  blockDim, gridDim, threadIdx, blockIdx, warpSize,
  gridSize, globalThreadIdx,
  gangParam,

  -- Other intrinsics
  laneId, warpId,
  laneMask_eq, laneMask_lt, laneMask_le, laneMask_gt, laneMask_ge,
  atomicAdd_f,

  -- Barriers and synchronisation
  __syncthreads,
  __threadfence_block, __threadfence_grid,

  -- Shared memory
  staticSharedMem,
  dynamicSharedMem,
  sharedMemAddrSpace,

  -- Kernel definitions
  (+++),
  makeOpenAcc, makeOpenAccWith,

) where

-- llvm
import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Constant
import LLVM.AST.Type.Global
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Metadata
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation
import qualified LLVM.AST.Global                                    as LLVM
import qualified LLVM.AST.Linkage                                   as LLVM
import qualified LLVM.AST.Name                                      as LLVM
import qualified LLVM.AST.Type                                      as LLVM

-- accelerate
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Sugar                            ( Elt, Vector, eltType )
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Downcast
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Ptr
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Target

-- standard library
import Control.Applicative
import Control.Monad                                                ( void )
import Text.Printf
import Prelude                                                      as P


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
laneId      = specialPTXReg "llvm.nvvm.read.ptx.sreg.laneid"

laneMask_eq, laneMask_lt, laneMask_le, laneMask_gt, laneMask_ge :: CodeGen (IR Int32)
laneMask_eq = specialPTXReg "llvm.nvvm.read.ptx.sreg.lanemask.eq"
laneMask_lt = specialPTXReg "llvm.nvvm.read.ptx.sreg.lanemask.lt"
laneMask_le = specialPTXReg "llvm.nvvm.read.ptx.sreg.lanemask.le"
laneMask_gt = specialPTXReg "llvm.nvvm.read.ptx.sreg.lanemask.gt"
laneMask_ge = specialPTXReg "llvm.nvvm.read.ptx.sreg.lanemask.ge"


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
barrier f = void $ call (Body VoidType f) [NoUnwind, NoDuplicate, Convergent]


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


-- Atomic functions
-- ----------------

-- LLVM provides atomic instructions for integer arguments only. CUDA provides
-- additional support for atomic add on floating point types, which can be
-- accessed through the following intrinsics.
--
-- Double precision is only supported on Compute 6.0 devices and later. LLVM-4.0
-- currently lacks support for this intrinsic, however it may be possible to use
-- inline assembly.
--
-- <https://github.com/AccelerateHS/accelerate/issues/363>
--
atomicAdd_f :: FloatingType a -> Operand (Ptr a) -> Operand a -> CodeGen ()
atomicAdd_f t addr val =
  let
      width :: Int
      width =
        case t of
          TypeFloat{}   -> 32
          TypeDouble{}  -> 64
          TypeCFloat{}  -> 32
          TypeCDouble{} -> 64

      addrspace :: Word32
      (t_addr, t_val, addrspace) =
        case typeOf addr of
          PrimType ta@(PtrPrimType (ScalarPrimType tv) (AddrSpace as))
            -> (ta, tv, as)
          _ -> $internalError "atomicAdd" "unexpected operand type"

      t_ret = PrimType (ScalarPrimType t_val)
      fun   = Label $ printf "llvm.nvvm.atomic.load.add.f%d.p%df%d" width addrspace width
  in
  void $ call (Lam t_addr addr (Lam (ScalarPrimType t_val) val (Body t_ret fun))) [NoUnwind]


-- Shared memory
-- -------------

sharedMemAddrSpace :: AddrSpace
sharedMemAddrSpace = AddrSpace 3

sharedMemVolatility :: Volatility
sharedMemVolatility = Volatile


-- Declare a new statically allocated array in the __shared__ memory address
-- space, with enough storage to contain the given number of elements.
--
staticSharedMem
    :: forall e. Elt e
    => Word64
    -> CodeGen (IRArray (Vector e))
staticSharedMem n = do
  ad    <- go (eltType (undefined::e))
  return $ IRArray { irArrayShape      = IR (OP_Pair OP_Unit (OP_Int (integral integralType (P.fromIntegral n))))
                   , irArrayData       = IR ad
                   , irArrayAddrSpace  = sharedMemAddrSpace
                   , irArrayVolatility = sharedMemVolatility
                   }
  where
    go :: TupleType s -> CodeGen (Operands s)
    go UnitTuple          = return OP_Unit
    go (PairTuple t1 t2)  = OP_Pair <$> go t1 <*> go t2
    go tt@(SingleTuple t) = do
      -- Declare a new global reference for the statically allocated array
      -- located in the __shared__ memory space.
      nm <- freshName
      sm <- return $ ConstantOperand $ GlobalReference (PrimType (PtrPrimType (ArrayType n t) sharedMemAddrSpace)) nm
      declare $ LLVM.globalVariableDefaults
        { LLVM.addrSpace = sharedMemAddrSpace
        , LLVM.type'     = LLVM.ArrayType n (downcast t)
        , LLVM.linkage   = LLVM.Internal
        , LLVM.name      = downcast nm
        , LLVM.alignment = 4 `P.max` P.fromIntegral (sizeOf tt)
        }

      -- Return a pointer to the first element of the __shared__ memory array.
      -- We do this rather than just returning the global reference directly due
      -- to how __shared__ memory needs to be indexed with the GEP instruction.
      p <- instr' $ GetElementPtr sm [num numType 0, num numType 0 :: Operand Int32]
      q <- instr' $ PtrCast (PtrPrimType (ScalarPrimType t) sharedMemAddrSpace) p

      return $ ir' t (unPtr q)


-- External declaration in shared memory address space. This must be declared in
-- order to access memory allocated dynamically by the CUDA driver. This results
-- in the following global declaration:
--
-- > @__shared__ = external addrspace(3) global [0 x i8]
--
initialiseDynamicSharedMemory :: CodeGen (Operand (Ptr Word8))
initialiseDynamicSharedMemory = do
  declare $ LLVM.globalVariableDefaults
    { LLVM.addrSpace = sharedMemAddrSpace
    , LLVM.type'     = LLVM.ArrayType 0 (LLVM.IntegerType 8)
    , LLVM.linkage   = LLVM.External
    , LLVM.name      = LLVM.Name "__shared__"
    , LLVM.alignment = 4
    }
  return $ ConstantOperand $ GlobalReference type' "__shared__"


-- Declared a new dynamically allocated array in the __shared__ memory space
-- with enough space to contain the given number of elements.
--
dynamicSharedMem
    :: forall e int. (Elt e, IsIntegral int)
    => IR int                                 -- number of array elements
    -> IR int                                 -- #bytes of shared memory the have already been allocated
    -> CodeGen (IRArray (Vector e))
dynamicSharedMem n@(op integralType -> m) (op integralType -> offset) = do
  smem <- initialiseDynamicSharedMemory
  let
      go :: TupleType s -> Operand int -> CodeGen (Operand int, Operands s)
      go UnitTuple         i  = return (i, OP_Unit)
      go (PairTuple t2 t1) i0 = do
        (i1, p1) <- go t1 i0
        (i2, p2) <- go t2 i1
        return $ (i2, OP_Pair p2 p1)
      go (SingleTuple t)   i  = do
        p <- instr' $ GetElementPtr smem [num numType 0, i] -- TLM: note initial zero index!!
        q <- instr' $ PtrCast (PtrPrimType (ScalarPrimType t) sharedMemAddrSpace) p
        a <- instr' $ Mul numType m (integral integralType (P.fromIntegral (sizeOf (SingleTuple t))))
        b <- instr' $ Add numType i a
        return (b, ir' t (unPtr q))
  --
  (_, ad) <- go (eltType (undefined::e)) offset
  IR sz   <- A.fromIntegral integralType (numType :: NumType Int) n
  return   $ IRArray { irArrayShape      = IR $ OP_Pair OP_Unit sz
                     , irArrayData       = IR ad
                     , irArrayAddrSpace  = sharedMemAddrSpace
                     , irArrayVolatility = sharedMemVolatility
                     }


-- Global kernel definitions
-- -------------------------

data instance KernelMetadata PTX = KM_PTX LaunchConfig

-- | Combine kernels into a single program
--
(+++) :: IROpenAcc PTX aenv a -> IROpenAcc PTX aenv a -> IROpenAcc PTX aenv a
IROpenAcc k1 +++ IROpenAcc k2 = IROpenAcc (k1 ++ k2)


-- | Create a single kernel program with the default launch configuration.
--
makeOpenAcc
    :: PTX
    -> Label
    -> [LLVM.Parameter]
    -> CodeGen ()
    -> CodeGen (IROpenAcc PTX aenv a)
makeOpenAcc (deviceProperties . ptxContext -> dev) =
  makeOpenAccWith (simpleLaunchConfig dev)

-- | Create a single kernel program with the given launch analysis information.
--
makeOpenAccWith
    :: LaunchConfig
    -> Label
    -> [LLVM.Parameter]
    -> CodeGen ()
    -> CodeGen (IROpenAcc PTX aenv a)
makeOpenAccWith config name param kernel = do
  body  <- makeKernel config name param kernel
  return $ IROpenAcc [body]

-- | Create a complete kernel function by running the code generation process
-- specified in the final parameter.
--
makeKernel :: LaunchConfig -> Label -> [LLVM.Parameter] -> CodeGen () -> CodeGen (Kernel PTX aenv a)
makeKernel config name@(Label l) param kernel = do
  _    <- kernel
  code <- createBlocks
  addMetadata "nvvm.annotations"
    [ Just . MetadataOperand       $ ConstantOperand (GlobalReference VoidType (Name l))
    , Just . MetadataStringOperand $ "kernel"
    , Just . MetadataOperand       $ scalar scalarType (1::Int)
    ]
  return $ Kernel
    { kernelMetadata = KM_PTX config
    , unKernel       = LLVM.functionDefaults
                     { LLVM.returnType  = LLVM.VoidType
                     , LLVM.name        = downcast name
                     , LLVM.parameters  = (param, False)
                     , LLVM.basicBlocks = code
                     }
    }

