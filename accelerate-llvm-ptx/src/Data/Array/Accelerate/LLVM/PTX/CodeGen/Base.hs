{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Base (

  -- Types
  DeviceProperties, KernelMetadata(..),

  -- Thread identifiers
  blockDim, gridDim, threadIdx, blockIdx, warpSize,
  gridSize, globalThreadIdx,

  -- Other intrinsics
  laneId, warpId,
  laneMask_eq, laneMask_lt, laneMask_le, laneMask_gt, laneMask_ge,
  atomicAdd_f,
  nanosleep,

  -- Barriers and synchronisation
  __syncthreads, __syncthreads_count, __syncthreads_and, __syncthreads_or,
  __syncwarp, __syncwarp_mask,
  __threadfence_block, __threadfence_grid,

  -- Shared memory
  staticSharedMem,
  dynamicSharedMem,
  sharedMemAddrSpace,

  -- Shuffles
  shfl_up, shfl_down,

  -- Kernel definitions
  (+++),
  makeOpenAcc, makeOpenAccWith,

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Ptr
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type

import Foreign.CUDA.Analysis                                        ( Compute(..), computeCapability )
import qualified Foreign.CUDA.Analysis                              as CUDA

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Function
import LLVM.AST.Type.InlineAssembly
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Metadata
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation
import qualified LLVM.AST.Constant                                  as LLVM hiding ( type' )
import qualified LLVM.AST.Global                                    as LLVM
import qualified LLVM.AST.Linkage                                   as LLVM
import qualified LLVM.AST.Name                                      as LLVM
import qualified LLVM.AST.Type                                      as LLVM

import Control.Applicative
import Control.Monad                                                ( void, (>=>) )
import Control.Monad.State                                          ( gets )
import Prelude                                                      as P
import qualified Foreign.CUDA.Driver.Utils
import Data.Array.Accelerate (Vec)

#if MIN_VERSION_llvm_hs(10,0,0)
import qualified LLVM.AST.Type.Instruction.RMW                      as RMW
import LLVM.AST.Type.Instruction.Atomic
#elif !MIN_VERSION_llvm_hs(9,0,0)
import Data.String
import Text.Printf
#endif


-- Thread identifiers
-- ------------------

-- | Read the builtin registers that store CUDA thread and grid identifiers
--
-- <https://github.com/llvm-mirror/llvm/blob/master/include/llvm/IR/IntrinsicsNVVM.td>
--
specialPTXReg :: Label -> CodeGen PTX (Operands Int32)
specialPTXReg f =
  call (Body type' (Just Tail) f) [NoUnwind, ReadNone]

blockDim, gridDim, threadIdx, blockIdx, warpSize :: CodeGen PTX (Operands Int32)
blockDim    = specialPTXReg "llvm.nvvm.read.ptx.sreg.ntid.x"
gridDim     = specialPTXReg "llvm.nvvm.read.ptx.sreg.nctaid.x"
threadIdx   = specialPTXReg "llvm.nvvm.read.ptx.sreg.tid.x"
blockIdx    = specialPTXReg "llvm.nvvm.read.ptx.sreg.ctaid.x"
warpSize    = specialPTXReg "llvm.nvvm.read.ptx.sreg.warpsize"

laneId :: CodeGen PTX (Operands Int32)
laneId      = specialPTXReg "llvm.nvvm.read.ptx.sreg.laneid"

laneMask_eq, laneMask_lt, laneMask_le, laneMask_gt, laneMask_ge :: CodeGen PTX (Operands Int32)
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
warpId :: CodeGen PTX (Operands Int32)
warpId = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  tid <- threadIdx
  A.quot integralType tid (A.liftInt32 (P.fromIntegral (CUDA.warpSize dev)))

_warpId :: CodeGen PTX (Operands Int32)
_warpId = specialPTXReg "llvm.ptx.read.warpid"


-- | The size of the thread grid
--
-- > gridDim.x * blockDim.x
--
gridSize :: CodeGen PTX (Operands Int32)
gridSize = do
  ncta  <- gridDim
  nt    <- blockDim
  mul numType ncta nt


-- | The global thread index
--
-- > blockDim.x * blockIdx.x + threadIdx.x
--
globalThreadIdx :: CodeGen PTX (Operands Int32)
globalThreadIdx = do
  ntid  <- blockDim
  ctaid <- blockIdx
  tid   <- threadIdx
  --
  u     <- mul numType ntid ctaid
  v     <- add numType tid u
  return v


{--
-- | Generate function parameters that will specify the first and last (linear)
-- index of the array this kernel should evaluate.
--
gangParam :: (Operands Int, Operands Int, [LLVM.Parameter])
gangParam =
  let start = "ix.start"
      end   = "ix.end"
  in
  (local start, local end, parameter start ++ parameter end )
--}


-- Barriers and synchronisation
-- ----------------------------

-- | Call a built-in CUDA synchronisation intrinsic
--
barrier :: Label -> CodeGen PTX ()
barrier f = void $ call (Body VoidType (Just Tail) f) [NoUnwind, NoDuplicate, Convergent]

barrier_op :: Label -> Operands Int32 -> CodeGen PTX (Operands Int32)
barrier_op f x = call (Lam primType (op integralType x) (Body type' (Just Tail) f)) [NoUnwind, NoDuplicate, Convergent]


-- | Wait until all threads in the thread block have reached this point, and all
-- global and shared memory accesses made by these threads prior to the
-- __syncthreads() are visible to all threads in the block.
--
-- <http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#synchronization-functions>
--
__syncthreads :: CodeGen PTX ()
__syncthreads = barrier "llvm.nvvm.barrier0"

-- | Identical to __syncthreads() with the additional feature that it returns
-- the number of threads in the block for which the predicate evaluates to
-- non-zero.
--
__syncthreads_count :: Operands Int32 -> CodeGen PTX (Operands Int32)
__syncthreads_count = barrier_op "llvm.nvvm.barrier0.popc"

-- | Identical to __syncthreads() with the additional feature that it returns
-- non-zero iff the predicate evaluates to non-zero for all threads in the
-- block.
--
__syncthreads_and :: Operands Int32 -> CodeGen PTX (Operands Int32)
__syncthreads_and = barrier_op "llvm.nvvm.barrier0.and"

-- | Identical to __syncthreads() with the additional feature that it returns
-- non-zero iff the predicate evaluates to non-zero for any thread in the block.
--
__syncthreads_or :: Operands Int32 -> CodeGen PTX (Operands Int32)
__syncthreads_or = barrier_op "llvm.nvvm.barrier0.or"


-- | Wait until all warp lanes have reached this point.
--
__syncwarp :: HasCallStack => CodeGen PTX ()
__syncwarp = __syncwarp_mask (liftWord32 0xffffffff)

-- | Wait until all warp lanes named in the mask have executed a __syncwarp()
-- with the same mask. All non-exited threads named in the mask must execute
-- a corresponding __syncwarp with the same mask, or the result is undefined.
--
-- This guarantees memory ordering among threads participating in the barrier.
--
-- Requires LLVM-6.0 or higher.
-- Only required for devices of SM7 and later.
--
__syncwarp_mask :: HasCallStack => Operands Word32 -> CodeGen PTX ()
__syncwarp_mask mask = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  if computeCapability dev < Compute 7 0
    then return ()
    else
#if !MIN_VERSION_llvm_hs(6,0,0)
         internalError "LLVM-6.0 or above is required for Volta devices and later"
#else
         void $ call (Lam primType (op primType mask) (Body VoidType (Just Tail) "llvm.nvvm.bar.warp.sync")) [NoUnwind, NoDuplicate, Convergent]
#endif


-- | Ensure that all writes to shared and global memory before the call to
-- __threadfence_block() are observed by all threads in the *block* of the
-- calling thread as occurring before all writes to shared and global memory
-- made by the calling thread after the call.
--
-- <http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#memory-fence-functions>
--
__threadfence_block :: CodeGen PTX ()
__threadfence_block = barrier "llvm.nvvm.membar.cta"


-- | As __threadfence_block(), but the synchronisation is for *all* thread blocks.
-- In CUDA this is known simply as __threadfence().
--
__threadfence_grid :: CodeGen PTX ()
__threadfence_grid = barrier "llvm.nvvm.membar.gl"


-- Atomic functions
-- ----------------

-- LLVM provides atomic instructions for integer arguments only. CUDA provides
-- additional support for atomic add on floating point types, which can be
-- accessed through the following intrinsics.
--
-- Double precision is supported on Compute 6.0 devices and later. Half
-- precision is supported on Compute 7.0 devices and later.
--
-- LLVM-4.0 currently lacks support for this intrinsic, however it is
-- accessible via inline assembly.
--
-- LLVM-9 integrated floating-point atomic operations into the AtomicRMW
-- instruction, but this functionality is missing from llvm-hs-9. We access
-- it via inline assembly..
--
-- <https://github.com/AccelerateHS/accelerate/issues/363>
--
atomicAdd_f :: HasCallStack => FloatingType a -> Operand (Ptr a) -> Operand a -> CodeGen PTX ()
atomicAdd_f t addr val =
#if MIN_VERSION_llvm_hs(10,0,0)
  void . instr' $ AtomicRMW (FloatingNumType t) NonVolatile RMW.FAdd addr val (CrossThread, AcquireRelease)
#else
  let
      _width :: Int
      _width =
        case t of
          TypeHalf    -> 16
          TypeFloat   -> 32
          TypeDouble  -> 64

      (t_addr, t_val, _addrspace) =
        case typeOf addr of
          PrimType ta@(PtrPrimType (ScalarPrimType tv) (AddrSpace as))
            -> (ta, tv, as)
          _ -> internalError "unexpected operand type"

      t_ret = PrimType (ScalarPrimType t_val)
#if MIN_VERSION_llvm_hs(9,0,0) || !MIN_VERSION_llvm_hs(6,0,0)
      asm   =
        case t of
          -- assuming .address_size 64
          TypeHalf   -> InlineAssembly "atom.add.noftz.f16  $0, [$1], $2;" "=c,l,c" True False ATTDialect
          TypeFloat  -> InlineAssembly "atom.global.add.f32 $0, [$1], $2;" "=f,l,f" True False ATTDialect
          TypeDouble -> InlineAssembly "atom.global.add.f64 $0, [$1], $2;" "=d,l,d" True False ATTDialect
  in
  void $ instr (Call (Lam t_addr addr (Lam (ScalarPrimType t_val) val (Body t_ret (Just Tail) (Left asm)))) [Right NoUnwind])
#else
      fun   = fromString $ printf "llvm.nvvm.atomic.load.add.f%d.p%df%d" _width (_addrspace :: Word32) _width
  in
  void $ call (Lam t_addr addr (Lam (ScalarPrimType t_val) val (Body t_ret (Just Tail) fun))) [NoUnwind]
#endif
#endif


-- Shuffles
-- ---------

-- | Each thread gets the value provided by lower threads
--
shfl_up :: TypeR a
        -> Operands a
        -> Operands Word32
        -> CodeGen PTX (Operands a)
shfl_up = shfl "up"

-- | Each thread gets the value provided by higher threads
--
shfl_down :: forall a. TypeR a
          -> Operands a
          -> Operands Word32
          -> CodeGen PTX (Operands a)
shfl_down  = shfl "down"

---------Unused options-------
-- Each of the shfl primitives also exists in ".p" form. This version returns, alongside the normal value,
-- a boolean. This could be used to check whether the source lane was within range, for example. We currently
-- do manual bounds-arithmetic to do this. Using the ".p" version might be faster in some cases, saving one or two instructions.

-- These two primitives are currently not used in the backend, but are available now.
-- shfl_xor takes a lane mask as argument. It XOR's that mask with the target lane index to get their source lane index.
-- note: I'm not 100% sure that 'bfly' is indeed the XOR version, it's more of a process of elimination :)

-- shfl_xor :: forall a. TypeR a
--          -> Operands a
--          -> Operands Word32
--          -> CodeGen PTX (Operands a)
-- shfl_xor = shfl "bfly"

-- shfl_idx takes an argument representing the source lane index.

-- shfl_idx :: forall a. TypeR a
--          -> Operands a
--          -> Operands Word32
--          -> CodeGen PTX (Operands a)
-- shfl_idx = shfl "idx"
----------------------------------

-------unexported shfl internals---------

shfl :: Label -> TypeR a -> Operands a -> Operands Word32 -> CodeGen PTX (Operands a)
shfl mode typer a delta = case typer of
  TupRunit -> return OP_Unit

  -- Shuffle both sides and rebuild.
  -- Note: this could be slightly inefficient, (Int16, Int16) can be done in 1 shuffle and this takes 2.
  TupRpair leftR rightR -> do
    let OP_Pair left right = a
    left' <- shfl mode leftR left delta
    right' <- shfl mode rightR right delta
    return (OP_Pair left' right')

  -- Cast types with =< 32 bits into one shuffle, and types with more into multiple shuffles
  TupRsingle sctyp -> case sctyp of
    SingleScalarType (NumSingleType x) -> case x of
      IntegralNumType TypeInt8   -> cast_shfl_cast_i a
      IntegralNumType TypeInt16  -> cast_shfl_cast_i a
      IntegralNumType TypeInt32  -> shfl_i32         a
      IntegralNumType TypeWord8  -> cast_shfl_cast_i a
      IntegralNumType TypeWord16 -> cast_shfl_cast_i a
      IntegralNumType TypeWord32 -> cast_shfl_cast_i a

      FloatingNumType TypeHalf   -> cast_shfl_cast_f16 a
      FloatingNumType TypeFloat  -> shfl_f32         a

      IntegralNumType TypeInt    -> shfl_64_bit a
      IntegralNumType TypeWord   -> shfl_64_bit a
      IntegralNumType TypeInt64  -> shfl_64_bit a
      IntegralNumType TypeWord64 -> shfl_64_bit a
      FloatingNumType TypeDouble -> shfl_64_bit a

    VectorScalarType vectyp -> shfl_vec vectyp a
  where
    shfl_i32 :: Operands Int32 -> CodeGen PTX (Operands Int32)
    shfl_i32 x = mk_shfl mode "i32" x delta

    shfl_f32 :: Operands Float -> CodeGen PTX (Operands Float)
    shfl_f32 x = mk_shfl mode "f32" x delta

    -- Shuffle a (<32) bit integral
    cast_shfl_cast_i :: (IsIntegral a, IsNum a) => Operands a -> CodeGen PTX (Operands a)
    cast_shfl_cast_i = A.fromIntegral integralType numType
                    >=> shfl_i32
                    >=> A.fromIntegral integralType numType

    -- Shuffle a 16 bit floating point number
    cast_shfl_cast_f16 :: Operands Half -> CodeGen PTX (Operands Half)
    cast_shfl_cast_f16 = bitcast scalarType scalarType
                      >=> cast_shfl_cast_i @Int16
                      >=> bitcast scalarType scalarType

    -- Shuffle a 64 bit thing
    shfl_64_bit :: IsScalar a => Operands a -> CodeGen PTX (Operands a)
    shfl_64_bit x = do
      let vectype = VectorScalarType (VectorType 2 (NumSingleType (IntegralNumType TypeInt32))) :: ScalarType (Vec 2 Int32)
      x' <- bitcast scalarType vectype x
      x'' <- shfl mode (TupRsingle vectype) x' delta
      bitcast vectype scalarType x''

    shfl_vec :: VectorType a -> Operands a -> CodeGen PTX (Operands a)
    shfl_vec (VectorType 0 _) x = return x
    shfl_vec (VectorType n t) x = go (P.fromIntegral n) x (VectorType n t)

    go :: Int32 -> Operands (Vec n a) -> VectorType (Vec n a) -> CodeGen PTX (Operands (Vec n a))
    go 0 x _ = return x
    go n x vt@(VectorType _ t) = do
      e <- instr $ ExtractElement (n-1) (op vt x)
      e' <- shfl mode (TupRsingle (SingleScalarType t)) e delta
      x' <- instr $ InsertElement (n-1) (op vt x) (op (SingleScalarType t) e')
      go (n-1) x' vt


-- Only works for 32-bit int or floats!!
mk_shfl :: (IsPrim a)
        => Label -- direction: ["up","down","bfly","idx"] (the latter two probably represent the "xor" and "get from an index" variants)
        -> Label -- the type, "i32" or "f32"
        -> Operands a                  -- value to give
        -> Operands Word32             -- delta
        -> CodeGen PTX (Operands a)   -- value received
mk_shfl mode typ val delta = do

  -- In CUDA, the default for the final parameter, width, is warpsize.
  -- Behind the scenes, all these instructions happen with the width parameter
  -- before they get passed into the actual shfl instruction, so we have to
  -- perform them here too. Alternatively, just passing '31' would maybe work
  -- and be slightly faster. I haven't tested yet how it would behave with '31'
  -- in small warps, nor what '0xffffffff' would do.
  -- -DB
  warpsize <- warpSize
  subOf32  <- sub         numType (liftInt32 32) warpsize
  shiftl8  <- shiftL integralType subOf32        (liftInt 8)
  width    <- case mode of
    "up"   -> return shiftl8
    "down" -> bor    integralType shiftl8        (liftInt32 31)
    _ -> error "find out what quirks other modes have by compiling them with LLVM"

  -- Starting CUDA 9.0, the normal `shfl` primitives are removed in favour of the newer `shfl_sync` ones:
  -- They behave the same, except they start with a 'mask' argument specifying which threads participate in the shuffle.
  -- Arguably, it might be better to branch on the minimum requirements for `shfl_sync`, or maybe even to branch
  -- on the compute version of the gpu, but I couldn't find exact version numbers for these. Clang also branches on CUDA 9.0.
  let sync = Foreign.CUDA.Driver.Utils.libraryVersion >= 9000

  (if sync
    then call . Lam primType (op primType $ liftWord32 0xffffffff) -- mask, 32 1s means all threads should participate in this shfl
    else call)
      (Lam    primType (op primType val)   $                       -- value to provide to other lanes
        Lam   primType (op primType delta) $                       -- offset in shfl_up/shfl_down, source lane, or mask to XOR
          Lam primType (op primType width) $                       -- width, see ~10 lines up. Optional in CUDA, required in LLVM
            Body (PrimType primType)
                  (Just Tail)
                  ("llvm.nvvm.shfl."                               -- name of the function, e.g. "llvm.nvvm.shfl.sync.up.i32"
                    <> (if sync then "sync." else "")              --                         or "llvm.nvvm.shfl.down.f32"
                    <> mode <> "." <> typ))
    [Convergent, InaccessibleMemOnly, NoUnwind]

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
    :: TypeR e
    -> Word64
    -> CodeGen PTX (IRArray (Vector e))
staticSharedMem tp n = do
  ad    <- go tp
  return $ IRArray { irArrayRepr       = ArrayR dim1 tp
                   , irArrayShape      = OP_Pair OP_Unit $ OP_Int $ integral integralType $ P.fromIntegral n
                   , irArrayData       = ad
                   , irArrayAddrSpace  = sharedMemAddrSpace
                   , irArrayVolatility = sharedMemVolatility
                   }
  where
    go :: TypeR s -> CodeGen PTX (Operands s)
    go TupRunit          = return OP_Unit
    go (TupRpair t1 t2)  = OP_Pair <$> go t1 <*> go t2
    go tt@(TupRsingle t) = do
      -- Declare a new global reference for the statically allocated array
      -- located in the __shared__ memory space.
      nm <- freshName
      sm <- return $ ConstantOperand $ GlobalReference (PrimType (PtrPrimType (ArrayPrimType n t) sharedMemAddrSpace)) nm
      declare $ LLVM.globalVariableDefaults
        { LLVM.addrSpace = sharedMemAddrSpace
        , LLVM.type'     = LLVM.ArrayType n (downcast t)
        , LLVM.linkage   = LLVM.External
        , LLVM.name      = downcast nm
        , LLVM.alignment = 4 `P.max` P.fromIntegral (bytesElt tt)
        }

      -- Return a pointer to the first element of the __shared__ memory array.
      -- We do this rather than just returning the global reference directly due
      -- to how __shared__ memory needs to be indexed with the GEP instruction.
      p <- instr' $ GetElementPtr sm [num numType 0, num numType 0 :: Operand Int32]
      q <- instr' $ PtrCast (PtrPrimType (ScalarPrimType t) sharedMemAddrSpace) p

      return $ ir t (unPtr q)


-- External declaration in shared memory address space. This must be declared in
-- order to access memory allocated dynamically by the CUDA driver. This results
-- in the following global declaration:
--
-- > @__shared__ = external addrspace(3) global [0 x i8]
--
initialiseDynamicSharedMemory :: CodeGen PTX (Operand (Ptr Word8))
initialiseDynamicSharedMemory = do
  declare $ LLVM.globalVariableDefaults
    { LLVM.addrSpace = sharedMemAddrSpace
    , LLVM.type'     = LLVM.ArrayType 0 (LLVM.IntegerType 8)
    , LLVM.linkage   = LLVM.External
    , LLVM.name      = LLVM.Name "__shared__"
    , LLVM.alignment = 4
    }
  return $ ConstantOperand $ GlobalReference (PrimType (PtrPrimType (ArrayPrimType 0 scalarType) sharedMemAddrSpace)) "__shared__"


-- Declared a new dynamically allocated array in the __shared__ memory space
-- with enough space to contain the given number of elements.
--
dynamicSharedMem
    :: forall e int.
       TypeR e
    -> IntegralType int
    -> Operands int                                 -- number of array elements
    -> Operands int                                 -- #bytes of shared memory the have already been allocated
    -> CodeGen PTX (IRArray (Vector e))
dynamicSharedMem tp int n@(op int -> m) (op int -> offset)
  | IntegralDict <- integralDict int = do
    smem         <- initialiseDynamicSharedMemory
    let
        numTp = IntegralNumType int

        go :: TypeR s -> Operand int -> CodeGen PTX (Operand int, Operands s)
        go TupRunit         i  = return (i, OP_Unit)
        go (TupRpair t2 t1) i0 = do
          (i1, p1) <- go t1 i0
          (i2, p2) <- go t2 i1
          return $ (i2, OP_Pair p2 p1)
        go (TupRsingle t)   i  = do
          p <- instr' $ GetElementPtr smem [num numTp 0, i] -- TLM: note initial zero index!!
          q <- instr' $ PtrCast (PtrPrimType (ScalarPrimType t) sharedMemAddrSpace) p
          a <- instr' $ Mul numTp m (integral int (P.fromIntegral (bytesElt (TupRsingle t))))
          b <- instr' $ Add numTp i a
          return (b, ir t (unPtr q))
    --
    (_, ad) <- go tp offset
    sz      <- A.fromIntegral int (numType :: NumType Int) n
    return   $ IRArray { irArrayRepr       = ArrayR dim1 tp
                       , irArrayShape      = OP_Pair OP_Unit sz
                       , irArrayData       = ad
                       , irArrayAddrSpace  = sharedMemAddrSpace
                       , irArrayVolatility = sharedMemVolatility
                       }


-- Other functions
-- ---------------

-- Sleep the thread for (approximately) the given number of nanoseconds.
-- Requires compute capability >= 7.0
--
nanosleep :: Operands Int32 -> CodeGen PTX ()
nanosleep ns =
  let
      attrs = [NoUnwind, Convergent]
      asm   = InlineAssembly "nanosleep.u32 $0;" "r" True False ATTDialect
  in
  void $ instr (Call (Lam primType (op integralType ns) (Body VoidType (Just Tail) (Left asm))) (map Right attrs))


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
    :: Label
    -> [LLVM.Parameter]
    -> CodeGen PTX ()
    -> CodeGen PTX (IROpenAcc PTX aenv a)
makeOpenAcc name param kernel = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  makeOpenAccWith (simpleLaunchConfig dev) name param kernel

-- | Create a single kernel program with the given launch analysis information.
--
makeOpenAccWith
    :: LaunchConfig
    -> Label
    -> [LLVM.Parameter]
    -> CodeGen PTX ()
    -> CodeGen PTX (IROpenAcc PTX aenv a)
makeOpenAccWith config name param kernel = do
  body  <- makeKernel config name param kernel
  return $ IROpenAcc [body]

-- | Create a complete kernel function by running the code generation process
-- specified in the final parameter.
--
makeKernel
    :: LaunchConfig
    -> Label
    -> [LLVM.Parameter]
    -> CodeGen PTX ()
    -> CodeGen PTX (Kernel PTX aenv a)
makeKernel config name@(Label l) param kernel = do
  _    <- kernel
  code <- createBlocks
  addMetadata "nvvm.annotations"
    [ Just . MetadataConstantOperand $ LLVM.GlobalReference (LLVM.PointerType (LLVM.FunctionType LLVM.VoidType [ t | LLVM.Parameter t _ _ <- param ] False) (AddrSpace 0)) (LLVM.Name l)
    , Just . MetadataStringOperand   $ "kernel"
    , Just . MetadataConstantOperand $ LLVM.Int 32 1
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

