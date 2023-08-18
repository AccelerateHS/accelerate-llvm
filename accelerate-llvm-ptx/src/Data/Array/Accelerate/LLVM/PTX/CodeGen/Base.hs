{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

  -- Warp shuffle instructions
  __shfl_up, __shfl_down, __shfl_idx, __broadcast,
  canShfl,

  -- Shared memory
  staticSharedMem,
  dynamicSharedMem,
  sharedMemAddrSpace, sharedMemVolatility,

  -- Kernel definitions
  (+++),
  makeOpenAcc, makeOpenAccWith,

) where

import Data.Primitive.Vec
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Ptr
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache
import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import qualified Data.Array.Accelerate.LLVM.CodeGen.Constant        as A

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
import qualified LLVM.AST.Constant                                  as LLVM ( Constant(GlobalReference, Int) )
import qualified LLVM.AST.Global                                    as LLVM
import qualified LLVM.AST.Instruction                               as LLVM hiding ( type', alignment )
import qualified LLVM.AST.Linkage                                   as LLVM
import qualified LLVM.AST.Name                                      as LLVM
import qualified LLVM.AST.Operand                                   as LLVM ( Operand(..) )
import qualified LLVM.AST.Type                                      as LLVM

import Control.Applicative
import Control.Monad                                                ( void )
import Control.Monad.State                                          ( gets )
import Data.Bits
import Data.Proxy
import Data.String
import Foreign.Storable
import Formatting                                                   hiding ( bytes, int )
import Prelude                                                      as P

import GHC.TypeLits

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
  void . instr' $ AtomicRMW (FloatingNumType t) NonVolatile RMW.Add addr val (CrossThread, AcquireRelease)
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


-- Warp shuffle functions
-- ----------------------
--
-- Exchange a variable between threads within a warp. Requires compute
-- capability 3.0 or higher.
--
-- <https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#warp-shuffle-functions>
--
-- Each of the shfl primitives also exists in ".p" form. This version
-- returns, alongside the normal value, a boolean that returns whether the
-- source lane was in range. This could be useful when doing bounds checks
-- in folds and scans.
--

data ShuffleOp
  = Idx     -- ^ Direct copy from indexed lane
  | Up      -- ^ Copy from a lane with lower ID relative to the caller
  | Down    -- ^ Copy from a lane with higher ID relative to the caller
  | XOR     -- ^ Copy from a lane based on bitwise XOR of own lane ID

-- | Each thread gets the value provided by lower threads
--
__shfl_up :: TypeR a -> Operands a -> Operands Word32 -> CodeGen PTX (Operands a)
__shfl_up = shfl Up

-- | Each thread gets the value provided by higher threads
--
__shfl_down :: TypeR a -> Operands a -> Operands Word32 -> CodeGen PTX (Operands a)
__shfl_down  = shfl Down

-- | shfl_idx takes an argument representing the source lane index.
--
__shfl_idx :: TypeR a -> Operands a -> Operands Word32 -> CodeGen PTX (Operands a)
__shfl_idx = shfl Idx

-- | Distribute the value from lane 0 across the warp
--
__broadcast :: TypeR a -> Operands a -> CodeGen PTX (Operands a)
__broadcast aR a = __shfl_idx aR a (liftWord32 0)

-- Warp shuffle instructions are available for compute capability >= 3.0
--
canShfl :: DeviceProperties -> Bool
canShfl dev = CUDA.computeCapability dev >= Compute 3 0


shfl :: ShuffleOp
     -> TypeR a
     -> Operands a
     -> Operands Word32
     -> CodeGen PTX (Operands a)
shfl sop tR val delta = go tR val
  where
    delta' :: Operand Word32
    delta' = op integralType delta

    go :: TypeR s -> Operands s -> CodeGen PTX (Operands s)
    go TupRunit         OP_Unit       = return OP_Unit
    go (TupRpair aR bR) (OP_Pair a b) = OP_Pair <$> go aR a <*> go bR b
    go (TupRsingle t)   a             = scalar t a

    scalar :: ScalarType s -> Operands s -> CodeGen PTX (Operands s)
    scalar (SingleScalarType t) = single t
    scalar (VectorScalarType t) = vector t

    single :: SingleType s -> Operands s -> CodeGen PTX (Operands s)
    single (NumSingleType t) = num t

    vector :: forall n s. VectorType (Vec n s) -> Operands (Vec n s) -> CodeGen PTX (Operands (Vec n s))
    vector v@(VectorType w t) a
      | SingleDict <- singleDict t
      = let bytes = sizeOf (undefined::s)
            (m,r) = P.quotRem (w * bytes) 4

            withSomeNat :: Int -> (forall m. KnownNat m => Proxy m -> b) -> b
            withSomeNat n k =
              case someNatVal (toInteger n) of
                Nothing          -> error "Welcome to overthinkers club. The first rule of overthinkers club is: yet to be decided."
                Just (SomeNat p) -> k p
         in
         if r == 0
            -- bitcast into a <m x i32> vector
            -- special case for a single element vector
            then
              if m == 1
                 then do
                   b <- A.bitcast (VectorScalarType v) (scalarType @Int32) a
                   c <- integral (integralType @Int32) b
                   d <- A.bitcast scalarType (VectorScalarType v) c
                   return d

                 else
                   let
                       vec :: forall m. KnownNat m => Proxy m -> CodeGen PTX (Operands (Vec n s))
                       vec _ = do
                         let v' = VectorType m (singleType @Int32)

                         b <- A.bitcast (VectorScalarType v) (VectorScalarType v') a

                         let c = op v' b

                             repack :: Int32 -> CodeGen PTX (Operands (Vec m Int32))
                             repack 0 = return $ ir v' (A.undef (VectorScalarType v'))
                             repack i = do
                               d <- instr $ ExtractElement (i-1) c
                               e <- integral integralType d
                               f <- repack (i-1)
                               g <- instr $ InsertElement (i-1) (op v' f) (op integralType e)
                               return g

                         h <- repack (P.fromIntegral m)
                         i <- A.bitcast (VectorScalarType v') (VectorScalarType v) h
                         return i
                   in
                   withSomeNat m vec

            -- Round up to the next multiple of 32:
            --
            --   1. bitcast to an integer of the same number of bits: e.g. bitcast <3 x i16> i48
            --   2. extend that to the next multiple of 32: e.g. zext i48 i64
            --   3. bitcast to <m+1 x i32>: e.g. bitcast i64 <2 x i32>
            --
            else
              let raw :: LLVM.Type -> LLVM.Instruction -> CodeGen PTX LLVM.Operand
                  raw ty ins = do
                    name <- downcast <$> freshLocalName
                    instr_ (name LLVM.:= ins)
                    return (LLVM.LocalReference ty name)

                  md :: LLVM.InstructionMetadata
                  md = []

                  t0 = LLVM.VectorType { LLVM.nVectorElements = P.fromIntegral w, LLVM.elementType = downcast t }
                  t1 = LLVM.IntegerType { LLVM.typeBits = P.fromIntegral ((w*bytes) * 8) }
                  t2 = LLVM.IntegerType { LLVM.typeBits = P.fromIntegral ((m+1) * 4 * 8) }
                  t3 = LLVM.VectorType { LLVM.nVectorElements = P.fromIntegral (m+1), LLVM.elementType = LLVM.i32 }

                  vec :: forall m. KnownNat m => Proxy m -> CodeGen PTX (Operands (Vec n s))
                  vec _ = do
                    let
                        v' :: VectorType (Vec m Int32)
                        v' = VectorType (m+1) (singleType @Int32)

                        upcast :: Type u -> LLVM.Operand -> Operand u
                        upcast s (LLVM.LocalReference s' (LLVM.UnName x))
                          = internalCheck (bformat ("couldn't match expected type `" % formatType % "' with actual type `" % shown % "'") s s') (s' == downcast s)
                          $ LocalReference s (UnName x)
                        upcast _ _
                          = internalError "expected local reference"

                    b <- raw t1 (LLVM.BitCast (downcast (op v a)) t1 md)
                    c <- raw t2 (LLVM.ZExt b t2 md)
                    d <- raw t3 (LLVM.BitCast c t3 md)
                    e <- vector v' (ir v' (upcast (PrimType (ScalarPrimType (VectorScalarType v'))) d))
                    f <- raw t2 (LLVM.BitCast (downcast (op v' e)) t2 md)
                    g <- raw t1 (LLVM.Trunc f t1 md)
                    h <- raw t0 (LLVM.BitCast g t0 md)
                    return (ir v (upcast (PrimType (ScalarPrimType (VectorScalarType v))) h))
               in
               withSomeNat (m+1) vec

    num :: NumType s -> Operands s -> CodeGen PTX (Operands s)
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: forall s. IntegralType s -> Operands s -> CodeGen PTX (Operands s)
    integral TypeInt32 a = shfl_op sop ShuffleInt32 delta' a
    integral t         a
      | IntegralDict <- integralDict t
      = case finiteBitSize (undefined::s) of
          64 -> do
            let ta = SingleScalarType (NumSingleType (IntegralNumType t))
                tb = scalarType @(Vec 2 Int32)
            --
            b <- A.bitcast ta tb a
            c <- vector (VectorType 2 singleType) b
            d <- A.bitcast tb ta c
            return d

          _  -> do
            b <- A.fromIntegral t (numType @Int32) a
            c <- integral integralType b
            d <- A.fromIntegral integralType (IntegralNumType t) c
            return d

    floating :: FloatingType s -> Operands s -> CodeGen PTX (Operands s)
    floating TypeFloat  a = shfl_op sop ShuffleFloat delta' a
    floating TypeDouble a = do
      b <- A.bitcast scalarType (scalarType @(Vec 2 Int32)) a
      c <- vector (VectorType 2 singleType) b
      d <- A.bitcast scalarType (scalarType @Double) c
      return d
    floating TypeHalf   a = do
      b <- A.bitcast scalarType (scalarType @Int16) a
      c <- integral integralType b
      d <- A.bitcast scalarType (scalarType @Half) c
      return d


data ShuffleType a where
  ShuffleInt32 :: ShuffleType Int32
  ShuffleFloat :: ShuffleType Float

shfl_op
    :: forall a.
       ShuffleOp
    -> ShuffleType a
    -> Operand Word32               -- delta
    -> Operands a                   -- value to give
    -> CodeGen PTX (Operands a)     -- value received
shfl_op sop t delta val = do
  dev <- liftCodeGen $ gets ptxDeviceProperties

  let
      -- The CUDA __shfl* instruction take an optional final parameter
      -- which is the warp size. Setting this value to something (always
      -- a power-of-two) other than 32 emulates the shfl behaviour at that
      -- warp size. Behind the scenes, a bunch of instructions happen with
      -- this width parameter before they get passed into the actual shfl
      -- instruction. Here, we have to directly set them to the 'actual'
      -- width parameter. The formula that clang compiles to is in the
      -- comments
      --
      width :: Operand Int32
      width = case sop of
        Up   -> A.integral integralType 0  -- ((32 - warpSize) `shiftL` 8)
        Down -> A.integral integralType 31 -- ((32 - warpSize) `shiftL` 8) `or` 31
        Idx  -> A.integral integralType 31 -- ((32 - warpSize) `shiftL` 8) `or` 31
        XOR  -> A.integral integralType 31 -- ((32 - warpSize) `shiftL` 8) `or` 31

      -- Starting CUDA 9.0, the normal `shfl` primitives are deprecated in
      -- favour of the newer `shfl_sync` primitives. They behave the same
      -- way, except they start with a 'mask' argument specifying which
      -- threads participate in the shuffle.
      --
      mask :: Operand Int32
      mask  = A.integral integralType (-1) -- all threads participate

      useSyncShfl = CUDA.computeCapability dev >= Compute 7 0

      call' = if useSyncShfl
                 then call . Lam primType mask
                 else call

      sync  = if useSyncShfl then "sync." else ""
      asm   = "llvm.nvvm.shfl."
           <> sync
           <> case sop of
                Idx  -> "idx."
                Up   -> "up."
                Down -> "down."
                XOR  -> "bfly."
           <> case t of
                ShuffleInt32 -> "i32"
                ShuffleFloat -> "f32"

      t_val = case t of
                ShuffleInt32 -> primType :: PrimType Int32
                ShuffleFloat -> primType :: PrimType Float

  call' (Lam t_val (op t_val val) (Lam primType delta (Lam primType width (Body (PrimType t_val) (Just Tail) asm)))) [Convergent, NoUnwind, InaccessibleMemOnly]


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
                   , irArrayShape      = OP_Pair OP_Unit $ OP_Int $ A.integral integralType $ P.fromIntegral n
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
      nm <- freshGlobalName
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
#if MIN_VERSION_llvm_hs(15,0,0)
      p <- instr' $ GetElementPtr t sm [A.num numType 0 :: Operand Int32]
#else
      p <- instr' $ GetElementPtr t sm [A.num numType 0, A.num numType 0 :: Operand Int32]
#endif
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
#if MIN_VERSION_llvm_hs(15,0,0)
          p <- instr' $ GetElementPtr scalarType smem [i]
#else
          p <- instr' $ GetElementPtr scalarType smem [A.num numTp 0, i] -- TLM: note initial zero index!!
#endif
          q <- instr' $ PtrCast (PtrPrimType (ScalarPrimType t) sharedMemAddrSpace) p
          a <- instr' $ Mul numTp m (A.integral int (P.fromIntegral (bytesElt (TupRsingle t))))
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
    :: UID
    -> Label
    -> [LLVM.Parameter]
    -> CodeGen PTX ()
    -> CodeGen PTX (IROpenAcc PTX aenv a)
makeOpenAcc uid name param kernel = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  makeOpenAccWith (simpleLaunchConfig dev) uid name param kernel

-- | Create a single kernel program with the given launch analysis information.
--
makeOpenAccWith
    :: LaunchConfig
    -> UID
    -> Label
    -> [LLVM.Parameter]
    -> CodeGen PTX ()
    -> CodeGen PTX (IROpenAcc PTX aenv a)
makeOpenAccWith config uid name param kernel = do
  body  <- makeKernel config (name <> fromString ('_' : show uid)) param kernel
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
    [ Just . MetadataConstantOperand
      $ LLVM.GlobalReference
#if !MIN_VERSION_llvm_hs(15,0,0)
          (LLVM.PointerType (LLVM.FunctionType LLVM.VoidType [ t | LLVM.Parameter t _ _ <- param ] False) (AddrSpace 0))
#endif
          (LLVM.Name l)
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

