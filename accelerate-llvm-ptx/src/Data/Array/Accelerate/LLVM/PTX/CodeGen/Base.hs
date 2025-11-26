{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-orphans     #-}
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

  -- Shared memory
  staticSharedMem,
  sharedMemorySizeAdd,
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

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Function
import LLVM.AST.Type.GetElementPtr
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Atomic
import qualified LLVM.AST.Type.Instruction.RMW                      as RMW
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Metadata
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation
import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty     as LP

import Control.Applicative
import Control.Monad                                                ( void )
import Control.Monad.Reader                                         ( asks )
import Data.Bits
import Data.Proxy
import Data.String
import Foreign.Storable
import Prelude                                                      as P

import GHC.TypeLits



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
  dev <- liftCodeGen $ asks ptxDeviceProperties
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
  llvmver <- getLLVMversion
  dev <- liftCodeGen $ asks ptxDeviceProperties
  case (computeCapability dev >= Compute 7 0, llvmver >= 6) of
    (True, True) -> void $ call (Lam primType (op primType mask) (Body VoidType (Just Tail) "llvm.nvvm.bar.warp.sync")) [NoUnwind, NoDuplicate, Convergent]
    (True, False) -> internalError "LLVM-6.0 or above is required for Volta devices and later"
    (False, _) -> return ()


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
atomicAdd_f t addr val = do
  llvmver <- getLLVMversion
  if | llvmver >= 10 ->
         void . instr' $ AtomicRMW (FloatingNumType t) NonVolatile RMW.Add addr val (CrossThread, AcquireRelease)

     | otherwise ->
         internalError "LLVM < 10 not supported"


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
              let raw :: LP.Type -> LP.Instr -> CodeGen PTX (LP.Typed LP.Value)
                  raw ty ins = do
                    name <- freshLocalName
                    instr_ (LP.Result (nameToPrettyI name) ins [] [])
                    return (LP.Typed ty (LP.ValIdent (nameToPrettyI name)))

                  rawUp :: Type u -> LP.Instr -> CodeGen PTX (Operand u)
                  rawUp ty ins = do
                    name <- freshLocalName
                    instr_ (LP.Result (nameToPrettyI name) ins [] [])
                    return (LocalReference ty name)


                  vec :: forall m. KnownNat m => Proxy m -> CodeGen PTX (Operands (Vec n s))
                  vec _ = do
                    let t0Up :: Type (Vec n s)
                        t0Up = PrimType (ScalarPrimType (VectorScalarType v))
                        t0 = downcast t0Up

                        t1 = LP.PrimType (LP.Integer (P.fromIntegral ((w*bytes) * 8)))
                        t2 = LP.PrimType (LP.Integer (P.fromIntegral ((m+1) * 4 * 8)))

                        v' :: VectorType (Vec m Int32)
                        v' = VectorType (m+1) (singleType @Int32)
                        t3Up :: Type (Vec m Int32)
                        t3Up = PrimType (ScalarPrimType (VectorScalarType v'))
                        t3 = downcast t3Up

                    b <- raw t1 (LP.Conv LP.BitCast (downcast (op v a)) t1)
                    c <- raw t2 (LP.Conv (LP.ZExt False) b t2)
                    d <- rawUp t3Up (LP.Conv LP.BitCast c t3)
                    e <- vector v' (ir v' d)
                    f <- raw t2 (LP.Conv LP.BitCast (downcast (op v' e)) t2)
                    g <- raw t1 (LP.Conv (LP.Trunc False False) f t1)
                    h <- rawUp t0Up (LP.Conv LP.BitCast g t0)
                    return (ir v h)
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
  dev <- liftCodeGen $ asks ptxDeviceProperties

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
-- Previously, like initialiseDynamicSharedMemory, this function declared an
-- external global, e.g. for 1 i64:
--   @sdata = external addrspace(3) global [1 x i64], align 8
-- This would correspond to the following CUDA source:
--   extern __shared__ int64_t sdata[1];
--
-- But this CUDA C++ is rejected by Clang. When LLVM is fed LLVM IR, however,
-- things are more subtle; in the old llvm-hs backend where we linked against
-- LLVM, with LLVM 15, the above IR (defining @0) was accepted. However,
-- passing this same IR to Clang 18 with the llvm-pretty backend (yes I'm aware
-- the clang version is also changing here), clang first calls ptxas and then
-- nvlink; nvlink complains:
--   Undefined reference to 'sdata' in '/tmp/test-409abe.cubin'
-- When linking against LLVM 15, nvlink is never invoked, but instead ptxas is
-- _not_ given the -c flag and it immediately produces a SASS file.
--
-- Because Clang doesn't even accept the corresponding C++ code, but does
-- accept this:
--   __shared__ int64_t sdata[1];
-- the global created in this function was changed to be of internal linkage
-- instead. The assigned value is 'undef', just like what Clang generates for
-- the internal sdata C++ declaration.
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
      let arrt = ArrayPrimType n t
          ptrarrt = PrimType (PtrPrimType arrt sharedMemAddrSpace)
      sm <- return $ ConstantOperand $ GlobalReference ptrarrt nm
      declareGlobalVar $ LP.Global
        { LP.globalSym = nameToPrettyS nm
        , LP.globalAttrs = LP.GlobalAttrs
            { LP.gaLinkage = Just LP.Internal
            , LP.gaVisibility = Nothing
            , LP.gaAddrSpace = sharedMemAddrSpace
            , LP.gaConstant = False }
        , LP.globalType = LP.Array n (downcast t)
        , LP.globalValue = Just LP.ValUndef
        , LP.globalAlign = Just (4 `P.max` P.fromIntegral (bytesElt tt))
        , LP.globalMetadata = mempty
        }

      -- Return a pointer to the first element of the __shared__ memory array.
      -- We do this rather than just returning the global reference directly due
      -- to how __shared__ memory needs to be indexed with the GEP instruction.
      p <- instr' $ GetElementPtr (GEP (PrimType arrt) sm (A.num numType 0 :: Operand Int32)
                                       (GEPArray (A.num numType 0 :: Operand Int32) (GEPEmpty (ScalarPrimType t))))
      q <- instr' $ PtrCast (PtrPrimType (ScalarPrimType t) sharedMemAddrSpace) p

      return $ ir t (unPtr q)


-- External declaration in shared memory address space. This must be declared in
-- order to access memory allocated dynamically by the CUDA driver. This results
-- in the following global declaration:
--
-- > @__shared__ = external addrspace(3) global [0 x i8]
--
initialiseDynamicSharedMemory :: CodeGen PTX (Operand (Ptr Int8))
initialiseDynamicSharedMemory = do
  declareGlobalVar $ LP.Global
    { LP.globalSym = LP.Symbol "__shared__"
    , LP.globalAttrs = LP.GlobalAttrs
        { LP.gaLinkage = Just LP.External
        , LP.gaVisibility = Nothing
        , LP.gaAddrSpace = sharedMemAddrSpace
        , LP.gaConstant = False }
    , LP.globalType = LP.Array 0 (LP.PrimType (LP.Integer 8))
    , LP.globalValue = Nothing
    , LP.globalAlign = Nothing
    , LP.globalMetadata = mempty
    }
  return $ ConstantOperand
    $ ConstantGetElementPtr (GEP (PrimType (ArrayPrimType 0 (scalarType @Int8)))
                                 (GlobalReference (PrimType (PtrPrimType (ArrayPrimType 0 scalarType) sharedMemAddrSpace)) "__shared__")
                                 (ScalarConstant (scalarType @Int32) 0)
                                 (GEPArray (ScalarConstant (scalarType @Int32) 0) (GEPEmpty primType)))

sharedMemorySizeAdd
  :: TypeR e
  -> Int -- number of array elements
  -> Int -- #bytes of shared memory the have already been allocated
  -> Int
sharedMemorySizeAdd tp n i = case tp of
  TupRunit -> i
  TupRpair t2 t1 ->
    -- First handle the second element of the tuple, then the first,
    -- to match the behaviour of dynamicSharedMem
    sharedMemorySizeAdd t2 n $ sharedMemorySizeAdd t1 n i
  TupRsingle t ->
    let
      bytes = bytesElt tp
      -- Align 'i' to the alignment of t
      aligned = alignTo (scalarAlignment t) i
    in
      aligned + bytes * n

-- Declare a new dynamically allocated array in the __shared__ memory space
-- with enough space to contain the given number of elements.
--
dynamicSharedMem
    :: forall e int.
       TypeR e
    -> IntegralType int
    -> Operands int                                 -- number of array elements
    -> Operands int                                 -- #bytes of shared memory that have already been allocated
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
          let bytes = bytesElt (TupRsingle t)
          let align = scalarAlignment t
          i' <- instr' $ Add numTp i (A.integral int $ P.fromIntegral $ align - 1)
          aligned <- instr' $ BAnd int i' (A.integral int $ P.fromIntegral $ Data.Bits.complement $ align - 1)
          p <- instr' $ GetElementPtr (GEP1 scalarType smem aligned)
          q <- instr' $ PtrCast (PtrPrimType (ScalarPrimType t) sharedMemAddrSpace) p
          a <- instr' $ Mul numTp m (A.integral int (P.fromIntegral bytes))
          b <- instr' $ Add numTp aligned a
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
nanosleep ns = do
  -- This is an acc prelude function because it requires inline assembly, and
  -- llvm-pretty does not yet support caling inline assembly snippets. Thus we
  -- manually wrap the assembly in an inlineable function and call that.
  let label = makeAccPreludeLabel "nanosleep"
  void $ instr (Call (Lam primType (op integralType ns) (Body VoidType (Just Tail) (Right label))))


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
    -> [LP.Typed LP.Ident]
    -> CodeGen PTX ()
    -> CodeGen PTX (IROpenAcc PTX aenv a)
makeOpenAcc uid name param kernel = do
  dev <- liftCodeGen $ asks ptxDeviceProperties
  makeOpenAccWith (simpleLaunchConfig dev) uid name param kernel

-- | Create a single kernel program with the given launch analysis information.
--
makeOpenAccWith
    :: LaunchConfig
    -> UID
    -> Label
    -> [LP.Typed LP.Ident]
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
    -> [LP.Typed LP.Ident]
    -> CodeGen PTX ()
    -> CodeGen PTX (Kernel PTX aenv a)
makeKernel config name param kernel = do
  _    <- kernel
  code <- createBlocks
  let define = LP.Define
        { LP.defLinkage    = Nothing
        , LP.defVisibility = Nothing
        , LP.defRetType    = LP.PrimType LP.Void
        , LP.defName       = labelToPrettyS name
        , LP.defArgs       = param
        , LP.defVarArgs    = False
        , LP.defAttrs      = []
        , LP.defSection    = Nothing
        , LP.defGC         = Nothing
        , LP.defBody       = code
        , LP.defMetadata   = mempty
        , LP.defComdat     = Nothing
        }
  addMetadata "nvvm.annotations"
    [ Just . MetadataConstantOperand
      $ LP.Typed (LP.defFunType define) (LP.ValSymbol (labelToPrettyS name))
    , Just . MetadataStringOperand   $ "kernel"
    , Just . MetadataConstantOperand $ LP.Typed (LP.PrimType (LP.Integer 32)) (LP.ValInteger 1)
    ]
  return $ Kernel
    { kernelMetadata = KM_PTX config
    , unKernel       = define
    }

scalarAlignment :: ScalarType t -> Int
scalarAlignment t@(SingleScalarType _) = bytesElt (TupRsingle t)
scalarAlignment (VectorScalarType (VectorType _ t)) = bytesElt (TupRsingle $ SingleScalarType t)

-- Align 'ptr' to the given alignment.
-- Assumes 'align' is a power of 2.
alignTo :: Int -> Int -> Int
alignTo align ptr = (ptr + align - 1) .&. Data.Bits.complement (align - 1)
