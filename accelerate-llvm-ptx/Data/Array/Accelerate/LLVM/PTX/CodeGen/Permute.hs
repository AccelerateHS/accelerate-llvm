{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Permute
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Permute (

  mkPermute,

) where

-- accelerate
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Sugar                            ( Array, Vector, Shape, Elt, eltType )
import Data.Array.Accelerate.Error
import qualified Data.Array.Accelerate.Array.Sugar                  as S

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Permute
import Data.Array.Accelerate.LLVM.CodeGen.Ptr
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Loop
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Target

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Atomic
import LLVM.AST.Type.Instruction.RMW                                as RMW
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import Foreign.CUDA.Analysis

import Data.Typeable
import Control.Monad                                                ( void )
import Prelude


-- Forward permutation specified by an indexing mapping. The resulting array is
-- initialised with the given defaults, and any further values that are permuted
-- into the result array are added to the current value using the combination
-- function.
--
-- The combination function must be /associative/ and /commutative/. Elements
-- that are mapped to the magic index 'ignore' are dropped.
--
-- Parallel forward permutation has to take special care because different
-- threads could concurrently try to update the same memory location. Where
-- available we make use of special atomic instructions and other optimisations,
-- but in the general case each element of the output array has a lock which
-- must be obtained by the thread before it can update that memory location.
--
-- TODO: After too many failures to acquire the lock on an element, the thread
-- should back off and try a different element, adding this failed element to
-- a queue or some such.
--
mkPermute
    :: forall aenv sh sh' e. (Shape sh, Shape sh', Elt e)
    => PTX
    -> Gamma aenv
    -> IRPermuteFun PTX aenv (e -> e -> e)
    -> IRFun1       PTX aenv (sh -> sh')
    -> IRDelayed    PTX aenv (Array sh e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh' e))
mkPermute ptx aenv IRPermuteFun{..} project arr =
  let
      bytes   = sizeOf (eltType (undefined :: e))
      sizeOk  = bytes == 4 || bytes == 8
  in
  case atomicRMW of
    Just (rmw, f) | sizeOk -> mkPermute_rmw   ptx aenv rmw f   project arr
    _                      -> mkPermute_mutex ptx aenv combine project arr


-- Parallel forward permutation function which uses atomic instructions to
-- implement lock-free array updates.
--
-- Atomic instruction support on CUDA devices is a bit patchy, so depending on
-- the element type and compute capability of the target hardware we may need to
-- emulate the operation using atomic compare-and-swap.
--
--              Int32    Int64    Float32    Float64
--           +----------------------------------------
--    (+)    |  2.0       2.0       2.0        6.0
--    (-)    |  2.0       2.0        x          x
--    (.&.)  |  2.0       3.2
--    (.|.)  |  2.0       3.2
--    xor    |  2.0       3.2
--    min    |  2.0       3.2        x          x
--    max    |  2.0       3.2        x          x
--    CAS    |  2.0       2.0
--
-- Note that NVPTX requires at least compute 2.0, so we can always implement the
-- lockfree update operations in terms of compare-and-swap.
--
mkPermute_rmw
    :: forall aenv sh sh' e. (Shape sh, Shape sh', Elt e)
    => PTX
    -> Gamma aenv
    -> RMWOperation
    -> IRFun1    PTX aenv (e -> e)
    -> IRFun1    PTX aenv (sh -> sh')
    -> IRDelayed PTX aenv (Array sh e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh' e))
mkPermute_rmw ptx@(deviceProperties . ptxContext -> dev) aenv rmw update project IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh' e))
      paramEnv                  = envParam aenv
      --
      bytes                     = sizeOf (eltType (undefined :: e))
      compute                   = computeCapability dev
      compute32                 = Compute 3 2
      -- compute60                 = Compute 6 0
  in
  makeOpenAcc ptx "permute_rmw" (paramGang ++ paramOut ++ paramEnv) $ do

    sh <- delayedExtent

    imapFromTo start end $ \i -> do

      i'  <- A.fromIntegral integralType numType i
      ix  <- indexOfInt sh i'
      ix' <- app1 project ix

      unless (ignore ix') $ do
        j <- intOfIndex (irArrayShape arrOut) ix'
        x <- app1 delayedLinearIndex i'
        r <- app1 update x

        case rmw of
          Exchange
            -> writeArray arrOut j r
          --
          _ | SingleTuple s <- eltType (undefined::e)
            , Just adata    <- gcast (irArrayData arrOut)
            , Just r'       <- gcast r
            -> do
                  addr <- instr' $ GetElementPtr (asPtr defaultAddrSpace (op s adata)) [op integralType j]
                  --
                  let
                      rmw_integral :: IntegralType t -> Operand (Ptr t) -> Operand t -> CodeGen ()
                      rmw_integral t ptr val
                        | primOk    = void . instr' $ AtomicRMW t NonVolatile rmw ptr val (CrossThread, AcquireRelease)
                        | otherwise =
                            case rmw of
                              RMW.And -> atomicCAS_rmw s' (A.band t (ir t val)) ptr
                              RMW.Or  -> atomicCAS_rmw s' (A.bor  t (ir t val)) ptr
                              RMW.Xor -> atomicCAS_rmw s' (A.xor  t (ir t val)) ptr
                              RMW.Min -> atomicCAS_cmp s' A.lt ptr val
                              RMW.Max -> atomicCAS_cmp s' A.gt ptr val
                              _       -> $internalError "mkPermute_rmw.integral" "unexpected transition"
                        where
                          s'      = NumScalarType (IntegralNumType t)
                          primOk  = compute >= compute32
                                 || bytes == 4
                                 || case rmw of
                                      RMW.Add -> True
                                      RMW.Sub -> True
                                      _       -> False

                      rmw_floating :: FloatingType t -> Operand (Ptr t) -> Operand t -> CodeGen ()
                      rmw_floating t ptr val =
                        case rmw of
                          RMW.Min       -> atomicCAS_cmp s' A.lt ptr val
                          RMW.Max       -> atomicCAS_cmp s' A.gt ptr val
                          RMW.Sub       -> atomicCAS_rmw s' (A.sub n (ir t val)) ptr
                          RMW.Add
                            | primAdd   -> atomicAdd_f t ptr val
                            | otherwise -> atomicCAS_rmw s' (A.add n (ir t val)) ptr
                          _             -> $internalError "mkPermute_rmw.floating" "unexpected transition"
                        where
                          n       = FloatingNumType t
                          s'      = NumScalarType n
                          primAdd = bytes == 4
                                 -- Disabling due to missing support from llvm-4.0.
                                 -- <https://github.com/AccelerateHS/accelerate/issues/363>
                                 -- compute >= compute60

                      rmw_nonnum :: NonNumType t -> Operand (Ptr t) -> Operand t -> CodeGen ()
                      rmw_nonnum TypeChar{} ptr val = do
                        ptr32 <- instr' $ PtrCast (primType   :: PrimType (Ptr Word32)) ptr
                        val32 <- instr' $ BitCast (scalarType :: ScalarType Word32)     val
                        void   $ instr' $ AtomicRMW (integralType :: IntegralType Word32) NonVolatile rmw ptr32 val32 (CrossThread, AcquireRelease)
                      rmw_nonnum _ _ _ = -- C character types are 8-bit, and thus not supported
                        $internalError "mkPermute_rmw.nonnum" "unexpected transition"
                  case s of
                    NumScalarType (IntegralNumType t) -> rmw_integral t addr (op t r')
                    NumScalarType (FloatingNumType t) -> rmw_floating t addr (op t r')
                    NonNumScalarType t                -> rmw_nonnum   t addr (op t r')
          --
          _ -> $internalError "mkPermute_rmw" "unexpected transition"

    return_


-- Parallel forward permutation function which uses a spinlock to acquire
-- a mutex before updating the value at that location.
--
mkPermute_mutex
    :: forall aenv sh sh' e. (Shape sh, Shape sh', Elt e)
    => PTX
    -> Gamma aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRFun1    PTX aenv (sh -> sh')
    -> IRDelayed PTX aenv (Array sh e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh' e))
mkPermute_mutex ptx aenv combine project IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out"  :: Name (Array sh' e))
      (arrLock, paramLock)      = mutableArray ("lock" :: Name (Vector Word32))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc ptx "permute_mutex" (paramGang ++ paramOut ++ paramLock ++ paramEnv) $ do

    sh <- delayedExtent

    imapFromTo start end $ \i -> do

      i'  <- A.fromIntegral integralType numType i
      ix  <- indexOfInt sh i'
      ix' <- app1 project ix

      -- project element onto the destination array and (atomically) update
      unless (ignore ix') $ do
        j <- intOfIndex (irArrayShape arrOut) ix'
        x <- app1 delayedLinearIndex i'

        atomically arrLock j $ do
          y <- readArray arrOut j
          r <- app2 combine x y
          writeArray arrOut j r

    return_


-- Atomically execute the critical section only when the lock at the given array
-- index is obtained. The thread spins waiting for the lock to be released and
-- there is no backoff strategy in case the lock is contended.
--
-- The canonical implementation of a spin-lock looks like this:
--
-- > do {
-- >   old = atomic_exchange(&lock[i], 1);
-- > } while (old == 1);
-- >
-- > /* critical section */
-- >
-- > atomic_exchange(&lock[i], 0);
--
-- The initial loop repeatedly attempts to take the lock by writing a 1 (locked)
-- into the lock slot. Once the 'old' state of the lock returns 0 (unlocked),
-- then we just acquired the lock and the atomic section can be computed.
-- Finally, the lock is released by writing 0 back to the lock slot.
--
-- However, there is a complication with CUDA devices because all threads in
-- a warp must execute in lockstep (with predicated execution). In the above
-- setup, once a thread acquires a lock, then it will be disabled and stop
-- participating in the loop, waiting for all other threads (to acquire their
-- locks) before continuing program execution. If two threads in the same warp
-- attempt to acquire the same lock, then once the lock is acquired by one
-- thread then it will sit idle waiting while the second thread spins attempting
-- to grab a lock that will never be released because the first thread (which
-- holds the lock) can not make progress. DEADLOCK.
--
-- To prevent this situation we must invert the algorithm so that threads can
-- always make progress, until each warp in the thread has committed their
-- result.
--
-- > done = 0;
-- > do {
-- >   if ( atomic_exchange(&lock[i], 1) == 0 ) {
-- >
-- >     /* critical section */
-- >
-- >     done = 1;
-- >     atomic_exchange(&lock[i], 0);
-- >   }
-- > } while ( done == 0 );
--
atomically
    :: IRArray (Vector Word32)
    -> IR Int
    -> CodeGen a
    -> CodeGen a
atomically barriers i action = do
  let
      lock    = integral integralType 1
      unlock  = integral integralType 0
      unlock' = lift 0
  --
  spin <- newBlock "spinlock.entry"
  crit <- newBlock "spinlock.critical-start"
  skip <- newBlock "spinlock.critical-end"
  exit <- newBlock "spinlock.exit"

  addr <- instr' $ GetElementPtr (asPtr defaultAddrSpace (op integralType (irArrayData barriers))) [op integralType i]
  _    <- br spin

  -- Loop until this thread has completed its critical section. If the slot was
  -- unlocked then we just acquired the lock and the thread can perform the
  -- critical section, otherwise skip to the bottom of the critical section.
  setBlock spin
  old  <- instr $ AtomicRMW integralType NonVolatile Exchange addr lock   (CrossThread, Acquire)
  ok   <- A.eq scalarType old unlock'
  no   <- cbr ok crit skip

  -- If we just acquired the lock, execute the critical section
  setBlock crit
  r    <- action
  _    <- instr $ AtomicRMW integralType NonVolatile Exchange addr unlock (CrossThread, Release)
  yes  <- br skip

  -- At the base of the critical section, threads participate in a memory fence
  -- to ensure the lock state is committed to memory. Depending on which
  -- incoming edge the thread arrived at this block from determines whether they
  -- have completed their critical section.
  setBlock skip
  done <- phi [(lift True, yes), (lift False, no)]

  __syncthreads
  _    <- cbr done exit spin

  setBlock exit
  return r


-- Helper functions
-- ----------------

-- Test whether the given index is the magic value 'ignore'. This operates
-- strictly rather than performing short-circuit (&&).
--
ignore :: forall ix. Shape ix => IR ix -> CodeGen (IR Bool)
ignore (IR ix) = go (S.eltType (undefined::ix)) (S.fromElt (S.ignore::ix)) ix
  where
    go :: TupleType t -> t -> Operands t -> CodeGen (IR Bool)
    go UnitTuple           ()          OP_Unit        = return (lift True)
    go (PairTuple tsh tsz) (ish, isz) (OP_Pair sh sz) = do x <- go tsh ish sh
                                                           y <- go tsz isz sz
                                                           land' x y
    go (SingleTuple t)     ig         sz              = A.eq t (ir t (scalar t ig)) (ir t (op' t sz))

