{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Permute
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Permute
  where

-- accelerate
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

import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )
import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Atomic
import LLVM.AST.Type.Instruction.RMW                                as RMW
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Representation

import Control.Applicative
import Control.Monad                                                ( void )
import Data.Typeable
import Prelude


-- Forward permutation specified by an indexing mapping. The resulting array is
-- initialised with the given defaults, and any further values that are permuted
-- into the result array are added to the current value using the combination
-- function.
--
-- The combination function must be /associative/ and /commutative/. Elements
-- that are mapped to the magic index 'ignore' are dropped.
--
mkPermute
    :: (Shape sh, Shape sh', Elt e)
    => Gamma aenv
    -> IRPermuteFun Native aenv (e -> e -> e)
    -> IRFun1       Native aenv (sh -> sh')
    -> IRDelayed    Native aenv (Array sh e)
    -> CodeGen (IROpenAcc Native aenv (Array sh' e))
mkPermute aenv combine project arr =
  (+++) <$> mkPermuteS aenv combine project arr
        <*> mkPermuteP aenv combine project arr


-- Forward permutation which does not require locking the output array. This
-- could be because we are executing sequentially with a single thread, or
-- because the default values are unused (e.g. for a filter).
--
-- We could also use this method if we can prove that the mapping function is
-- injective (distinct elements in the domain map to distinct elements in the
-- co-domain).
--
mkPermuteS
    :: forall aenv sh sh' e. (Shape sh, Shape sh', Elt e)
    => Gamma aenv
    -> IRPermuteFun Native aenv (e -> e -> e)
    -> IRFun1       Native aenv (sh -> sh')
    -> IRDelayed    Native aenv (Array sh e)
    -> CodeGen (IROpenAcc Native aenv (Array sh' e))
mkPermuteS aenv IRPermuteFun{..} project IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh' e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "permuteS" (paramGang ++ paramOut ++ paramEnv) $ do

    sh <- delayedExtent

    imapFromTo start end $ \i -> do

      ix  <- indexOfInt sh i
      ix' <- app1 project ix

      unless (ignore ix') $ do
        j <- intOfIndex (irArrayShape arrOut) ix'

        -- project element onto the destination array and update
        x <- app1 delayedLinearIndex i
        y <- readArray arrOut j
        r <- app2 combine x y

        writeArray arrOut j r

    return_


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
mkPermuteP
    :: forall aenv sh sh' e. (Shape sh, Shape sh', Elt e)
    => Gamma aenv
    -> IRPermuteFun Native aenv (e -> e -> e)
    -> IRFun1       Native aenv (sh -> sh')
    -> IRDelayed    Native aenv (Array sh e)
    -> CodeGen (IROpenAcc Native aenv (Array sh' e))
mkPermuteP aenv IRPermuteFun{..} project arr =
  case atomicRMW of
    Nothing       -> mkPermuteP_mutex aenv combine project arr
    Just (rmw, f) -> mkPermuteP_rmw   aenv rmw f   project arr


-- Parallel forward permutation function which uses atomic instructions to
-- implement lock-free array updates.
--
mkPermuteP_rmw
    :: forall aenv sh sh' e. (Shape sh, Shape sh', Elt e)
    => Gamma aenv
    -> RMWOperation
    -> IRFun1    Native aenv (e -> e)
    -> IRFun1    Native aenv (sh -> sh')
    -> IRDelayed Native aenv (Array sh e)
    -> CodeGen (IROpenAcc Native aenv (Array sh' e))
mkPermuteP_rmw aenv rmw update project IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh' e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "permuteP_rmw" (paramGang ++ paramOut ++ paramEnv) $ do

    sh <- delayedExtent

    imapFromTo start end $ \i -> do

      ix  <- indexOfInt sh i
      ix' <- app1 project ix

      unless (ignore ix') $ do
        j <- intOfIndex (irArrayShape arrOut) ix'
        x <- app1 delayedLinearIndex i
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
                  case s of
                    NumScalarType (IntegralNumType t) -> void . instr' $ AtomicRMW t NonVolatile rmw addr (op t r') (CrossThread, AcquireRelease)
                    NumScalarType t | RMW.Add <- rmw  -> atomicCAS_rmw s (A.add t r') addr
                    NumScalarType t | RMW.Sub <- rmw  -> atomicCAS_rmw s (A.sub t r') addr
                    _ -> case rmw of
                           RMW.Min                    -> atomicCAS_cmp s A.lt addr (op s r')
                           RMW.Max                    -> atomicCAS_cmp s A.gt addr (op s r')
                           _                          -> $internalError "mkPermute_rmw" "unexpected transition"
          --
          _ -> $internalError "mkPermute_rmw" "unexpected transition"

    return_


-- Parallel forward permutation function which uses a spinlock to acquire
-- a mutex before updating the value at that location.
--
mkPermuteP_mutex
    :: forall aenv sh sh' e. (Shape sh, Shape sh', Elt e)
    => Gamma aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRFun1    Native aenv (sh -> sh')
    -> IRDelayed Native aenv (Array sh e)
    -> CodeGen (IROpenAcc Native aenv (Array sh' e))
mkPermuteP_mutex aenv combine project IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out"  :: Name (Array sh' e))
      (arrLock, paramLock)      = mutableArray ("lock" :: Name (Vector Word8))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "permuteP_mutex" (paramGang ++ paramOut ++ paramLock ++ paramEnv) $ do

    sh <- delayedExtent

    imapFromTo start end $ \i -> do

      ix  <- indexOfInt sh i
      ix' <- app1 project ix

      -- project element onto the destination array and (atomically) update
      unless (ignore ix') $ do
        j <- intOfIndex (irArrayShape arrOut) ix'
        x <- app1 delayedLinearIndex i

        atomically arrLock j $ do
          y <- readArray arrOut j
          r <- app2 combine x y
          writeArray arrOut j r

    return_


-- Atomically execute the critical section only when the lock at the given array
-- index is obtained. The thread spins waiting for the lock to be released and
-- there is no backoff strategy in case the lock is contended.
--
-- It is important that the thread loops trying to acquire the lock without
-- writing data anything until the lock value changes. Then, because of MESI
-- caching protocols there will be no bus traffic while the CPU waits for the
-- value to change.
--
-- <https://en.wikipedia.org/wiki/Spinlock#Significant_optimizations>
--
atomically
    :: IRArray (Vector Word8)
    -> IR Int
    -> CodeGen a
    -> CodeGen a
atomically barriers i action = do
  let
      lock      = integral integralType 1
      unlock    = integral integralType 0
      unlocked  = lift 0
  --
  spin <- newBlock "spinlock.entry"
  crit <- newBlock "spinlock.critical-section"
  exit <- newBlock "spinlock.exit"

  addr <- instr' $ GetElementPtr (asPtr defaultAddrSpace (op integralType (irArrayData barriers))) [op integralType i]
  _    <- br spin

  -- Atomically (attempt to) set the lock slot to the locked state. If the slot
  -- was unlocked we just acquired it, otherwise the state remains unchanged and
  -- we spin until it becomes available.
  setBlock spin
  old  <- instr $ AtomicRMW integralType NonVolatile Exchange addr lock   (CrossThread, Acquire)
  ok   <- A.eq scalarType old unlocked
  _    <- cbr ok crit spin

  -- We just acquired the lock; perform the critical section then release the
  -- lock and exit. For ("some") x86 processors, an unlocked MOV instruction
  -- could be used rather than the slower XCHG, due to subtle memory ordering
  -- rules.
  setBlock crit
  r    <- action
  _    <- instr $ AtomicRMW integralType NonVolatile Exchange addr unlock (CrossThread, Release)
  _    <- br exit

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

