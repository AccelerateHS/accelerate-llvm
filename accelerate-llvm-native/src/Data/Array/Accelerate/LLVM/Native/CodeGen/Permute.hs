{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Permute
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Permute
  where

import Data.Array.Accelerate.AST                                    ( PrimMaybe )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type

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
import Data.Array.Accelerate.LLVM.Compile.Cache

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
    :: HasCallStack
    => UID
    -> Gamma               aenv
    -> ArrayR (Array sh e)
    -> ShapeR sh'
    -> IRPermuteFun Native aenv (e -> e -> e)
    -> IRFun1       Native aenv (sh -> PrimMaybe sh')
    -> MIRDelayed   Native aenv (Array sh e)
    -> CodeGen      Native      (IROpenAcc Native aenv (Array sh' e))
mkPermute uid aenv repr shr combine project arr =
  (+++) <$> mkPermuteS uid aenv repr shr combine project arr
        <*> mkPermuteP uid aenv repr shr combine project arr


-- Forward permutation which does not require locking the output array. This
-- could be because we are executing sequentially with a single thread, or
-- because the default values are unused (e.g. for a filter).
--
-- We could also use this method if we can prove that the mapping function is
-- injective (distinct elements in the domain map to distinct elements in the
-- co-domain).
--
mkPermuteS
    :: UID
    -> Gamma               aenv
    -> ArrayR (Array sh e)
    -> ShapeR sh'
    -> IRPermuteFun Native aenv (e -> e -> e)
    -> IRFun1       Native aenv (sh -> PrimMaybe sh')
    -> MIRDelayed   Native aenv (Array sh e)
    -> CodeGen      Native      (IROpenAcc Native aenv (Array sh' e))
mkPermuteS uid aenv repr shr IRPermuteFun{..} project marr =
  let
      (start, end, paramGang) = gangParam    (arrayRshape repr)
      (arrOut, paramOut)      = mutableArray (reprOut repr shr) "out"
      (arrIn,  paramIn)       = delayedArray "in" marr
      paramEnv                = envParam aenv
  in
  makeOpenAcc uid "permuteS" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do

    sh <- delayedExtent arrIn

    imapNestFromTo (arrayRshape repr) start end sh $ \ix _ -> do

      ix' <- app1 project ix

      when (isJust ix') $ do
        i <- fromJust ix'
        j <- intOfIndex shr (irArrayShape arrOut) i

        -- project element onto the destination array and update
        x <- app1 (delayedIndex arrIn) ix
        y <- readArray TypeInt arrOut j
        r <- app2 combine x y

        writeArray TypeInt arrOut j r

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
    :: HasCallStack
    => UID
    -> Gamma               aenv
    -> ArrayR (Array sh e)
    -> ShapeR sh'
    -> IRPermuteFun Native aenv (e -> e -> e)
    -> IRFun1       Native aenv (sh -> PrimMaybe sh')
    -> MIRDelayed   Native aenv (Array sh e)
    -> CodeGen      Native      (IROpenAcc Native aenv (Array sh' e))
mkPermuteP uid aenv repr shr IRPermuteFun{..} project arr =
  case atomicRMW of
    Nothing       -> mkPermuteP_mutex uid aenv repr shr combine project arr
    Just (rmw, f) -> mkPermuteP_rmw   uid aenv repr shr rmw f   project arr


-- Parallel forward permutation function which uses atomic instructions to
-- implement lock-free array updates.
--
mkPermuteP_rmw
    :: HasCallStack
    => UID
    -> Gamma aenv
    -> ArrayR (Array sh e)
    -> ShapeR sh'
    -> RMWOperation
    -> IRFun1     Native aenv (e -> e)
    -> IRFun1     Native aenv (sh -> PrimMaybe sh')
    -> MIRDelayed Native aenv (Array sh e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array sh' e))
mkPermuteP_rmw uid aenv repr shr rmw update project marr =
  let
      (start, end, paramGang) = gangParam    (arrayRshape repr)
      (arrOut, paramOut)      = mutableArray (reprOut repr shr) "out"
      (arrIn,  paramIn)       = delayedArray "in" marr
      paramEnv                = envParam aenv
  in
  makeOpenAcc uid "permuteP_rmw" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do

    sh <- delayedExtent arrIn

    imapNestFromTo (arrayRshape repr) start end sh $ \ix _ -> do

      ix' <- app1 project ix

      when (isJust ix') $ do
        i <- fromJust ix'
        j <- intOfIndex shr (irArrayShape arrOut) i
        x <- app1 (delayedIndex arrIn) ix
        r <- app1 update x

        case rmw of
          Exchange
            -> writeArray TypeInt arrOut j r
          --
          _ | TupRsingle (SingleScalarType s)   <- arrayRtype repr
            , adata                             <- irArrayData arrOut
            -> do
                  addr <- instr' $ GetElementPtr (asPtr defaultAddrSpace (op s adata)) [op integralType j]
                  --
                  case s of
#if MIN_VERSION_llvm_hs(10,0,0)
                    NumSingleType t             -> void . instr' $ AtomicRMW t NonVolatile rmw addr (op t r) (CrossThread, AcquireRelease)
#else
                    NumSingleType t
                      | IntegralNumType{} <- t  -> void . instr' $ AtomicRMW t NonVolatile rmw addr (op t r) (CrossThread, AcquireRelease)
                      | RMW.Add <- rmw          -> atomicCAS_rmw s (A.add t r) addr
                      | RMW.Sub <- rmw          -> atomicCAS_rmw s (A.sub t r) addr
#endif
                    _ | RMW.Min <- rmw          -> atomicCAS_cmp s A.lt addr (op s r)
                      | RMW.Max <- rmw          -> atomicCAS_cmp s A.gt addr (op s r)
                    _                           -> internalError "unexpected transition"
          --
          _ -> internalError "unexpected transition"

    return_


-- Parallel forward permutation function which uses a spinlock to acquire
-- a mutex before updating the value at that location.
--
mkPermuteP_mutex
    :: UID
    -> Gamma             aenv
    -> ArrayR (Array sh e)
    -> ShapeR sh'
    -> IRFun2     Native aenv (e -> e -> e)
    -> IRFun1     Native aenv (sh -> PrimMaybe sh')
    -> MIRDelayed Native aenv (Array sh e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array sh' e))
mkPermuteP_mutex uid aenv repr shr combine project marr =
  let
      (start, end, paramGang) = gangParam    (arrayRshape repr)
      (arrOut,  paramOut)     = mutableArray (reprOut repr shr)  "out"
      (arrLock, paramLock)    = mutableArray reprLock "lock"
      (arrIn,   paramIn)      = delayedArray "in" marr
      paramEnv                = envParam aenv
  in
  makeOpenAcc uid "permuteP_mutex" (paramGang ++ paramOut ++ paramLock ++ paramIn ++ paramEnv) $ do

    sh <- delayedExtent arrIn

    imapNestFromTo (arrayRshape repr) start end sh $ \ix _ -> do

      ix' <- app1 project ix

      -- project element onto the destination array and (atomically) update
      when (isJust ix') $ do
        i <- fromJust ix'
        j <- intOfIndex shr (irArrayShape arrOut) i
        x <- app1 (delayedIndex arrIn) ix

        atomically arrLock j $ do
          y <- readArray TypeInt arrOut j
          r <- app2 combine x y
          writeArray TypeInt arrOut j r

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
    -> Operands Int
    -> CodeGen Native a
    -> CodeGen Native a
atomically barriers i action = do
  let
      lock      = integral integralType 1
      unlock    = integral integralType 0
      unlocked  = ir TypeWord8 unlock
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
  old  <- instr $ AtomicRMW numType NonVolatile Exchange addr lock   (CrossThread, Acquire)
  ok   <- A.eq singleType old unlocked
  _    <- cbr ok crit spin

  -- We just acquired the lock; perform the critical section then release the
  -- lock and exit. For ("some") x86 processors, an unlocked MOV instruction
  -- could be used rather than the slower XCHG, due to subtle memory ordering
  -- rules.
  setBlock crit
  r    <- action
  _    <- instr $ AtomicRMW numType NonVolatile Exchange addr unlock (CrossThread, Release)
  _    <- br exit

  setBlock exit
  return r


-- Helper functions
-- ----------------

reprOut :: ArrayR (Array sh e) -> ShapeR sh' -> ArrayR (Array sh' e)
reprOut (ArrayR _ tp) shr = ArrayR shr tp

reprLock :: ArrayR (Array ((), Int) Word8)
reprLock = ArrayR (ShapeRsnoc ShapeRz) $ TupRsingle scalarTypeWord8

