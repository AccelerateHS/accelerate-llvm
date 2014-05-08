{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Permute
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Permute
  where

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.RMWOperation

-- accelerate
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar                                ( Array, Vector, Shape, Elt )
import qualified Data.Array.Accelerate.Array.Sugar                      as Sugar

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                    as A
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop

-- standard library
import Prelude                                                          hiding ( all )
import Control.Monad


-- A forward permutation is specified by an index mapping that determines, for
-- each element in the source array, where it should go in the target array. The
-- resultant array is initialised with the given defaults and any further values
-- that are permuted into the result array are added to the current value using
-- the given combination function.
--
-- The combination function must be associative. Elements that are mapped to the
-- magic value 'ignore' by the permutation function are dropped.
--
--   permute :: (Shape sh, Shape sh', Elt a)
--           => (Exp a -> Exp a -> Exp a)               -- combination function
--           -> Acc (Array sh' a)                       -- array of default values (manifest)
--           -> (Exp sh -> Exp sh')                     -- forward permutation
--           -> Acc (Array sh  a)                       -- array to be permuted (delayed)
--           -> Acc (Array sh' a)
--
mkPermute
    :: forall arch aenv sh sh' e. (Shape sh, Shape sh', Elt e)
    => Gamma aenv
    -> IRFun2 aenv (e -> e -> e)
    -> IRFun1 aenv (sh -> sh')
    -> IRDelayed aenv (Array sh e)
    -> CodeGen [Kernel arch aenv (Array sh' e)]
mkPermute aenv combine permute IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      arrOut                    = arrayData  (undefined::Array sh' e) "out"
      shOut                     = arrayShape (undefined::Array sh' e) "out"
      paramOut                  = arrayParam (undefined::Array sh' e) "out"
      paramEnv                  = envParam aenv

      ignore                    = map (constOp . integral integralType)
                                $ Sugar.shapeToList (Sugar.ignore :: sh')

      barrier                   = arrayData  (undefined::Vector Word8) "barrier"
      paramBarrier              = arrayParam (undefined::Vector Word8) "barrier"
  in do
  makeKernel "permute" (paramGang ++ paramBarrier ++ paramOut ++ paramEnv) $ do
    imapFromTo start end $ \i -> do
      ix   <- indexOfInt shOut i                -- convert to multidimensional index
      ix'  <- permute ix                        -- apply backwards index permutation

      -- If this index will not be used, jump immediately to the exit
      exit <- all (uncurry (neq int)) (zip ix' ignore)
      dst  <- intOfIndex shOut ix'              -- index of result array

      -- The thread spins waiting for this slot of the output array to become
      -- unlocked. It takes the associated flag, write-combines its results, and
      -- returns the lock.
      _    <- spinlock barrier dst $ do
        old     <- readArray arrOut dst
        new     <- delayedLinearIndex [i]
        val     <- combine new old
        writeArray arrOut dst val

      _    <- br exit
      setBlock exit

    return_


-- Execute the given action only when the lock at the given array and index can
-- be taken. The thread spins waiting for the lock to be released, which is the
-- worst possible setup in a highly contented environment.
--
-- However, since the thread attempts to acquire the lock in a loop without
-- trying to write anything until the value changes, because of MESI caching
-- protocols this should cause the cache line the lock is on to become "Shared".
-- If this is the case, then there is remarkably _no_ bus traffic while the CPU
-- waits for the lock. This optimisation is effective on all CPU architectures
-- that have a cache per CPU.
--
spinlock :: [Name] -> Operand -> CodeGen a -> CodeGen (a, Block)
spinlock barrier' i action =
  let
      [barrier] = barrier'
      locked    = constOp (integral (integralType :: IntegralType Word8) 1)
      unlocked  = constOp (integral (integralType :: IntegralType Word8) 0)
  in do
  loop  <- newBlock "spinlock.entry"
  done  <- newBlock "spinlock.critical-section"

  addr  <- instr $ GetElementPtr False (local barrier) [i] []
  _     <- br loop
  setBlock loop

  -- Atomically set the slot to the locked state, returning the old state. If
  -- the slot was unlocked we just acquired it, otherwise the state remains
  -- unchanged (previously locked) and we need to spin until it becomes
  -- unlocked.
  --
  old   <- instr $ AtomicRMW True Xchg addr locked (Atomicity True Acquire) []
  c     <- eq (scalarType :: ScalarType Word8) old locked
  _     <- cbr c loop done

  -- Later x86 architectures can release the lock safely by using an unlocked
  -- MOV instruction rather than the slower locked XCHG. This is due to subtle
  -- memory ordering rules which allow this, even though MOV is not a full
  -- memory barrier.
  --
  setBlock done
  res   <- action
  do_    $ Store True addr unlocked (Just $ Atomicity True Release) 0 []

  return (res, done)


-- Apply the function (f :: a -> Bool) to each argument. If any application
-- returns false, jump to the exit block, the label of which is supplied as the
-- return value
--
all :: forall a. (a -> CodeGen Operand) -> [a] -> CodeGen Block
all f args = do
  exit  <- newBlock "all.exit"

  let go :: Int -> a -> CodeGen ()
      go n x = do next <- newBlock ("all.and" ++ show n)
                  c    <- f x
                  _    <- cbr c next exit
                  setBlock next

  zipWithM_ go [0..] args
  return exit

