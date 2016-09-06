{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Permute
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Permute
  where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Sugar                            ( Array, Vector, Shape, Elt, eltType )
import qualified Data.Array.Accelerate.Array.Sugar                  as S
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Permute
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Loop
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Target

import LLVM.General.AST.Type.AddrSpace
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Instruction.Atomic
import LLVM.General.AST.Type.Instruction.RMW                        as RMW
import LLVM.General.AST.Type.Instruction.Volatile
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Representation

import Control.Applicative
import Data.Typeable
import Data.String                                                  ( fromString )
import Prelude
import Control.Monad                                                ( void )


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
    => PTX
    -> Gamma aenv
    -> IRPermuteFun PTX aenv (e -> e -> e)
    -> IRFun1       PTX aenv (sh -> sh')
    -> IRDelayed    PTX aenv (Array sh e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh' e))
mkPermute ptx@(deviceProperties . ptxContext -> dev) aenv combine project arr =
  mkPermuteP dev aenv combine project arr
  -- (+++) <$> mkPermuteS aenv combine project arr
  --       <*> mkPermuteP aenv combine project arr




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
    => DeviceProperties
    -> Gamma aenv
    -> IRPermuteFun PTX aenv (e -> e -> e)
    -> IRFun1       PTX aenv (sh -> sh')
    -> IRDelayed    PTX aenv (Array sh e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh' e))
mkPermuteP dev aenv IRPermuteFun{..} project arr =
  mkPermuteP_mutex dev aenv combine project arr
  --   case atomicRMW of
  --     Nothing       -> mkPermuteP_mutex aenv combine project arr
  --     Just (rmw, f) -> mkPermuteP_rmw   aenv rmw f   project arr


-- mkPermuteP_rmw
--     :: forall aenv sh sh' e. (Shape sh, Shape sh', Elt e)
--     => Gamma aenv
--     -> RMWOperation
--     -> IRFun1    PTX aenv (e -> e)
--     -> IRFun1    PTX aenv (sh -> sh')
--     -> IRDelayed PTX aenv (Array sh e)
--     -> CodeGen (IROpenAcc PTX aenv (Array sh' e))
-- mkPermuteP_rmw aenv rmw update project IRDelayed{..} =
--   let
--       (start, end, paramGang)   = gangParam
--       (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh' e))
--       paramEnv                  = envParam aenv
--   in
--   makeOpenAcc "permuteP_rmw" (paramGang ++ paramOut ++ paramEnv) $ do
--
--     sh <- delayedExtent
--
--     imapFromTo start end $ \i -> do
--
--       ix  <- indexOfInt sh i
--       ix' <- app1 project ix
--
--       unless (ignore ix') $ do
--         j <- intOfIndex (irArrayShape arrOut) ix'
--         x <- app1 delayedLinearIndex i
--         r <- app1 update x
--
--         case rmw of
--           Exchange
--             -> writeArray arrOut j r
--           --
--           _ | SingleTuple s@(NumScalarType (IntegralNumType t)) <- eltType (undefined::e)
--             , Just adata <- gcast (irArrayData arrOut)
--             , Just r'    <- gcast r
--             -> do
--                   addr <- instr' $ GetElementPtr (ptr s (op t adata)) [op integralType j]
--                   _    <- instr' $ AtomicRMW t NonVolatile rmw addr (op t r') (CrossThread, AcquireRelease)
--                   return ()
--           _ -> $internalError "mkPermute_rmw" "unexpected transition"
--
--     return_


mkPermuteP_mutex
    :: forall aenv sh sh' e. (Shape sh, Shape sh', Elt e)
    => DeviceProperties
    -> Gamma aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRFun1    PTX aenv (sh -> sh')
    -> IRDelayed PTX aenv (Array sh e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh' e))
mkPermuteP_mutex dev aenv combine project IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out"  :: Name (Array sh' e))
      (arrLock, paramLock)      = mutableArray ("lock" :: Name (Vector Word32))
      paramEnv                  = envParam aenv
  in
  makeOpenAccWith (simpleLaunchConfig dev) "permute_mutex" (paramGang ++ paramOut ++ paramLock ++ paramEnv) $ do

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
-- It is important that the thread loops trying to acquire the lock without
-- writing data anything until the lock value changes. Then, because of MESI
-- caching protocols there will be no bus traffic while the CPU waits for the
-- value to change.
--
-- <https://en.wikipedia.org/wiki/Spinlock#Significant_optimizations>
--
atomically
    :: IRArray (Vector Word32)
    -> IR Int
    -> CodeGen ()
    -> CodeGen ()
atomically barriers i action =
  void $ do
    addr  <- instr $ GetElementPtr (ptr scalarType (op integralType (irArrayData barriers))) [op integralType i]
    while (\done -> (A.eq scalarType done (lift 0)))
          (\done -> do
            oldVal <- instr $ AtomicRMW integralType NonVolatile Exchange addr (lift 1) (CrossThread, Acquire)
            done'  <- if (A.eq scalarType oldVal (lift 0))
                        then do
                          action
                          instr $ AtomicRMW integralType NonVolatile Exchange addr (lift 0) (CrossThread, Release)
                          return (lift 1)
                        else return (lift 0)
            __syncthreads
            return done')
          (lift 0)



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


-- XXX: hack because we can't properly manipulate pointer-type operands.
--
ptr :: ScalarType a -> Operand a -> Operand (Ptr a)
ptr t x =
  let
      rename (Name n)   = Name n
      rename (UnName n) = UnName n
      ptr_t             = PrimType (PtrPrimType t defaultAddrSpace)
  in
  case x of
    LocalReference _ n -> LocalReference ptr_t (rename n)
    ConstantOperand{}  -> $internalError "atomically" "expected local reference"

