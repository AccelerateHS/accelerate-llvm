{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Permute
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Permute (

  IRPermuteFun(..),
  llvmOfPermuteFun,

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                            hiding ( Foreign )
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Foreign

import LLVM.General.AST.Type.Instruction.RMW

import Control.Applicative
import Prelude


-- | A forward permutation might be specialised to use atomic instructions to
-- perform the read-modify-write of the output array directly, rather than
-- separately acquiring a lock. The basic operation is always provided in case
-- a backend does not support the atomic operation at that type, or if it is
-- executing sequentially.
--
-- For the atomicRMW case, the function is applied to the new value before
-- feeding to the atomic instruction to combine with the old.
--
data IRPermuteFun arch aenv t where
  IRPermuteFun :: { combine   :: IRFun2 arch aenv (e -> e -> e)
                  , atomicRMW :: Maybe
                      ( RMWOperation
                      , IRFun1 arch aenv (e -> e)
                      )
                  }
               -> IRPermuteFun arch aenv (e -> e -> e)


-- | Analysis and code generation for forward permutation combination function.
--
-- Specialisation for atomic operations is currently limited to direct
-- applications of the function; that is, we don't dig down underneath
-- let-bindings.
--
llvmOfPermuteFun
    :: forall arch aenv e. Foreign arch
    => arch
    -> DelayedFun aenv (e -> e -> e)
    -> Gamma aenv
    -> IRPermuteFun arch aenv (e -> e -> e)
llvmOfPermuteFun arch fun aenv = IRPermuteFun{..}
  where
    combine   = llvmOfFun2 arch fun aenv
    atomicRMW
      -- LLVM supports atomic operations for integral types only; see if we can
      -- use one of those.
      | Lam (Lam (Body body))                           <- fun
      , SingleTuple (NumScalarType (IntegralNumType _)) <- eltType (undefined::e)
      , Just (rmw, x)                                   <- rmwOp body
      , Just x'                                         <- strengthenE latest x
      , fun'                                            <- llvmOfFun1 arch (Lam (Body x')) aenv
      = Just (rmw, fun')

      -- If the old value is not used (i.e. permute const) then we can just
      -- store the new value directly. We can do this for any scalar value (not
      -- just those types supported by the atomicRMW instruction) with a regular
      -- Store, but not for product types; this is racey and different fields
      -- could get their value from different threads.
      | Lam (Lam (Body body))                           <- fun
      , SingleTuple _                                   <- eltType (undefined::e)
      , Just body'                                      <- strengthenE latest body
      , fun'                                            <- llvmOfFun1 arch (Lam (Body body')) aenv
      = Just (Exchange, fun')

      | otherwise
      = Nothing

    rmwOp :: DelayedOpenExp (((),e),e) aenv e -> Maybe (RMWOperation, DelayedOpenExp (((),e),e) aenv e)
    rmwOp (PrimApp f xs)
      | PrimAdd{}  <- f = (Add,) <$> extract xs
      | PrimSub{}  <- f = (Sub,) <$> extract xs
      | PrimMin{}  <- f = (Min,) <$> extract xs
      | PrimMax{}  <- f = (Max,) <$> extract xs
      | PrimBOr{}  <- f = (Or,)  <$> extract xs
      | PrimBAnd{} <- f = (And,) <$> extract xs
      | PrimBXor{} <- f = (Xor,) <$> extract xs
    rmwOp _             = Nothing

    -- Determine which argument to a binary function was the new value being
    -- combined. This only works when the old value is used unmodified, but that
    -- is sufficient for us because otherwise it would not be suitable for the
    -- atomic update operation.
    --
    -- In the permutation function, the old value is given as the second
    -- argument, corresponding to ZeroIdx.
    --
    extract :: DelayedOpenExp (((),e),e) aenv (e,e) -> Maybe (DelayedOpenExp (((),e),e) aenv e)
    extract (Tuple (SnocTup (SnocTup NilTup x) y))
      | Just REFL <- match x (Var ZeroIdx) = Just y
      | Just REFL <- match y (Var ZeroIdx) = Just x
    extract _
      = Nothing

    -- Used with 'strengthenE' to ensure that the expression does not make use
    -- of the old value except in the combination function.
    latest :: (((),e),e) :?> ((),e)
    latest ZeroIdx      = Nothing
    latest (SuccIdx ix) = Just ix

