{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Array.Accelerate.Array.Sugar                            ( Array, Shape, Elt )
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Sugar                  as S

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )
import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop


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
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRFun1    Native aenv (sh -> sh')
    -> IRDelayed Native aenv (Array sh e)
    -> CodeGen (IROpenAcc Native aenv (Array sh' e))
mkPermute aenv combine project arr =
  mkPermuteS aenv combine project arr



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
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRFun1    Native aenv (sh -> sh')
    -> IRDelayed Native aenv (Array sh e)
    -> CodeGen (IROpenAcc Native aenv (Array sh' e))
mkPermuteS aenv combine project IRDelayed{..} =
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

