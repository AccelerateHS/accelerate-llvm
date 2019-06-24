{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Map
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Map
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt, DIM1 )

import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache

import Data.Array.Accelerate.LLVM.Native.Target                 ( Native )
import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop


-- C Code
-- ======
--
-- float f(float);
--
-- void map(float* __restrict__ out, const float* __restrict__ in, const int n)
-- {
--     for (int i = 0; i < n; ++i)
--         out[i] = f(in[i]);
--
--     return;
-- }

-- Corresponding LLVM
-- ==================
--
-- define void @map(float* noalias nocapture %out, float* noalias nocapture %in, i32 %n) nounwind uwtable ssp {
--   %1 = icmp sgt i32 %n, 0
--   br i1 %1, label %.lr.ph, label %._crit_edge
--
-- .lr.ph:                                           ; preds = %0, %.lr.ph
--   %indvars.iv = phi i64 [ %indvars.iv.next, %.lr.ph ], [ 0, %0 ]
--   %2 = getelementptr inbounds float* %in, i64 %indvars.iv
--   %3 = load float* %2, align 4
--   %4 = tail call float @apply(float %3) nounwind
--   %5 = getelementptr inbounds float* %out, i64 %indvars.iv
--   store float %4, float* %5, align 4
--   %indvars.iv.next = add i64 %indvars.iv, 1
--   %lftr.wideiv = trunc i64 %indvars.iv.next to i32
--   %exitcond = icmp eq i32 %lftr.wideiv, %n
--   br i1 %exitcond, label %._crit_edge, label %.lr.ph
--
-- ._crit_edge:                                      ; preds = %.lr.ph, %0
--   ret void
-- }
--
-- declare float @apply(float)
--

-- Apply the given unary function to each element of an array.
--
-- The map operation can always treat an array of any dimension in its flat
-- underlying representation, which simplifies code generation.
--
mkMap :: forall aenv sh a b. (Shape sh, Elt a, Elt b)
      => UID
      -> Gamma            aenv
      -> IRFun1    Native aenv (a -> b)
      -> CodeGen   Native      (IROpenAcc Native aenv (Array sh b))
mkMap uid aenv apply =
  let
      (start, end, paramGang)   = gangParam @DIM1
      (arrIn,  paramIn)         = mutableArray @sh "in"
      (arrOut, paramOut)        = mutableArray @sh "out"
      paramEnv                  = envParam aenv
  in
  makeOpenAcc uid "map" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do

    imapFromTo (indexHead start) (indexHead end) $ \i -> do
      xs <- readArray arrIn i
      ys <- app1 apply xs
      writeArray arrOut i ys

    return_

