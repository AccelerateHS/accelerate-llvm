{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Native.Map
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--


module Data.Array.Accelerate.LLVM.CodeGen.Native.Map
  where

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Attribute
import LLVM.General.AST.Global
import LLVM.General.AST.Type

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt, eltType )
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Trafo

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

-- standard library
import Control.Monad.State
import qualified Data.IntMap                                    as Map


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

mkMap :: forall aenv sh a b. Elt b
      => Aval aenv
      -> IRFun1    aenv (a -> b)
      -> IRDelayed aenv (Array sh a)
      -> Skeleton  aenv (Array sh b)
mkMap _aenv apply IRDelayed{..}
  = runLLVM "map"
  $ do
        ((), code) <- runCodeGen body
        return $ functionDefaults
                   { returnType  = VoidType
                   , name        = "map"
                   , parameters  = (paramOut ++ paramIn, False)
                   , basicBlocks = code
                   }
  where
    arrOut      = arrayData (undefined::Array sh b) "out"
    n           = local $ Name "n"
    zero        = constOp $ scalar sint 0
    one         = constOp $ num nint 1

    sint        = scalarType :: ScalarType Int
    nint        = numType    :: NumType Int

    -- TODO
    arrIn       = arrayData (undefined::Array sh a) "in"
    ptr t       = PointerType t (AddrSpace 0)
    paramOut    = [ Parameter (ptr t) v [NoAlias, NoCapture] | t <- llvmOfTupleType (eltType (undefined::b)) | v <- arrOut ]
    paramIn     = [ Parameter (ptr t) v [NoAlias, NoCapture] | t <- llvmOfTupleType (eltType (undefined::a)) | v <- arrIn ]
               ++ [ Parameter (typeOf nint) (Name "n") [] ]

    body :: CodeGen ()
    body = do
      loop <- newBlock "loop.top"
      exit <- newBlock "loop.exit"

      -- Header: check if n > 0
      c    <- gt sint n zero
      top  <- cbr c loop exit

      -- Body: keep looping until i == n
      --
      -- Read an element from the array, apply the function, and store the value
      -- into the result array.
      --
      setBlock loop
      indv <- lift freshName
      let i = local indv
      xs   <- delayedLinearIndex [i]
      ys   <- apply xs
      writeArray arrOut i ys

      i'   <- add nint i one
      c'   <- eq sint i' n
      bot  <- cbr c' loop exit
      _    <- phi' loop indv (typeOf nint) [(i', bot), (zero,top)]

      setBlock exit
      _    <- terminate $ Do (Ret Nothing [])
      return ()
