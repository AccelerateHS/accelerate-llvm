{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Stencil
-- Copyright   : [2018] Trevor L. McDonell
--               [2018] Josh Meredith
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Stencil (

  mkStencil1,
  mkStencil2,

) where

import Data.Array.Accelerate.AST                                ( Stencil )
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )

import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Stencil
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
import Data.Array.Accelerate.LLVM.Native.Target                 ( Native )

import LLVM.AST.Type.Name

import Control.Monad


-- The stencil function is similar to a map, but has access to surrounding
-- elements as specified by the stencil pattern.
--
-- This generates two functions:
--
--  * stencil_inside: does not apply boundary conditions, assumes all element
--                    accesses are valid
--
--  * stencil_border: applies boundary condition check to each array access
--
mkStencil1
    :: (Stencil sh a stencil, Elt b)
    => UID
    -> Gamma aenv
    -> IRFun1     Native aenv (stencil -> b)
    -> IRBoundary Native aenv (Array sh a)
    -> IRDelayed  Native aenv (Array sh a)
    -> CodeGen (IROpenAcc Native aenv (Array sh b))
mkStencil1 uid aenv f bnd arr =
  (+++) <$> mkInside uid aenv (IRFun1 $ app1 f <=< stencilAccess Nothing    arr)
        <*> mkBorder uid aenv (IRFun1 $ app1 f <=< stencilAccess (Just bnd) arr)

mkStencil2
    :: (Stencil sh a stencil1, Stencil sh b stencil2, Elt c)
    => UID
    -> Gamma aenv
    -> IRFun2     Native aenv (stencil1 -> stencil2 -> c)
    -> IRBoundary Native aenv (Array sh a)
    -> IRDelayed  Native aenv (Array sh a)
    -> IRBoundary Native aenv (Array sh b)
    -> IRDelayed  Native aenv (Array sh b)
    -> CodeGen (IROpenAcc Native aenv (Array sh c))
mkStencil2 uid aenv f bnd1 arr1 bnd2 arr2 =
  let
      inside  = IRFun1 $ \ix -> do
        stencil1 <- stencilAccess Nothing arr1 ix
        stencil2 <- stencilAccess Nothing arr2 ix
        app2 f stencil1 stencil2
      --
      border  = IRFun1 $ \ix -> do
        stencil1 <- stencilAccess (Just bnd1) arr1 ix
        stencil2 <- stencilAccess (Just bnd2) arr2 ix
        app2 f stencil1 stencil2
  in
  (+++) <$> mkInside uid aenv inside
        <*> mkBorder uid aenv border


mkInside
    :: forall aenv sh e. (Shape sh, Elt e)
    => UID
    -> Gamma aenv
    -> IRFun1  Native aenv (sh -> e)
    -> CodeGen (IROpenAcc Native aenv (Array sh e))
mkInside = mkGenerate "stencil_inside"

mkBorder
    :: forall aenv sh e. (Shape sh, Elt e)
    => UID
    -> Gamma aenv
    -> IRFun1  Native aenv (sh -> e)
    -> CodeGen (IROpenAcc Native aenv (Array sh e))
mkBorder = mkGenerate "stencil_border"

mkGenerate
    :: forall aenv sh e. (Shape sh, Elt e)
    => Label
    -> UID
    -> Gamma aenv
    -> IRFun1  Native aenv (sh -> e)
    -> CodeGen (IROpenAcc Native aenv (Array sh e))
mkGenerate name uid aenv apply =
  let
      (start, end, paramGang)   = gangParam    (Proxy :: Proxy sh)
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh e))
      paramEnv                  = envParam aenv
      shOut                     = irArrayShape arrOut
  in
  makeOpenAcc uid name (paramGang ++ paramOut ++ paramEnv) $ do

    imapNestFromTo start end shOut $ \ix i -> do
      r <- app1 apply ix                        -- apply generator function
      writeArray arrOut i r                     -- store result

    return_

