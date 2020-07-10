{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Stencil
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Stencil (

  mkStencil1,
  mkStencil2,

) where

import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Stencil
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Stencil
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Loop
import Data.Array.Accelerate.LLVM.PTX.Target                        ( PTX )

import qualified LLVM.AST.Global                                    as LLVM

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
    :: Gamma           aenv
    -> StencilR sh a stencil
    -> TypeR b
    -> IRFun1      PTX aenv (stencil -> b)
    -> IRBoundary  PTX aenv (Array sh a)
    -> MIRDelayed  PTX aenv (Array sh a)
    -> CodeGen     PTX      (IROpenAcc PTX aenv (Array sh b))
mkStencil1 aenv stencil tp fun bnd marr =
  let repr             = ArrayR shr tp
      (shr, halo)      = stencilHalo stencil
      (arrIn, paramIn) = delayedArray "in" marr
  in
  (+++) <$> mkInside aenv repr halo (IRFun1 $ app1 fun <=< stencilAccess stencil Nothing    arrIn) paramIn
        <*> mkBorder aenv repr      (IRFun1 $ app1 fun <=< stencilAccess stencil (Just bnd) arrIn) paramIn


mkStencil2
    :: Gamma           aenv
    -> StencilR sh a stencil1
    -> StencilR sh b stencil2
    -> TypeR c
    -> IRFun2      PTX aenv (stencil1 -> stencil2 -> c)
    -> IRBoundary  PTX aenv (Array sh a)
    -> MIRDelayed  PTX aenv (Array sh a)
    -> IRBoundary  PTX aenv (Array sh b)
    -> MIRDelayed  PTX aenv (Array sh b)
    -> CodeGen     PTX      (IROpenAcc PTX aenv (Array sh c))
mkStencil2 aenv stencil1 stencil2 tp f bnd1 marr1 bnd2 marr2 =
  let
      repr = ArrayR shr tp
      (arrIn1, paramIn1)  = delayedArray "in1" marr1
      (arrIn2, paramIn2)  = delayedArray "in2" marr2

      inside  = IRFun1 $ \ix -> do
        s1 <- stencilAccess stencil1 Nothing arrIn1 ix
        s2 <- stencilAccess stencil2 Nothing arrIn2 ix
        app2 f s1 s2
      --
      border  = IRFun1 $ \ix -> do
        s1 <- stencilAccess stencil1 (Just bnd1) arrIn1 ix
        s2 <- stencilAccess stencil2 (Just bnd2) arrIn2 ix
        app2 f s1 s2

      (shr, halo1) = stencilHalo stencil1
      (_,   halo2) = stencilHalo stencil2
      halo         = union shr halo1 halo2
  in
  (+++) <$> mkInside aenv repr halo inside (paramIn1 ++ paramIn2)
        <*> mkBorder aenv repr      border (paramIn1 ++ paramIn2)


mkInside
    :: Gamma aenv
    -> ArrayR (Array sh e)
    -> sh
    -> IRFun1  PTX aenv (sh -> e)
    -> [LLVM.Parameter]
    -> CodeGen PTX      (IROpenAcc PTX aenv (Array sh e))
mkInside aenv repr@(ArrayR shr _) halo apply paramIn =
  let
      (arrOut, paramOut)  = mutableArray repr "out"
      paramInside         = parameter    (shapeType shr) "shInside"
      shInside            = local        (shapeType shr) "shInside"
      shOut               = irArrayShape arrOut
      paramEnv            = envParam aenv
      --
  in
  makeOpenAcc "stencil_inside" (paramInside ++ paramOut ++ paramIn ++ paramEnv) $ do

    start <- return (liftInt 0)
    end   <- shapeSize shr shInside

    -- iterate over the inside region as a linear index space
    --
    imapFromTo start end $ \i -> do

      ixIn  <- indexOfInt shr shInside i                    -- convert to multidimensional index of inside region
      ixOut <- offset shr ixIn (lift (shapeType shr) halo)  -- shift to multidimensional index of outside region
      r     <- app1 apply ixOut                             -- apply generator function
      j     <- intOfIndex shr shOut ixOut
      writeArray TypeInt arrOut j r

    return_


mkBorder
    :: Gamma aenv
    -> ArrayR (Array sh e)
    -> IRFun1  PTX aenv (sh -> e)
    -> [LLVM.Parameter]
    -> CodeGen PTX      (IROpenAcc PTX aenv (Array sh e))
mkBorder aenv repr@(ArrayR shr _) apply paramIn =
  let
      (arrOut, paramOut)  = mutableArray repr "out"
      paramFrom           = parameter    (shapeType shr) "shFrom"
      shFrom              = local        (shapeType shr) "shFrom"
      paramInside         = parameter    (shapeType shr) "shInside"
      shInside            = local        (shapeType shr) "shInside"
      shOut               = irArrayShape arrOut
      paramEnv            = envParam aenv
      --
  in
  makeOpenAcc "stencil_border" (paramFrom ++ paramInside ++ paramOut ++ paramIn ++ paramEnv) $ do

    start <- return (liftInt 0)
    end   <- shapeSize shr shInside

    imapFromTo start end $ \i -> do

      ixIn  <- indexOfInt shr shInside i    -- convert to multidimensional index of inside region
      ixOut <- offset shr ixIn shFrom       -- shift to multidimensional index of outside region
      r     <- app1 apply ixOut             -- apply generator function
      j     <- intOfIndex shr shOut ixOut
      writeArray TypeInt arrOut j r

    return_


offset :: ShapeR sh -> Operands sh -> Operands sh -> CodeGen PTX (Operands sh)
offset shr sh1 sh2 = go shr sh1 sh2
  where
    go :: ShapeR t -> Operands t -> Operands t -> CodeGen PTX (Operands t)
    go ShapeRz OP_Unit OP_Unit
      = return OP_Unit

    go (ShapeRsnoc t) (OP_Pair sa1 sb1) (OP_Pair sa2 sb2)
      = do x <- add (numType :: NumType Int) sb1 sb2
           OP_Pair <$> go t sa1 sa2 <*> return x

