{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Stencil
-- Copyright   : [2018..2019] The Accelerate Team
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

import Data.Array.Accelerate.AST                                    ( StencilR(..) )
import Data.Array.Accelerate.Array.Representation
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
    -> TupleType b
    -> IRFun1      PTX aenv (stencil -> b)
    -> IRBoundary  PTX aenv (Array sh a)
    -> MIRDelayed  PTX aenv (Array sh a)
    -> CodeGen     PTX      (IROpenAcc PTX aenv (Array sh b))
mkStencil1 aenv stencil tp fun bnd marr =
  let repr             = ArrayR (stencilShape stencil) tp
      halo             = stencilBorder stencil
      (arrIn, paramIn) = delayedArray "in" marr
  in
  (+++) <$> mkInside aenv repr halo (IRFun1 $ app1 fun <=< stencilAccess stencil Nothing    arrIn) paramIn
        <*> mkBorder aenv repr      (IRFun1 $ app1 fun <=< stencilAccess stencil (Just bnd) arrIn) paramIn


mkStencil2
    :: Gamma           aenv
    -> StencilR sh a stencil1
    -> StencilR sh b stencil2
    -> TupleType c
    -> IRFun2      PTX aenv (stencil1 -> stencil2 -> c)
    -> IRBoundary  PTX aenv (Array sh a)
    -> MIRDelayed  PTX aenv (Array sh a)
    -> IRBoundary  PTX aenv (Array sh b)
    -> MIRDelayed  PTX aenv (Array sh b)
    -> CodeGen     PTX      (IROpenAcc PTX aenv (Array sh c))
mkStencil2 aenv stencil1 stencil2 tp f bnd1 marr1 bnd2 marr2 =
  let
      shr = stencilShape stencil1
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

      halo1   = stencilBorder stencil1
      halo2   = stencilBorder stencil2
      halo    = union shr halo1 halo2
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

      ixIn  <- indexOfInt shr shInside i    -- convert to multidimensional index of inside region
      ixOut <- offset shr ixIn (lift (shapeType shr) halo)  -- shift to multidimensional index of outside region
      r     <- app1 apply ixOut         -- apply generator function
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
      r     <- app1 apply ixOut         -- apply generator function
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

stencilBorder :: StencilR sh a stencil -> sh
stencilBorder = go
  where
    go :: StencilR sh' e stencil' -> sh'
    go = Prelude.snd . go'

    go' :: StencilR sh' e stencil' -> (ShapeR sh', sh')
    go' (StencilRunit3 _) = (dim1, ((), 1))
    go' (StencilRunit5 _) = (dim1, ((), 2))
    go' (StencilRunit7 _) = (dim1, ((), 3))
    go' (StencilRunit9 _) = (dim1, ((), 4))
    --
    go' (StencilRtup3 a b c            ) = (ShapeRsnoc shr, (foldl1 (union shr) [a', go b, go c]                                    , 1))
      where (shr, a') = go' a
    go' (StencilRtup5 a b c d e        ) = (ShapeRsnoc shr, (foldl1 (union shr) [a', go b, go c, go d, go e]                        , 2))
      where (shr, a') = go' a
    go' (StencilRtup7 a b c d e f g    ) = (ShapeRsnoc shr, (foldl1 (union shr) [a', go b, go c, go d, go e, go f, go g]            , 3))
      where (shr, a') = go' a
    go' (StencilRtup9 a b c d e f g h i) = (ShapeRsnoc shr, (foldl1 (union shr) [a', go b, go c, go d, go e, go f, go g, go h, go i], 4))
      where (shr, a') = go' a

