{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Stencil
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Stencil (

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
import Data.Array.Accelerate.LLVM.CodeGen.Loop                      hiding ( imapFromStepTo )
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Stencil
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )

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
    :: UID
    -> Gamma              aenv
    -> StencilR sh a stencil
    -> TypeR b
    -> IRFun1      Native aenv (stencil -> b)
    -> IRBoundary  Native aenv (Array sh a)
    -> MIRDelayed  Native aenv (Array sh a)
    -> CodeGen     Native      (IROpenAcc Native aenv (Array sh b))
mkStencil1 uid aenv sr tp f bnd marr =
  let (arrIn, paramIn) = delayedArray "in" marr
      repr = ArrayR (stencilShapeR sr) tp
   in (+++) <$> mkInside uid aenv repr (IRFun1 $ app1 f <=< stencilAccess sr Nothing    arrIn) paramIn
            <*> mkBorder uid aenv repr (IRFun1 $ app1 f <=< stencilAccess sr (Just bnd) arrIn) paramIn

mkStencil2
    :: UID
    -> Gamma              aenv
    -> StencilR sh a stencil1
    -> StencilR sh b stencil2
    -> TypeR c
    -> IRFun2      Native aenv (stencil1 -> stencil2 -> c)
    -> IRBoundary  Native aenv (Array sh a)
    -> MIRDelayed  Native aenv (Array sh a)
    -> IRBoundary  Native aenv (Array sh b)
    -> MIRDelayed  Native aenv (Array sh b)
    -> CodeGen     Native      (IROpenAcc Native aenv (Array sh c))
mkStencil2 uid aenv sr1 sr2 tp f bnd1 marr1 bnd2 marr2 =
  let
      (arrIn1, paramIn1)  = delayedArray "in1" marr1
      (arrIn2, paramIn2)  = delayedArray "in2" marr2

      repr = ArrayR (stencilShapeR sr1) tp

      inside  = IRFun1 $ \ix -> do
        stencil1 <- stencilAccess sr1 Nothing arrIn1 ix
        stencil2 <- stencilAccess sr2 Nothing arrIn2 ix
        app2 f stencil1 stencil2
      --
      border  = IRFun1 $ \ix -> do
        stencil1 <- stencilAccess sr1 (Just bnd1) arrIn1 ix
        stencil2 <- stencilAccess sr2 (Just bnd2) arrIn2 ix
        app2 f stencil1 stencil2
  in
  (+++) <$> mkInside uid aenv repr inside (paramIn1 ++ paramIn2)
        <*> mkBorder uid aenv repr border (paramIn1 ++ paramIn2)


mkInside
    :: UID
    -> Gamma aenv
    -> ArrayR (Array sh e)
    -> IRFun1  Native aenv (sh -> e)
    -> [LLVM.Parameter]
    -> CodeGen Native      (IROpenAcc Native aenv (Array sh e))
mkInside uid aenv repr apply paramIn =
  let
      (start, end, paramGang)   = gangParam    (arrayRshape repr)
      (arrOut, paramOut)        = mutableArray repr "out"
      paramEnv                  = envParam aenv
      shOut                     = irArrayShape arrOut
  in
  makeOpenAcc uid "stencil_inside" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do

    imapNestFromToTile (arrayRshape repr) 4 start end shOut $ \ix i -> do
      r <- app1 apply ix                        -- apply generator function
      writeArray TypeInt arrOut i r                     -- store result

    return_

mkBorder
    :: UID
    -> Gamma aenv
    -> ArrayR (Array sh e)
    -> IRFun1  Native aenv (sh -> e)
    -> [LLVM.Parameter]
    -> CodeGen Native      (IROpenAcc Native aenv (Array sh e))
mkBorder uid aenv repr apply paramIn =
  let
      (start, end, paramGang)   = gangParam    (arrayRshape repr)
      (arrOut, paramOut)        = mutableArray repr "out"
      paramEnv                  = envParam aenv
      shOut                     = irArrayShape arrOut
  in
  makeOpenAcc uid "stencil_border" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do

    imapNestFromTo (arrayRshape repr) start end shOut $ \ix i -> do
      r <- app1 apply ix                        -- apply generator function
      writeArray TypeInt arrOut i r             -- store result

    return_


imapNestFromToTile
    :: ShapeR sh
    -> Int                                                  -- ^ unroll amount (tile height)
    -> Operands sh                                          -- ^ initial index (inclusive)
    -> Operands sh                                          -- ^ final index (exclusive)
    -> Operands sh                                          -- ^ total array extent
    -> (Operands sh -> Operands Int -> CodeGen Native ())   -- ^ apply at each index
    -> CodeGen Native ()
imapNestFromToTile shr unroll start end extent body =
  go shr start end body'
  where
    body' ix = body ix =<< intOfIndex shr extent ix

    go :: ShapeR t
       -> Operands t
       -> Operands t
       -> (Operands t -> CodeGen Native ())
       -> CodeGen Native ()
    go ShapeRz OP_Unit OP_Unit k
      = k OP_Unit

    -- To correctly generate the unrolled loop nest we need to explicitly match
    -- on the last two dimensions.
    --
    go (ShapeRsnoc (ShapeRsnoc ShapeRz)) (OP_Pair (OP_Pair OP_Unit sy) sx) (OP_Pair (OP_Pair OP_Unit ey) ex) k
      = do
          -- Tile the stencil operator in the xy-plane by unrolling in the
          -- y-dimension and vectorising in the x-dimension.
          --
          sy' <- imapFromStepTo sy (liftInt unroll) ey $ \iy ->
                  imapFromTo    sx                  ex $ \ix ->
                    forM_ [0 .. unroll-1] $ \n -> do
                    iy' <- add numType iy (liftInt n)
                    k (OP_Pair (OP_Pair OP_Unit iy') ix)

          -- Take care of any remaining loop iterations in the y-dimension
          --
          _       <- imapFromTo  sy' ey $ \iy ->
                      imapFromTo sx  ex $ \ix ->
                        k (OP_Pair (OP_Pair OP_Unit iy) ix)
          return ()

    -- The 1- and 3+-dimensional cases can recurse normally
    --
    go (ShapeRsnoc shr') (OP_Pair ssh ssz) (OP_Pair esh esz) k
      = go shr' ssh esh
      $ \sz      -> imapFromTo ssz esz
      $ \i       -> k (OP_Pair sz i)

imapFromStepTo
    :: Operands Int
    -> Operands Int
    -> Operands Int
    -> (Operands Int -> CodeGen Native ())
    -> CodeGen Native (Operands Int)
imapFromStepTo start step end body =
  let
      incr i = add numType i step
      test i = do i' <- incr i
                  lt singleType i' end
  in
  while (TupRsingle scalarTypeInt) test
        (\i -> body i >> incr i)
        start

