{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Stencil
-- Copyright   : [2018..2019] The Accelerate Team
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

import Data.Array.Accelerate.AST                                    ( Stencil )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                            ( Array, Shape, Elt, eltType )
import Data.Array.Accelerate.Error
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
    :: (Stencil sh a stencil, Elt b)
    => UID
    -> Gamma              aenv
    -> IRFun1      Native aenv (stencil -> b)
    -> IRBoundary  Native aenv (Array sh a)
    -> MIRDelayed  Native aenv (Array sh a)
    -> CodeGen     Native      (IROpenAcc Native aenv (Array sh b))
mkStencil1 uid aenv f bnd marr =
  let (arrIn, paramIn) = delayedArray "in" marr
   in (+++) <$> mkInside uid aenv (IRFun1 $ app1 f <=< stencilAccess Nothing    arrIn) paramIn
            <*> mkBorder uid aenv (IRFun1 $ app1 f <=< stencilAccess (Just bnd) arrIn) paramIn

mkStencil2
    :: (Stencil sh a stencil1, Stencil sh b stencil2, Elt c)
    => UID
    -> Gamma              aenv
    -> IRFun2      Native aenv (stencil1 -> stencil2 -> c)
    -> IRBoundary  Native aenv (Array sh a)
    -> MIRDelayed  Native aenv (Array sh a)
    -> IRBoundary  Native aenv (Array sh b)
    -> MIRDelayed  Native aenv (Array sh b)
    -> CodeGen     Native      (IROpenAcc Native aenv (Array sh c))
mkStencil2 uid aenv f bnd1 marr1 bnd2 marr2 =
  let
      (arrIn1, paramIn1)  = delayedArray "in1" marr1
      (arrIn2, paramIn2)  = delayedArray "in2" marr2

      inside  = IRFun1 $ \ix -> do
        stencil1 <- stencilAccess Nothing arrIn1 ix
        stencil2 <- stencilAccess Nothing arrIn2 ix
        app2 f stencil1 stencil2
      --
      border  = IRFun1 $ \ix -> do
        stencil1 <- stencilAccess (Just bnd1) arrIn1 ix
        stencil2 <- stencilAccess (Just bnd2) arrIn2 ix
        app2 f stencil1 stencil2
  in
  (+++) <$> mkInside uid aenv inside (paramIn1 ++ paramIn2)
        <*> mkBorder uid aenv border (paramIn1 ++ paramIn2)


mkInside
    :: forall aenv sh e. (Shape sh, Elt e)
    => UID
    -> Gamma aenv
    -> IRFun1  Native aenv (sh -> e)
    -> [LLVM.Parameter]
    -> CodeGen Native      (IROpenAcc Native aenv (Array sh e))
mkInside uid aenv apply paramIn =
  let
      (start, end, paramGang)   = gangParam    @sh
      (arrOut, paramOut)        = mutableArray @sh "out"
      paramEnv                  = envParam aenv
      shOut                     = irArrayShape arrOut
  in
  makeOpenAcc uid "stencil_inside" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do

    imapNestFromToTile 4 start end shOut $ \ix i -> do
      r <- app1 apply ix                        -- apply generator function
      writeArray arrOut i r                     -- store result

    return_

mkBorder
    :: forall aenv sh e. (Shape sh, Elt e)
    => UID
    -> Gamma aenv
    -> IRFun1  Native aenv (sh -> e)
    -> [LLVM.Parameter]
    -> CodeGen Native      (IROpenAcc Native aenv (Array sh e))
mkBorder uid aenv apply paramIn =
  let
      (start, end, paramGang)   = gangParam    @sh
      (arrOut, paramOut)        = mutableArray @sh "out"
      paramEnv                  = envParam aenv
      shOut                     = irArrayShape arrOut
  in
  makeOpenAcc uid "stencil_border" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do

    imapNestFromTo start end shOut $ \ix i -> do
      r <- app1 apply ix                        -- apply generator function
      writeArray arrOut i r                     -- store result

    return_


imapNestFromToTile
    :: forall sh. Shape sh
    => Int                                      -- ^ unroll amount (tile height)
    -> IR sh                                    -- ^ initial index (inclusive)
    -> IR sh                                    -- ^ final index (exclusive)
    -> IR sh                                    -- ^ total array extent
    -> (IR sh -> IR Int -> CodeGen Native ())   -- ^ apply at each index
    -> CodeGen Native ()
imapNestFromToTile unroll (IR start) (IR end) extent body =
  go (eltType @sh) start end (body' . IR)
  where
    body' ix = body ix =<< intOfIndex extent ix

    go :: TupleType t
       -> Operands t
       -> Operands t
       -> (Operands t -> CodeGen Native ())
       -> CodeGen Native ()
    go TypeRunit OP_Unit OP_Unit k
      = k OP_Unit

    -- To correctly generate the unrolled loop nest we need to explicitly match
    -- on the last two dimensions.
    --
    go (TypeRpair (TypeRpair TypeRunit tsy) tsx) (OP_Pair (OP_Pair OP_Unit sy) sx) (OP_Pair (OP_Pair OP_Unit ey) ex) k
      | TypeRscalar ty  <- tsy
      , TypeRscalar tx  <- tsx
      , Just Refl       <- matchScalarType ty (scalarType :: ScalarType Int)
      , Just Refl       <- matchScalarType tx (scalarType :: ScalarType Int)
      = do
          -- Tile the stencil operator in the xy-plane by unrolling in the
          -- y-dimension and vectorising in the x-dimension.
          --
          IR sy'  <- imapFromStepTo (IR sy) (lift unroll) (IR ey) $ \iy      ->
                      imapFromTo    (IR sx)               (IR ex) $ \(IR ix) ->
                       forM_ [0 .. unroll-1] $ \n -> do
                        IR iy' <- add numType iy (lift n)
                        k (OP_Pair (OP_Pair OP_Unit iy') ix)

          -- Take care of any remaining loop iterations in the y-dimension
          --
          _       <- imapFromTo  (IR sy') (IR ey) $ \(IR iy) ->
                      imapFromTo (IR sx)  (IR ex) $ \(IR ix) ->
                        k (OP_Pair (OP_Pair OP_Unit iy) ix)
          return ()

    -- The 1- and 3+-dimensional cases can recurse normally
    --
    go (TypeRpair tsh tsz) (OP_Pair ssh ssz) (OP_Pair esh esz) k
      | TypeRscalar t   <- tsz
      , Just Refl       <- matchScalarType t (scalarType :: ScalarType Int)
      = go tsh ssh esh
      $ \sz      -> imapFromTo (IR ssz) (IR esz)
      $ \(IR i)  -> k (OP_Pair sz i)

    go _ _ _ _
      = $internalError "imapNestFromTo" "expected shape with Int components"


imapFromStepTo
    :: IR Int
    -> IR Int
    -> IR Int
    -> (IR Int -> CodeGen Native ())
    -> CodeGen Native (IR Int)
imapFromStepTo start step end body =
  let
      incr i = add numType i step
      test i = do i' <- incr i
                  lt singleType i' end
  in
  while test
        (\i -> body i >> incr i)
        start

