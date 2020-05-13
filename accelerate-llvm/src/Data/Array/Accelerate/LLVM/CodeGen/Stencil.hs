{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Stencil
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Stencil
  where

-- accelerate
import Data.Array.Accelerate.AST                                hiding ( Val(..), PreBoundary(..), prj )
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic            ( ifThenElse )
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic  as A

import Control.Applicative
import Prelude


-- Stencil boundary conditions
--
data IRBoundary arch aenv t where
  IRClamp     :: IRBoundary arch aenv t
  IRMirror    :: IRBoundary arch aenv t
  IRWrap      :: IRBoundary arch aenv t
  IRConstant  :: Operands e -> IRBoundary arch aenv (Array sh e)
  IRFunction  :: IRFun1 arch aenv (sh -> e) -> IRBoundary arch aenv (Array sh e)


-- Generate the stencil pattern including boundary conditions
--
stencilAccess
    :: StencilR sh e stencil
    -> Maybe (IRBoundary arch aenv (Array sh e))
    ->        IRDelayed  arch aenv (Array sh e)
    -> Operands sh
    -> IRExp arch aenv stencil
stencilAccess stencilR mbndy arr =
  case mbndy of
    Nothing   -> goR stencilR (inbounds     arr)
    Just bndy -> goR stencilR (bounded bndy arr)
  where
    -- Base cases, nothing interesting to do here since we know the lower
    -- dimension is Z.
    --
    goR :: StencilR sh e stencil
        -> (Operands sh -> IRExp arch aenv e)
        -> Operands sh
        -> IRExp arch aenv stencil
    goR (StencilRunit3 _) rf ix
      = let (z, i) = unindex ix
            rf' d  = do d' <- A.add numType i (int d)
                        rf (index z d')
        in
        tup3 <$> rf' (-1)
             <*> rf   ix
             <*> rf'   1

    goR (StencilRunit5 _) rf ix
      = let (z, i) = unindex ix
            rf' d  = do d' <- A.add numType i (int d)
                        rf (index z d')
        in
        tup5 <$> rf' (-2)
             <*> rf' (-1)
             <*> rf   ix
             <*> rf'   1
             <*> rf'   2

    goR (StencilRunit7 _) rf ix
      = let (z, i) = unindex ix
            rf' d  = do d' <- A.add numType i (int d)
                        rf (index z d')
        in
        tup7 <$> rf' (-3)
             <*> rf' (-2)
             <*> rf' (-1)
             <*> rf   ix
             <*> rf'   1
             <*> rf'   2
             <*> rf'   3

    goR (StencilRunit9 _) rf ix
      = let (z, i) = unindex ix
            rf' d  = do d' <- A.add numType i (int d)
                        rf (index z d')
        in
        tup9 <$> rf' (-4)
             <*> rf' (-3)
             <*> rf' (-2)
             <*> rf' (-1)
             <*> rf   ix
             <*> rf'   1
             <*> rf'   2
             <*> rf'   3
             <*> rf'   4

    -- Recursive cases. Note that because the stencil pattern is defined with
    -- a cons ordering, whereas shapes (indices) are defined as a snoc list,
    -- when we recurse on the stencil structure we must manipulate the
    -- _left-most_ index component
    --
    goR (StencilRtup3 s1 s2 s3) rf ix =
      let shr = stencilShape s1
          (i, ix') = uncons shr ix
          rf' 0 ds = rf (cons shr i ds)
          rf' d ds = do d' <- A.add numType i (int d)
                        rf (cons shr d' ds)
      in
      tup3 <$> goR s1 (rf' (-1)) ix'
           <*> goR s2 (rf'   0)  ix'
           <*> goR s3 (rf'   1)  ix'

    goR (StencilRtup5 s1 s2 s3 s4 s5) rf ix =
      let shr = stencilShape s1
          (i, ix') = uncons shr ix
          rf' 0 ds = rf (cons shr i ds)
          rf' d ds = do d' <- A.add numType i (int d)
                        rf (cons shr d' ds)
      in
      tup5 <$> goR s1 (rf' (-2)) ix'
           <*> goR s2 (rf' (-1)) ix'
           <*> goR s3 (rf'   0)  ix'
           <*> goR s4 (rf'   1)  ix'
           <*> goR s5 (rf'   2)  ix'

    goR (StencilRtup7 s1 s2 s3 s4 s5 s6 s7) rf ix =
      let shr = stencilShape s1
          (i, ix') = uncons shr ix
          rf' 0 ds = rf (cons shr i ds)
          rf' d ds = do d' <- A.add numType i (int d)
                        rf (cons shr d' ds)
      in
      tup7 <$> goR s1 (rf' (-3)) ix'
           <*> goR s2 (rf' (-2)) ix'
           <*> goR s3 (rf' (-1)) ix'
           <*> goR s4 (rf'   0)  ix'
           <*> goR s5 (rf'   1)  ix'
           <*> goR s6 (rf'   2)  ix'
           <*> goR s7 (rf'   3)  ix'

    goR (StencilRtup9 s1 s2 s3 s4 s5 s6 s7 s8 s9) rf ix =
      let shr = stencilShape s1
          (i, ix') = uncons shr ix
          rf' 0 ds = rf (cons shr i ds)
          rf' d ds = do d' <- A.add numType i (int d)
                        rf (cons shr d' ds)
      in
      tup9 <$> goR s1 (rf' (-4)) ix'
           <*> goR s2 (rf' (-3)) ix'
           <*> goR s3 (rf' (-2)) ix'
           <*> goR s4 (rf' (-1)) ix'
           <*> goR s5 (rf'   0)  ix'
           <*> goR s6 (rf'   1)  ix'
           <*> goR s7 (rf'   2)  ix'
           <*> goR s8 (rf'   3)  ix'
           <*> goR s9 (rf'   4)  ix'


-- Do not apply any boundary conditions to the given index
--
inbounds
    :: IRDelayed arch aenv (Array sh e)
    -> Operands sh
    -> IRExp arch aenv e
inbounds IRDelayed{..} ix =
  app1 delayedIndex ix


-- Apply boundary conditions to the given index
--
bounded
    :: IRBoundary arch aenv (Array sh e)
    -> IRDelayed  arch aenv (Array sh e)
    -> Operands sh
    -> IRExp arch aenv e
bounded bndy IRDelayed{..} ix = do
  let ArrayR shr tp = delayedRepr
  sh <- delayedExtent
  case bndy of
    IRConstant v ->
      if ( tp, inside shr sh ix )
        then app1 delayedIndex ix
        else return v
    IRFunction f ->
      if ( tp, inside shr sh ix )
        then app1 delayedIndex ix
        else app1 f ix
    _            -> do
      ix' <- bound shr sh ix
      v   <- app1 delayedIndex ix'
      return v
  --
  where
    -- Return the index, updated to obey the given boundary conditions (clamp,
    -- mirror, or wrap only).
    --
    bound :: ShapeR sh -> Operands sh -> Operands sh -> CodeGen arch (Operands sh)
    bound ShapeRz OP_Unit OP_Unit
      = return OP_Unit
    bound (ShapeRsnoc shr') (OP_Pair sh sz) (OP_Pair ih iz)
      = do
            ix' <- bound shr' sh ih
            i' <- if ( TupRsingle scalarTypeInt
                        , A.lt (singleType :: SingleType Int) iz (int 0))
                      then
                        case bndy of
                          IRClamp  -> return (int 0)
                          IRMirror -> A.negate numType iz
                          IRWrap   -> A.add    numType sz iz
                          _        -> $internalError "bound" "unexpected boundary condition"
                      else
                        if ( TupRsingle scalarTypeInt
                            , A.gte (singleType :: SingleType Int) iz sz)
                          then
                            case bndy of
                              IRClamp  -> A.sub numType sz (int 1)
                              IRWrap   -> A.sub numType iz sz
                              IRMirror -> do
                                a <- A.sub numType iz sz
                                b <- A.add numType a (int 2)
                                c <- A.sub numType sz b
                                return c
                              _        -> $internalError "bound" "unexpected boundary condition"
                          else
                            return iz
            return $ OP_Pair ix' i'

    -- Return whether the index is inside the bounds of the given shape
    --
    inside :: ShapeR sh -> Operands sh -> Operands sh -> CodeGen arch (Operands Bool)
    inside ShapeRz OP_Unit OP_Unit
      = return (bool True)
    inside (ShapeRsnoc shr') (OP_Pair sh sz) (OP_Pair ih iz)
      = if ( TupRsingle scalarTypeBool
            , A.lt  (singleType :: SingleType Int) iz (int 0) `A.lor'`
              A.gte (singleType :: SingleType Int) iz sz)
          then return (bool False)
          else inside shr' sh ih


-- Utilities
-- ---------

int :: Int -> Operands Int
int x = constant (TupRsingle scalarTypeInt) x

bool :: Bool -> Operands Bool
bool b = constant (TupRsingle scalarTypeBool) b

unindex :: Operands (sh, Int) -> (Operands sh, Operands Int)
unindex (OP_Pair sh i) = (sh, i)

index :: Operands sh -> Operands Int -> Operands (sh, Int)
index sh i = OP_Pair sh i

tup3 :: Operands a -> Operands b -> Operands c -> Operands (Tup3 a b c)
tup3 a b c = OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c

tup5 :: Operands a -> Operands b -> Operands c -> Operands d -> Operands e -> Operands (Tup5 a b c d e)
tup5 a b c d e =
  OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c) d) e

tup7 :: Operands a -> Operands b -> Operands c -> Operands d -> Operands e -> Operands f -> Operands g -> Operands (Tup7 a b c d e f g)
tup7 a b c d e f g =
  OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c) d) e) f) g

tup9 :: Operands a -> Operands b -> Operands c -> Operands d -> Operands e -> Operands f -> Operands g -> Operands h -> Operands i -> Operands (Tup9 a b c d e f g h i)
tup9 a b c d e f g h i =
  OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c) d) e) f) g) h) i


-- Add a _left-most_ dimension to a shape
--
cons :: ShapeR sh -> Operands Int -> Operands sh -> Operands (sh, Int)
cons ShapeRz          ix OP_Unit         = OP_Pair OP_Unit ix
cons (ShapeRsnoc shr) ix (OP_Pair sh sz) = OP_Pair (cons shr ix sh) sz


-- Remove the _left-most_ index to a shape, and return the remainder
--
uncons :: ShapeR sh -> Operands (sh, Int) -> (Operands Int, Operands sh)
uncons ShapeRz          (OP_Pair OP_Unit v2) = (v2, OP_Unit)
uncons (ShapeRsnoc shr) (OP_Pair v1 v3)
  = let (i, v1') = uncons shr v1
    in  (i, OP_Pair v1' v3)

