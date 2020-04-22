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
  IRConstant  :: IR e -> IRBoundary arch aenv (Array sh e)
  IRFunction  :: IRFun1 arch aenv (sh -> e) -> IRBoundary arch aenv (Array sh e)


-- Generate the stencil pattern including boundary conditions
--
stencilAccess
    :: StencilR sh e stencil
    -> Maybe (IRBoundary arch aenv (Array sh e))
    ->        IRDelayed  arch aenv (Array sh e)
    -> IR sh
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
        -> (IR sh -> IRExp arch aenv e)
        -> IR sh
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
    -> IR sh
    -> IRExp arch aenv e
inbounds IRDelayed{..} ix =
  app1 delayedIndex ix


-- Apply boundary conditions to the given index
--
bounded
    :: IRBoundary arch aenv (Array sh e)
    -> IRDelayed  arch aenv (Array sh e)
    -> IR sh
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
    bound :: ShapeR sh -> IR sh -> IR sh -> CodeGen arch (IR sh)
    bound shr (IR extent1) (IR extent2) = IR <$> go shr extent1 extent2
      where
        go :: ShapeR t -> Operands t -> Operands t -> CodeGen arch (Operands t)
        go ShapeRz OP_Unit OP_Unit
          = return OP_Unit
        go (ShapeRsnoc shr') (OP_Pair sh sz) (OP_Pair ih iz)
          = do
               ix' <- go shr' sh ih
               IR i' <- if ( TupRsingle scalarTypeInt
                           , A.lt (singleType :: SingleType Int) (IR iz) (int 0))
                          then
                            case bndy of
                              IRClamp  -> return (int 0)
                              IRMirror -> A.negate numType (IR iz)
                              IRWrap   -> A.add    numType (IR sz) (IR iz)
                              _        -> $internalError "bound" "unexpected boundary condition"
                          else
                            if ( TupRsingle scalarTypeInt
                               , A.gte (singleType :: SingleType Int) (IR iz) (IR sz))
                              then
                                case bndy of
                                  IRClamp  -> A.sub numType (IR sz) (int 1)
                                  IRWrap   -> A.sub numType (IR iz) (IR sz)
                                  IRMirror -> do
                                    a <- A.sub numType (IR iz) (IR sz)
                                    b <- A.add numType a (int 2)
                                    c <- A.sub numType (IR sz) b
                                    return c
                                  _        -> $internalError "bound" "unexpected boundary condition"
                              else
                                return (IR iz)
               return $ OP_Pair ix' i'

    -- Return whether the index is inside the bounds of the given shape
    --
    inside :: forall arch sh. ShapeR sh -> IR sh -> IR sh -> CodeGen arch (IR Bool)
    inside shr (IR extent1) (IR extent2) = go shr extent1 extent2
      where
        go :: ShapeR t -> Operands t -> Operands t -> CodeGen arch (IR Bool)
        go ShapeRz OP_Unit OP_Unit
          = return (bool True)
        go (ShapeRsnoc shr') (OP_Pair sh sz) (OP_Pair ih iz)
          = if ( TupRsingle scalarTypeBool
               , A.lt  (singleType :: SingleType Int) (IR iz) (int 0) `A.lor'`
                 A.gte (singleType :: SingleType Int) (IR iz) (IR sz))
              then return (bool False)
              else go shr' sh ih


-- Utilities
-- ---------

int :: Int -> IR Int
int x = IR (constant (TupRsingle scalarTypeInt) x)

bool :: Bool -> IR Bool
bool b = IR (constant (TupRsingle scalarTypeBool) b)

unindex :: IR (sh, Int) -> (IR sh, IR Int)
unindex (IR (OP_Pair sh i)) = (IR sh, IR i)

index :: IR sh -> IR Int -> IR (sh, Int)
index (IR sh) (IR i) = IR (OP_Pair sh i)

tup3 :: IR a -> IR b -> IR c -> IR (Tup3 a b c)
tup3 (IR a) (IR b) (IR c) = IR $ OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c

tup5 :: IR a -> IR b -> IR c -> IR d -> IR e -> IR (Tup5 a b c d e)
tup5 (IR a) (IR b) (IR c) (IR d) (IR e) =
  IR $ OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c) d) e

tup7 :: IR a -> IR b -> IR c -> IR d -> IR e -> IR f -> IR g -> IR (Tup7 a b c d e f g)
tup7 (IR a) (IR b) (IR c) (IR d) (IR e) (IR f) (IR g) =
  IR $ OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c) d) e) f) g

tup9 :: IR a -> IR b -> IR c -> IR d -> IR e -> IR f -> IR g -> IR h -> IR i -> IR (Tup9 a b c d e f g h i)
tup9 (IR a) (IR b) (IR c) (IR d) (IR e) (IR f) (IR g) (IR h) (IR i) =
  IR $ OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c) d) e) f) g) h) i


-- Add a _left-most_ dimension to a shape
--
cons :: ShapeR sh -> IR Int -> IR sh -> IR (sh, Int)
cons shr (IR ix) (IR extent) = IR $ go shr extent
  where
    go :: ShapeR sh -> Operands sh -> Operands (sh, Int)
    go ShapeRz          OP_Unit         = OP_Pair OP_Unit ix
    go (ShapeRsnoc shr') (OP_Pair sh sz) = OP_Pair (go shr' sh) sz


-- Remove the _left-most_ index to a shape, and return the remainder
--
uncons :: ShapeR sh -> IR (sh, Int) -> (IR Int, IR sh)
uncons shr (IR extent) = let (ix, extent') = go shr extent
                         in  (IR ix, IR extent')
  where
    go :: ShapeR sh -> Operands (sh, Int) -> (Operands Int, Operands sh)
    go ShapeRz          (OP_Pair OP_Unit v2) = (v2, OP_Unit)
    go (ShapeRsnoc shr') (OP_Pair v1 v3)
      = let (i, v1') = go shr' v1
        in  (i, OP_Pair v1' v3)

