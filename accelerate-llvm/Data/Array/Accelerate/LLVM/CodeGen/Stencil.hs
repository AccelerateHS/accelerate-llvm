{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Stencil
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Stencil
  where

-- accelerate
import Data.Array.Accelerate.AST                                hiding ( Val(..), prj, stencilAccess )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                        hiding ( bound )
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic            ( ifThenElse )
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic  as A

import Control.Applicative
import Prelude


-- Generate the stencil pattern including boundary conditions
--
stencilAccess
    :: Stencil sh e stencil
    => Boundary (IR e)
    -> IRArray (Array sh e)
    -> IR sh
    -> CodeGen (IR stencil)
stencilAccess bndy arr = goR stencil (bounded bndy arr)
  where
    -- Base cases, nothing interesting to do here since we know the lower
    -- dimension is Z.
    --
    goR :: StencilR sh e stencil -> (IR sh -> CodeGen (IR e)) -> IR sh -> CodeGen (IR stencil)
    goR StencilRunit3 rf ix
      = let z :. i = unindex ix
            rf' d  = do d' <- A.add numType i (int d)
                        rf (index z d')
        in
        tup3 <$> rf' (-1)
             <*> rf   ix
             <*> rf'   1

    goR StencilRunit5 rf ix
      = let z :. i = unindex ix
            rf' d  = do d' <- A.add numType i (int d)
                        rf (index z d')
        in
        tup5 <$> rf' (-2)
             <*> rf' (-1)
             <*> rf   ix
             <*> rf'   1
             <*> rf'   2

    goR StencilRunit7 rf ix
      = let z :. i = unindex ix
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

    goR StencilRunit9 rf ix
      = let z :. i = unindex ix
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
    -- _innermost_ index component
    --
    goR (StencilRtup3 s1 s2 s3) rf ix =
      let (i, ix') = uncons ix
          rf' 0 ds = rf (cons i ds)
          rf' d ds = do d' <- A.add numType i (int d)
                        rf (cons d' ds)
      in
      tup3 <$> goR s1 (rf' (-1)) ix'
           <*> goR s2 (rf'   0)  ix'
           <*> goR s3 (rf'   1)  ix'

    goR (StencilRtup5 s1 s2 s3 s4 s5) rf ix =
      let (i, ix') = uncons ix
          rf' 0 ds = rf (cons i ds)
          rf' d ds = do d' <- A.add numType i (int d)
                        rf (cons d' ds)
      in
      tup5 <$> goR s1 (rf' (-2)) ix'
           <*> goR s2 (rf' (-1)) ix'
           <*> goR s3 (rf'   0)  ix'
           <*> goR s4 (rf'   1)  ix'
           <*> goR s5 (rf'   2)  ix'

    goR (StencilRtup7 s1 s2 s3 s4 s5 s6 s7) rf ix =
      let (i, ix') = uncons ix
          rf' 0 ds = rf (cons i ds)
          rf' d ds = do d' <- A.add numType i (int d)
                        rf (cons d' ds)
      in
      tup7 <$> goR s1 (rf' (-3)) ix'
           <*> goR s2 (rf' (-2)) ix'
           <*> goR s3 (rf' (-1)) ix'
           <*> goR s4 (rf'   0)  ix'
           <*> goR s5 (rf'   1)  ix'
           <*> goR s6 (rf'   2)  ix'
           <*> goR s7 (rf'   3)  ix'

    goR (StencilRtup9 s1 s2 s3 s4 s5 s6 s7 s8 s9) rf ix =
      let (i, ix') = uncons ix
          rf' 0 ds = rf (cons i ds)
          rf' d ds = do d' <- A.add numType i (int d)
                        rf (cons d' ds)
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


-- Apply boundary conditions to the given index
--
bounded
    :: (Shape sh, Elt e)
    => Boundary (IR e)
    -> IRArray (Array sh e)
    -> IR sh
    -> CodeGen (IR e)
bounded bndy arr@IRArray{..} ix =
  case bndy of
    Constant v ->
      if inside irArrayShape ix
        then do i <- intOfIndex irArrayShape ix
                readArray arr i
        else return v
    _          -> do
      ix' <- bound irArrayShape ix
      i   <- intOfIndex irArrayShape ix'
      readArray arr i
  --
  where
    -- Return the index, updated to obey the given boundary conditions (clamp,
    -- mirror, or wrap only).
    --
    bound :: forall sh. Shape sh => IR sh -> IR sh -> CodeGen (IR sh)
    bound (IR extent1) (IR extent2) = IR <$> go (eltType (undefined::sh)) extent1 extent2
      where
        go :: TupleType t -> Operands t -> Operands t -> CodeGen (Operands t)
        go UnitTuple OP_Unit OP_Unit
          = return OP_Unit
        go (PairTuple tsh ti) (OP_Pair sh sz) (OP_Pair ih iz)
          = do
               ix' <- go tsh sh ih
               i'  <- go ti  sz iz
               return $ OP_Pair ix' i'
        go (SingleTuple t) sz iz
          | Just Refl <- matchScalarType t (scalarType :: ScalarType Int)
          = do
               IR i' <- if A.lt t (IR iz) (int 0)
                          then
                            case bndy of
                              Clamp      -> return (int 0)
                              Mirror     -> A.negate numType (IR iz)
                              Wrap       -> A.add    numType (IR sz) (IR iz)
                              Constant _ -> $internalError "bound" "unexpected boundary condition"
                          else
                            if A.gte t (IR iz) (IR sz)
                              then
                                case bndy of
                                  Clamp      -> A.sub numType (IR sz) (int 1)
                                  Mirror     -> do
                                    a <- A.add numType (IR sz) (int 2)
                                    b <- A.sub numType (IR iz) a
                                    c <- A.sub numType (IR sz) b
                                    return c
                                  Wrap       -> A.sub numType (IR iz) (IR sz)
                                  Constant _ -> $internalError "bound" "unexpected boundary condition"
                              else
                                return (IR iz)
               return i'
          | otherwise
          = $internalError "bound" "expected shape with Int components"

    -- Return whether the index is inside the bounds of the given shape
    --
    inside :: forall sh. Shape sh => IR sh -> IR sh -> CodeGen (IR Bool)
    inside (IR extent1) (IR extent2) = go (eltType (undefined::sh)) extent1 extent2
      where
        go :: TupleType t -> Operands t -> Operands t -> CodeGen (IR Bool)
        go UnitTuple OP_Unit OP_Unit
          = return (bool True)
        go (PairTuple tsh ti) (OP_Pair sh sz) (OP_Pair ih iz)
          = if go ti sz iz
              then go tsh sh ih
              else return (bool False)
        go (SingleTuple t) sz iz
          | Just Refl <- matchScalarType t (scalarType :: ScalarType Int)
          = if A.lt t (IR iz) (int 0)
              then return (bool False)
              else
                if A.gte t (IR iz) (IR sz)
                  then return (bool False)
                  else return (bool True)
          --
          | otherwise
          = $internalError "bound" "expected shape with Int components"


-- Utilities
-- ---------

int :: Int -> IR Int
int x = IR (constant (eltType (undefined::Int)) x)

bool :: Bool -> IR Bool
bool b = IR (constant (eltType (undefined::Bool)) b)

unindex :: IR (sh :. Int) -> IR sh :. IR Int
unindex (IR (OP_Pair sh i)) = IR sh :. IR i

index :: IR sh -> IR Int -> IR (sh :. Int)
index (IR sh) (IR i) = IR (OP_Pair sh i)

tup3 :: IR a -> IR b -> IR c -> IR (a,b,c)
tup3 (IR a) (IR b) (IR c) = IR $ OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c

tup5 :: IR a -> IR b -> IR c -> IR d -> IR e -> IR (a,b,c,d,e)
tup5 (IR a) (IR b) (IR c) (IR d) (IR e) =
  IR $ OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c) d) e

tup7 :: IR a -> IR b -> IR c -> IR d -> IR e -> IR f -> IR g -> IR (a,b,c,d,e,f,g)
tup7 (IR a) (IR b) (IR c) (IR d) (IR e) (IR f) (IR g) =
  IR $ OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c) d) e) f) g

tup9 :: IR a -> IR b -> IR c -> IR d -> IR e -> IR f -> IR g -> IR h -> IR i -> IR (a,b,c,d,e,f,g,h,i)
tup9 (IR a) (IR b) (IR c) (IR d) (IR e) (IR f) (IR g) (IR h) (IR i) =
  IR $ OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair (OP_Pair OP_Unit a) b) c) d) e) f) g) h) i


-- Add an _innermost_ dimension to a shape
--
cons :: forall sh. Shape sh => IR Int -> IR sh -> IR (sh :. Int)
cons (IR ix) (IR extent) = IR $ go (eltType (undefined::sh)) extent
  where
    go :: TupleType t -> Operands t -> Operands (t,Int)
    go UnitTuple OP_Unit                 = OP_Pair OP_Unit ix
    go (PairTuple th tz) (OP_Pair sh sz)
      | SingleTuple t <- tz
      , Just Refl     <- matchScalarType t (scalarType :: ScalarType Int)
      = OP_Pair (go th sh) sz
    go _ _
      = $internalError "cons" "expected shape with Int components"


-- Remove the _innermost_ dimension to a shape, and return the remainder
--
uncons :: forall sh. Shape sh => IR (sh :. Int) -> (IR Int, IR sh)
uncons (IR extent) = let (ix, extent') = go (eltType (undefined::(sh :. Int))) extent
                     in  (IR ix, IR extent')
  where
    go :: TupleType (t, Int) -> Operands (t, Int) -> (Operands Int, Operands t)
    go (PairTuple UnitTuple _) (OP_Pair OP_Unit v2)      = (v2, OP_Unit)
    go (PairTuple t1@(PairTuple _ t2) _) (OP_Pair v1 v3)
      | SingleTuple t <- t2
      , Just Refl     <- matchScalarType t (scalarType :: ScalarType Int)
      = let (i, v1') = go t1 v1
        in  (i, OP_Pair v1' v3)
    go _ _
      = $internalError "uncons" "expected shape with Int components"

