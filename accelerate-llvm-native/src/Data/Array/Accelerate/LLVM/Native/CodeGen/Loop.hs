{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Native.Loop
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
  where

-- accelerate
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Representation.Shape

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import qualified Data.Array.Accelerate.LLVM.CodeGen.Loop            as Loop

import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )


-- | A standard 'for' loop, that steps from the start to end index executing the
-- given function at each index.
--
imapFromTo
    :: Operands Int                                   -- ^ starting index (inclusive)
    -> Operands Int                                   -- ^ final index (exclusive)
    -> (Operands Int -> CodeGen Native ())            -- ^ apply at each index
    -> CodeGen Native ()
imapFromTo start end body =
  Loop.imapFromStepTo start (liftInt 1) end body


-- | Generate a series of nested 'for' loops which iterate between the start and
-- end indices of a given hyper-rectangle. LLVM is very good at vectorising
-- these kinds of nested loops, but not so good at vectorising the flattened
-- representation utilising to/from index.
--
imapNestFromTo
    :: ShapeR sh
    -> Operands sh                                          -- ^ initial index (inclusive)
    -> Operands sh                                          -- ^ final index (exclusive)
    -> Operands sh                                          -- ^ total array extent
    -> (Operands sh -> Operands Int -> CodeGen Native ())   -- ^ apply at each index
    -> CodeGen Native ()
imapNestFromTo shr start end extent body =
  go shr start end body'
  where
    body' ix = body ix =<< intOfIndex shr extent ix

    go :: ShapeR t -> Operands t -> Operands t -> (Operands t -> CodeGen Native ()) -> CodeGen Native ()
    go ShapeRz OP_Unit OP_Unit k
      = k OP_Unit

    go (ShapeRsnoc shr') (OP_Pair ssh ssz) (OP_Pair esh esz) k
      = go shr' ssh esh
      $ \sz      -> imapFromTo ssz esz
      $ \i       -> k (OP_Pair sz i)


{--
-- TLM: this version (seems to) compute the corresponding linear index as it
--      goes. We need to compare it against the above implementation to see if
--      there are any advantages.
--
imapNestFromTo'
    :: forall sh. Shape sh
    => Operands sh
    -> Operands sh
    -> Operands sh
    -> (Operands sh -> Operands Int -> CodeGen Native ())
    -> CodeGen Native ()
imapNestFromTo' start end extent body = do
  startl <- intOfIndex extent start
  void $ go (eltType @sh) start end extent (int 1) startl body'
  where
    body' :: Operands (EltRepr sh) -> Operands Int -> CodeGen Native (Operands Int)
    body' ix l = body ix l >> add numType (int 1) l

    go :: TupleType t
       -> Operands t
       -> Operands t
       -> Operands t
       -> Operands Int
       -> Operands Int
       -> (Operands t -> Operands Int -> CodeGen Native (Operands Int))
       -> CodeGen Native (Operands Int)
    go TypeRunit OP_Unit OP_Unit OP_Unit _delta l k
      = k OP_Unit l

    go (TypeRpair tsh tsz) (OP_Pair ssh ssz) (OP_Pair esh esz) (OP_Pair exh exz) delta l k
      | TypeRscalar t <- tsz
      , Just Refl     <- matchScalarType t (scalarType :: ScalarType Int)
      = do
          delta' <- mul numType delta exz
          go tsh ssh esh exh delta' l $ \sz ll -> do
            Loop.iterFromStepTo ssz (int 1) esz ll $ \i l' ->
              k (OP_Pair sz i) l'
            add numType ll delta'

    go _ _ _ _ _ _ _
      = $internalError "imapNestFromTo'" "expected shape with Int components"
--}

{--
-- | Generate a series of nested 'for' loops which iterate between the start and
-- end indices of a given hyper-rectangle. LLVM is very good at vectorising
-- these kinds of nested loops, but not so good at vectorising the flattened
-- representation utilising to/from index.
--
imapNestFromStepTo
    :: forall sh. Shape sh
    => Operands sh                                    -- ^ initial index (inclusive)
    -> Operands sh                                    -- ^ steps
    -> Operands sh                                    -- ^ final index (exclusive)
    -> Operands sh                                    -- ^ total array extent
    -> (Operands sh -> Operands Int -> CodeGen Native ())   -- ^ apply at each index
    -> CodeGen Native ()
imapNestFromStepTo start steps end extent body =
  go (eltType @sh) start steps end (body' . IR)
  where
    body' ix = body ix =<< intOfIndex extent ix

    go :: TupleType t -> Operands t -> Operands t -> Operands t -> (Operands t -> CodeGen Native ()) -> CodeGen Native ()
    go TypeRunit OP_Unit OP_Unit OP_Unit k
      = k OP_Unit

    go (TypeRpair tsh tsz) (OP_Pair ssh ssz) (OP_Pair sts stz) (OP_Pair esh esz) k
      | TypeRscalar t <- tsz
      , Just Refl     <- matchScalarType t (scalarType :: ScalarType Int)
      = go tsh ssh sts esh
      $ \sz      -> Loop.imapFromStepTo ssz stz esz
      $ \i       -> k (OP_Pair sz i)

    go _ _ _ _ _
      = $internalError "imapNestFromTo" "expected shape with Int components"
--}

-- | Iterate with an accumulator between the start and end index, executing the
-- given function at each.
--
iterFromTo
    :: TypeR a
    -> Operands Int                                       -- ^ starting index (inclusive)
    -> Operands Int                                       -- ^ final index (exclusive)
    -> Operands a                                         -- ^ initial value
    -> (Operands Int -> Operands a -> CodeGen Native (Operands a))    -- ^ apply at each index
    -> CodeGen Native (Operands a)
iterFromTo tp start end seed body =
  Loop.iterFromStepTo tp start (liftInt 1) end seed body

