{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Native.Loop
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
  where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type

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
    :: IR Int                                   -- ^ starting index (inclusive)
    -> IR Int                                   -- ^ final index (exclusive)
    -> (IR Int -> CodeGen Native ())            -- ^ apply at each index
    -> CodeGen Native ()
imapFromTo start end body =
  Loop.imapFromStepTo start (lift 1) end body


-- | Generate a series of nested 'for' loops which iterate between the start and
-- end indices of a given hyper-rectangle. LLVM is very good at vectorising
-- these kinds of nested loops, but not so good at vectorising the flattened
-- representation utilising to/from index.
--
imapNestFromTo
    :: forall sh. Shape sh
    => IR sh                                    -- ^ initial index (inclusive)
    -> IR sh                                    -- ^ final index (exclusive)
    -> IR sh                                    -- ^ total array extent
    -> (IR sh -> IR Int -> CodeGen Native ())   -- ^ apply at each index
    -> CodeGen Native ()
imapNestFromTo (IR start) (IR end) extent body =
  go (eltType @sh) start end (body' . IR)
  where
    body' ix = body ix =<< intOfIndex extent ix

    go :: TupleType t -> Operands t -> Operands t -> (Operands t -> CodeGen Native ()) -> CodeGen Native ()
    go TypeRunit OP_Unit OP_Unit k
      = k OP_Unit

    go (TypeRpair tsh tsz) (OP_Pair ssh ssz) (OP_Pair esh esz) k
      | TypeRscalar t <- tsz
      , Just Refl     <- matchScalarType t (scalarType :: ScalarType Int)
      = go tsh ssh esh
      $ \sz      -> imapFromTo (IR ssz) (IR esz)
      $ \(IR i)  -> k (OP_Pair sz i)

    go _ _ _ _
      = $internalError "imapNestFromTo" "expected shape with Int components"


{--
-- TLM: this version (seems to) compute the corresponding linear index as it
--      goes. We need to compare it against the above implementation to see if
--      there are any advantages.
--
imapNestFromTo'
    :: forall sh. Shape sh
    => IR sh
    -> IR sh
    -> IR sh
    -> (IR sh -> IR Int -> CodeGen Native ())
    -> CodeGen Native ()
imapNestFromTo' (IR start) (IR end) (IR extent) body = do
  startl <- intOfIndex (IR extent :: IR sh) (IR start)
  void $ go (eltType @sh) start end extent (int 1) startl body'
  where
    body' :: Operands (EltRepr sh) -> IR Int -> CodeGen Native (IR Int)
    body' ix l = body (IR ix) l >> add numType (int 1) l

    go :: TupleType t
       -> Operands t
       -> Operands t
       -> Operands t
       -> IR Int
       -> IR Int
       -> (Operands t -> IR Int -> CodeGen Native (IR Int))
       -> CodeGen Native (IR Int)
    go TypeRunit OP_Unit OP_Unit OP_Unit _delta l k
      = k OP_Unit l

    go (TypeRpair tsh tsz) (OP_Pair ssh ssz) (OP_Pair esh esz) (OP_Pair exh exz) delta l k
      | TypeRscalar t <- tsz
      , Just Refl     <- matchScalarType t (scalarType :: ScalarType Int)
      = do
          delta' <- mul numType delta (IR exz)
          go tsh ssh esh exh delta' l $ \sz ll -> do
            Loop.iterFromStepTo (IR ssz) (int 1) (IR esz) ll $ \(IR i) l' ->
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
    => IR sh                                    -- ^ initial index (inclusive)
    -> IR sh                                    -- ^ steps
    -> IR sh                                    -- ^ final index (exclusive)
    -> IR sh                                    -- ^ total array extent
    -> (IR sh -> IR Int -> CodeGen Native ())   -- ^ apply at each index
    -> CodeGen Native ()
imapNestFromStepTo (IR start) (IR steps) (IR end) extent body =
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
      $ \sz      -> Loop.imapFromStepTo (IR ssz :: IR Int) (IR stz) (IR esz)
      $ \(IR i)  -> k (OP_Pair sz i)

    go _ _ _ _ _
      = $internalError "imapNestFromTo" "expected shape with Int components"
--}

-- | Iterate with an accumulator between the start and end index, executing the
-- given function at each.
--
iterFromTo
    :: Elt a
    => IR Int                                       -- ^ starting index (inclusive)
    -> IR Int                                       -- ^ final index (exclusive)
    -> IR a                                         -- ^ initial value
    -> (IR Int -> IR a -> CodeGen Native (IR a))    -- ^ apply at each index
    -> CodeGen Native (IR a)
iterFromTo start end seed body =
  Loop.iterFromStepTo start (lift 1) end seed body

