{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Native.Loop
-- Copyright   : [2014..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
import qualified Data.Array.Accelerate.LLVM.CodeGen.Loop        as Loop


-- | A standard 'for' loop, that steps from the start to end index executing the
-- given function at each index.
--
imapFromTo
    :: IR Int                                   -- ^ starting index (inclusive)
    -> IR Int                                   -- ^ final index (exclusive)
    -> (IR Int -> CodeGen ())                   -- ^ apply at each index
    -> CodeGen ()
imapFromTo start end body =
  Loop.imapFromStepTo start (lift 1) end body


-- | Generate a series of nested 'for' loops which iterate between the start and
-- end indices of a given hyper-rectangle. LLVM is very good at vectorising
-- these kinds of nested loops, but not so good at vectorising the flattened
-- representation utilising to/from index.
--
imapNestedFromTo
    :: forall sh. Shape sh
    => IR sh                                    -- ^ initial index (inclusive)
    -> IR sh                                    -- ^ final index (exclusive)
    -> IR sh                                    -- ^ total array extent
    -> (IR sh -> IR Int -> CodeGen ())          -- ^ apply at each index
    -> CodeGen ()
imapNestedFromTo (IR start) (IR end) extent body = go (eltType (undefined::sh)) start end (body' . IR)
  where
    body' ix = body ix =<< intOfIndex extent ix

    go :: TupleType t -> Operands t -> Operands t -> (Operands t -> CodeGen ()) -> CodeGen ()
    go UnitTuple OP_Unit OP_Unit k
      = k OP_Unit

    go (PairTuple tsh tsz) (OP_Pair ssh ssz) (OP_Pair esh esz) k
      | SingleTuple t <- tsz
      , Just Refl     <- matchScalarType t (scalarType :: ScalarType Int)
      = go tsh ssh esh
      $ \sz      -> imapFromTo (IR ssz) (IR esz)
      $ \(IR i)  -> k (OP_Pair sz i)

    go _ _ _ _
      = $internalError "imapNestedFromTo" "expected shape with Int components"


-- | Iterate with an accumulator between the start and end index, executing the
-- given function at each.
--
iterFromTo
    :: Elt a
    => IR Int                                   -- ^ starting index (inclusive)
    -> IR Int                                   -- ^ final index (exclusive)
    -> IR a                                     -- ^ initial value
    -> (IR Int -> IR a -> CodeGen (IR a))       -- ^ apply at each index
    -> CodeGen (IR a)
iterFromTo start end seed body =
  Loop.iterFromStepTo start (lift 1) end seed body

