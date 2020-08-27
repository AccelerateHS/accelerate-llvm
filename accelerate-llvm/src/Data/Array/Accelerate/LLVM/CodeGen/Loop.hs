{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Loop
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Loop
  where

import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad

import Prelude                                                  hiding ( fst, snd, uncurry )
import Control.Monad


-- | TODO: Iterate over a multidimensional index space.
--
-- Build nested loops that iterate over a hyper-rectangular index space between
-- the given coordinates. The LLVM optimiser will be able to vectorise nested
-- loops, including when we insert conversions to the corresponding linear index
-- (e.g., in order to index arrays).
--
-- iterate
--     :: Shape sh
--     => Operands sh                                    -- ^ starting index
--     -> Operands sh                                    -- ^ final index
--     -> (Operands sh -> CodeGen (Operands a))          -- ^ body of the loop
--     -> CodeGen (Operands a)
-- iterate from to body = error "CodeGen.Loop.iterate"


-- | Execute the given function at each index in the range
--
imapFromStepTo
    :: forall i arch. IsNum i
    => Operands i                                     -- ^ starting index (inclusive)
    -> Operands i                                     -- ^ step size
    -> Operands i                                     -- ^ final index (exclusive)
    -> (Operands i -> CodeGen arch ())                -- ^ loop body
    -> CodeGen arch ()
imapFromStepTo start step end body =
  for (TupRsingle $ SingleScalarType $ NumSingleType num) start
      (\i -> lt (NumSingleType num) i end)
      (\i -> add num i step)
      body
  where num = numType @i


-- | Iterate with an accumulator between given start and end indices, executing
-- the given function at each.
--
iterFromStepTo
    :: forall i a arch. IsNum i
    => TypeR a
    -> Operands i                                     -- ^ starting index (inclusive)
    -> Operands i                                     -- ^ step size
    -> Operands i                                     -- ^ final index (exclusive)
    -> Operands a                                     -- ^ initial value
    -> (Operands i -> Operands a -> CodeGen arch (Operands a))    -- ^ loop body
    -> CodeGen arch (Operands a)
iterFromStepTo tp start step end seed body =
  iter (TupRsingle $ SingleScalarType $ NumSingleType num) tp start seed
       (\i -> lt (NumSingleType num) i end)
       (\i -> add num i step)
       body
  where num = numType @i


-- | A standard 'for' loop.
--
for :: TypeR i
    -> Operands i                                         -- ^ starting index
    -> (Operands i -> CodeGen arch (Operands Bool))       -- ^ loop test to keep going
    -> (Operands i -> CodeGen arch (Operands i))          -- ^ increment loop counter
    -> (Operands i -> CodeGen arch ())                    -- ^ body of the loop
    -> CodeGen arch ()
for tp start test incr body =
  void $ while tp test (\i -> body i >> incr i) start


-- | An loop with iteration count and accumulator.
--
iter :: TypeR i
     -> TypeR a
     -> Operands i                                                -- ^ starting index
     -> Operands a                                                -- ^ initial value
     -> (Operands i -> CodeGen arch (Operands Bool))              -- ^ index test to keep looping
     -> (Operands i -> CodeGen arch (Operands i))                 -- ^ increment loop counter
     -> (Operands i -> Operands a -> CodeGen arch (Operands a))   -- ^ loop body
     -> CodeGen arch (Operands a)
iter tpi tpa start seed test incr body = do
  r <- while (TupRpair tpi tpa)
             (test . fst)
             (\v -> do v' <- uncurry body v     -- update value and then...
                       i' <- incr (fst v)       -- ...calculate new index
                       return $ pair i' v')
             (pair start seed)
  return $ snd r


-- | A standard 'while' loop
--
while :: TypeR a
      -> (Operands a -> CodeGen arch (Operands Bool))
      -> (Operands a -> CodeGen arch (Operands a))
      -> Operands a
      -> CodeGen arch (Operands a)
while tp test body start = do
  loop <- newBlock   "while.top"
  exit <- newBlock   "while.exit"
  _    <- beginBlock "while.entry"

  -- Entry: generate the initial value
  p    <- test start
  top  <- cbr p loop exit

  -- Create the critical variable that will be used to accumulate the results
  prev <- fresh tp

  -- Generate the loop body. Afterwards, we insert a phi node at the head of the
  -- instruction stream, which selects the input value depending on which edge
  -- we entered the loop from: top or bottom.
  --
  setBlock loop
  next <- body prev
  p'   <- test next
  bot  <- cbr p' loop exit

  _    <- phi' tp loop prev [(start,top), (next,bot)]

  -- Now the loop exit
  setBlock exit
  phi tp [(start,top), (next,bot)]

