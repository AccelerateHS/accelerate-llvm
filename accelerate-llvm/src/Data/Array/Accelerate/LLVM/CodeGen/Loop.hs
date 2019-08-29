{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Loop
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Loop
  where

import Prelude                                                  hiding ( fst, snd, uncurry )
import Control.Monad

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar                        hiding ( iter )

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad


-- | TODO: Iterate over a multidimensional index space.
--
-- Build nested loops that iterate over a hyper-rectangular index space between
-- the given coordinates. The LLVM optimiser will be able to vectorise nested
-- loops, including when we insert conversions to the corresponding linear index
-- (e.g., in order to index arrays).
--
-- iterate
--     :: Shape sh
--     => IR sh                                    -- ^ starting index
--     -> IR sh                                    -- ^ final index
--     -> (IR sh -> CodeGen (IR a))                -- ^ body of the loop
--     -> CodeGen (IR a)
-- iterate from to body = error "CodeGen.Loop.iterate"


-- | Execute the given function at each index in the range
--
imapFromStepTo
    :: (IsNum i, Elt i)
    => IR i                                     -- ^ starting index (inclusive)
    -> IR i                                     -- ^ step size
    -> IR i                                     -- ^ final index (exclusive)
    -> (IR i -> CodeGen arch ())                -- ^ loop body
    -> CodeGen arch ()
imapFromStepTo start step end body =
  for start
      (\i -> lt singleType i end)
      (\i -> add numType i step)
      body


-- | Iterate with an accumulator between given start and end indices, executing
-- the given function at each.
--
iterFromStepTo
    :: (IsNum i, Elt i, Elt a)
    => IR i                                     -- ^ starting index (inclusive)
    -> IR i                                     -- ^ step size
    -> IR i                                     -- ^ final index (exclusive)
    -> IR a                                     -- ^ initial value
    -> (IR i -> IR a -> CodeGen arch (IR a))    -- ^ loop body
    -> CodeGen arch (IR a)
iterFromStepTo start step end seed body =
  iter start seed
       (\i -> lt singleType i end)
       (\i -> add numType i step)
       body


-- | A standard 'for' loop.
--
for :: Elt i
    => IR i                                     -- ^ starting index
    -> (IR i -> CodeGen arch (IR Bool))         -- ^ loop test to keep going
    -> (IR i -> CodeGen arch (IR i))            -- ^ increment loop counter
    -> (IR i -> CodeGen arch ())                -- ^ body of the loop
    -> CodeGen arch ()
for start test incr body =
  void $ while test (\i -> body i >> incr i) start


-- | An loop with iteration count and accumulator.
--
iter :: (Elt i, Elt a)
     => IR i                                    -- ^ starting index
     -> IR a                                    -- ^ initial value
     -> (IR i -> CodeGen arch (IR Bool))        -- ^ index test to keep looping
     -> (IR i -> CodeGen arch (IR i))           -- ^ increment loop counter
     -> (IR i -> IR a -> CodeGen arch (IR a))   -- ^ loop body
     -> CodeGen arch (IR a)
iter start seed test incr body = do
  r <- while (test . fst)
             (\v -> do v' <- uncurry body v     -- update value and then...
                       i' <- incr (fst v)       -- ...calculate new index
                       return $ pair i' v')
             (pair start seed)
  return $ snd r


-- | A standard 'while' loop
--
while :: Elt a
      => (IR a -> CodeGen arch (IR Bool))
      -> (IR a -> CodeGen arch (IR a))
      -> IR a
      -> CodeGen arch (IR a)
while test body start = do
  loop <- newBlock   "while.top"
  exit <- newBlock   "while.exit"
  _    <- beginBlock "while.entry"

  -- Entry: generate the initial value
  p    <- test start
  top  <- cbr p loop exit

  -- Create the critical variable that will be used to accumulate the results
  prev <- fresh

  -- Generate the loop body. Afterwards, we insert a phi node at the head of the
  -- instruction stream, which selects the input value depending on which edge
  -- we entered the loop from: top or bottom.
  --
  setBlock loop
  next <- body prev
  p'   <- test next
  bot  <- cbr p' loop exit

  _    <- phi' loop prev [(start,top), (next,bot)]

  -- Now the loop exit
  setBlock exit
  phi [(start,top), (next,bot)]

