{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Loop
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Loop
  where

import Control.Monad

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad


-- | A standard 'for' loop.
--
for :: forall i. (Elt i, IsIntegral i)
    => IR i                                     -- ^ starting index
    -> (IR i -> CodeGen (IR Bool))              -- ^ loop test to keep going
    -> (IR i -> CodeGen (IR i))                 -- ^ increment loop counter
    -> (IR i -> CodeGen ())                     -- ^ body of the loop
    -> CodeGen ()
for start test incr body =
  void $ while test (\i -> body i >> incr i) start


-- | A standard 'while' loop
--
while :: Elt a
      => (IR a -> CodeGen (IR Bool))
      -> (IR a -> CodeGen (IR a))
      -> IR a
      -> CodeGen (IR a)
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

