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

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad


-- | A standard 'for' loop.
--
for :: forall i. (Elt i, IsIntegral i)
    => IR i                                     -- ^ starting index
    -> (IR i -> CodeGen (IR Bool))              -- ^ loop test to keep going
    -> (IR i -> CodeGen (IR i))                 -- ^ increment loop counter
    -> (IR i -> CodeGen (IR ()))                -- ^ body of the loop
    -> CodeGen ()
for start test incr body = do
  loop  <- newBlock "for.top"
  exit  <- newBlock "for.exit"
  _     <- beginBlock "for.entry"

  -- entry
  p     <- test start
  top   <- cbr (op nonNumType p) loop exit

  -- loop body
  setBlock loop
  ci    <- freshName
  let i  = local scalarType ci

  _     <- body i
  i'    <- incr i
  c'    <- test i'
  bot   <- cbr (op nonNumType c') loop exit

  -- append the phi instruction at the loop head to set the loop count
  _     <- phi' loop ci [(op integralType i',bot), (op integralType start, top)]

  setBlock exit

