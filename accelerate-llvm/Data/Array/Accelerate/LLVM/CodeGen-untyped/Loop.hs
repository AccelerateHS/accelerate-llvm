{-# LANGUAGE ParallelListComp #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Loop
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Loop
  where

-- llvm-general
import LLVM.General.AST

-- accelerate
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Monad

-- standard library
import Control.Monad


-- | A standard 'for' loop
--
for :: Type                             -- ^ type of the index
    -> Operand                          -- ^ starting index
    -> (Operand -> CodeGen Operand)     -- ^ loop test to keep going
    -> (Operand -> CodeGen Operand)     -- ^ increment the index
    -> (Operand -> CodeGen ())          -- ^ body of the loop
    -> CodeGen ()
for ti start test incr body = do
  loop  <- newBlock "for.top"
  exit  <- newBlock "for.exit"

  -- entry test
  c     <- test start
  top   <- cbr c loop exit

  -- Main loop
  -- ---------
  setBlock loop
  c_i   <- freshName
  let i  = local c_i

  body i

  i'    <- incr i
  c'    <- test i'
  bot   <- cbr c' loop exit
  _     <- phi loop c_i ti [(i',bot), (start,top)]

  setBlock exit


-- | An iteration loop with an accumulator
--
iter :: Type                                            -- ^ The type of the loop counter
     -> Operand                                         -- ^ starting index
     -> (Operand -> CodeGen Operand)                    -- ^ loop test to keep going
     -> (Operand -> CodeGen Operand)                    -- ^ step the index
     -> [Type]                                          -- ^ type of the accumulator
     -> [Operand]                                       -- ^ initial value
     -> (Operand -> [Operand] -> CodeGen [Operand])     -- ^ loop body: index -> a -> a
     -> CodeGen [Operand]
iter ti start test incr ta seed body = do
  loop  <- newBlock "iter.top"
  exit  <- newBlock "iter.exit"

  -- entry test
  c     <- test start
  top   <- cbr c loop exit

  -- Main loop
  -- ---------
  setBlock loop
  crit_i        <- freshName
  crit_acc      <- replicateM (length seed) freshName
  let i         = local crit_i
      acc       = map local crit_acc

  acc'  <- body i acc
  i'    <- incr i
  c'    <- test i'
  bot   <- cbr c' loop exit

  -- set the phi loop
  _     <- phi loop crit_i ti [(i',bot), (start,top)]
  _     <- sequence $ zipWith3 (phi loop) crit_acc ta
              [ [(t,top), (b,bot)] | t <- seed | b <- acc' ]

  setBlock exit
  zipWithM phi' ta [ [(t,top), (b,bot) ] | t <- seed | b <- acc' ]

