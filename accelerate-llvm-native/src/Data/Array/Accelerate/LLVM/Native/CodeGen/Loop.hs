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
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
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

