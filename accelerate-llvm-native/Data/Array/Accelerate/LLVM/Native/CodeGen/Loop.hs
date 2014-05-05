-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Native.Loop
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
  where

-- llvm-general
import LLVM.General.AST

-- accelerate
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type
import qualified Data.Array.Accelerate.LLVM.CodeGen.Loop        as Loop


-- | A standard 'for' loop, that steps from the start to end index executing the
-- given function at each index.
--
imapFromTo :: Operand -> Operand -> (Operand -> CodeGen ()) -> CodeGen ()
imapFromTo start end body =
  Loop.for (typeOf (int :: IntegralType Int))
           start
           (\i -> lt int i end)
           (\i -> add int i (constOp (num int 1)))
           body

-- | Iterate with an accumulator between the start and end index, executing the
-- given function at each.
--
iterFromTo
    :: Operand
    -> Operand
    -> [Type]
    -> [Operand]
    -> (Operand -> [Operand] -> CodeGen [Operand])
    -> CodeGen [Operand]
iterFromTo start end tacc acc body =
  Loop.iter (typeOf (int :: IntegralType Int))
            start
            (\i -> lt int i end)
            (\i -> add int i (constOp (num int 1)))
            tacc acc body

