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

-- accelerate
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import qualified Data.Array.Accelerate.LLVM.CodeGen.Loop        as Loop


-- | A standard 'for' loop, that steps from the start to end index executing the
-- given function at each index.
--
imapFromTo :: IR Int -> IR Int -> (IR Int -> CodeGen ()) -> CodeGen ()
imapFromTo start end body =
  Loop.for start
           (\i -> lt scalarType i end)
           (\i -> add numType i (ir numType (num numType 1)))
           body

{--
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
--}

