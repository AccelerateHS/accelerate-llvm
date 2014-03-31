-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Loop
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.CodeGen.Loop
  where

-- llvm-general
import LLVM.General.AST

-- accelerate
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type
import qualified Data.Array.Accelerate.LLVM.CodeGen.Loop        as Loop

import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Base


-- | A standard loop where the CUDA threads cooperatively step over an index
-- space from the start to end indices. The threads stride the array in a way
-- that maintains memory coalescing.
--
-- The start and array indices are given as natural array indexes, and the
-- thread specific indices are calculated by the loop.
--
-- > for ( int32 i = blockDim.x * blockIdx.x + threadIdx.x + start
-- >     ; i <  end
-- >     ; i += blockDim.x * gridDim.x )
--
-- TODO: This assumes that the starting offset retains alignment to the warp
--       boundary. This might not always be the case, so provide a version that
--       explicitly aligns reads to the warp boundary.
--
imapFromTo
    :: Operand                  -- start
    -> Operand                  -- end
    -> (Operand -> CodeGen ())  -- function to execute at each index
    -> CodeGen ()
imapFromTo start end body = do
  step  <- gridSize
  tid   <- globalThreadIdx
  z     <- add int32 tid start

  Loop.for (typeOf (int32 :: IntegralType Int32))
           z
           (\ix -> lt  int32 ix end)
           (\ix -> add int32 ix step)
           body


-- | Iterate with an accumulator between the start and end index, executing the
-- given function at each. The threads stride the array in a way that maintains
-- memory coalescing.
--
-- The start and end indices are given as natural array indices, and the
-- thread-specific indices are calculated by the loop.
--
-- > for ( int32 i = blockDim.x * blockIdx.x + threadIdx.x + start
-- >     ; x < end
-- >     ; x += blockDim.x * gridDim.x )
--
-- TODO: This assumes that the starting offset retains alignment to the warp
--       boundary. This might not always be the case, so provide a version that
--       explicitly aligns reads on the warp boundary (e.g. the segmented
--       reduction in the accelerate-cuda package).
--
iterFromTo
    :: Operand
    -> Operand
    -> [Type]
    -> [Operand]
    -> (Operand -> [Operand] -> CodeGen [Operand])
    -> CodeGen [Operand]
iterFromTo start end tacc acc body = do
  step  <- gridSize
  tid   <- globalThreadIdx
  z     <- add int32 tid start

  Loop.iter (typeOf (int32 :: IntegralType Int32))
            z
            (\i -> lt int i end)
            (\i -> add int i step)
            tacc acc body

