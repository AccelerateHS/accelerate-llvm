-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Loop
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Loop
  where

-- accelerate
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import qualified Data.Array.Accelerate.LLVM.CodeGen.Loop        as Loop

import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base


-- | A standard loop where the CUDA threads cooperatively step over an index
-- space from the start to end indices. The threads stride the array in a way
-- that maintains memory coalescing.
--
-- The start and end array indices are given as natural array indexes, and the
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
imapFromTo :: IR Int32 -> IR Int32 -> (IR Int32 -> CodeGen ()) -> CodeGen ()
imapFromTo start end body = do
  step  <- gridSize
  tid   <- globalThreadIdx
  i0    <- add numType tid start
  --
  Loop.imapFromStepTo i0 step end body

