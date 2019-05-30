-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Loop
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Loop
  where

-- accelerate
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic            as A
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import qualified Data.Array.Accelerate.LLVM.CodeGen.Loop        as Loop

import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.Target


-- | A standard loop where the CUDA threads cooperatively step over an index
-- space from the start to end indices. The threads stride the array in a way
-- that maintains memory coalescing.
--
-- The start and end array indices are given as natural array indexes, and the
-- thread specific indices are calculated by the loop.
--
-- > for ( int i = blockDim.x * blockIdx.x + threadIdx.x + start
-- >     ; i <  end
-- >     ; i += blockDim.x * gridDim.x )
--
-- TODO: This assumes that the starting offset retains alignment to the warp
--       boundary. This might not always be the case, so provide a version that
--       explicitly aligns reads to the warp boundary.
--
imapFromTo :: IR Int -> IR Int -> (IR Int -> CodeGen PTX ()) -> CodeGen PTX ()
imapFromTo start end body = do
  step  <- A.fromIntegral integralType numType =<< gridSize
  tid   <- A.fromIntegral integralType numType =<< globalThreadIdx
  i0    <- add numType tid start
  --
  Loop.imapFromStepTo i0 step end body

