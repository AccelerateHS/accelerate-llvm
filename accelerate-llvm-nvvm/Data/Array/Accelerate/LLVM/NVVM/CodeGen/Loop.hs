-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Loop
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
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
-- space from the start to end indices.
--
-- > for ( int32 i = blockDim.x * blockIdx.x + threadIdx.x + start
-- >     ; i <  end
-- >     ; i += blockDim.x * gridDim.x )
--
imapFromTo
    :: Operand                  -- start
    -> Operand                  -- end
    -> (Operand -> CodeGen ())  -- function to execute at each index
    -> CodeGen ()
imapFromTo start end body = do
  step  <- gridSize
  tid   <- threadIdx
  cta   <- blockIdx
  ntid  <- blockDim

  x     <- mul int32 ntid cta
  y     <- add int32 tid x
  z     <- add int32 start y

  Loop.for (typeOf (int32 :: IntegralType Int32))
           z
           (\ix -> lt  int32 ix end)
           (\ix -> add int32 ix step)
           body

