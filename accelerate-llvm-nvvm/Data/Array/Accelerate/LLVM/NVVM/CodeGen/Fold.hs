{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Fold
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.CodeGen.Generate
  where

-- llvm-general
import LLVM.General.AST

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.NVVM.Target                   ( NVVM )
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Loop

-- CUDA
import Foreign.CUDA.Analysis.Device                             as CUDA

-- standard library
import Prelude                                                  as P
import Control.Monad


-- Threads of a warp cooperatively reduce the elements of the named shared
-- memory array (AddrSpace 3). This procedure uses warp-synchronous programming
-- to avoid the use of __syncthreads(), but we still do a bounds check to ensure
-- only valid elements are read (the shared memory array is not initialised with
-- a neutral element).
--
reduceWarpTree
    :: DeviceProperties
    -> [Type]                       -- type of element 'e'
    -> IRFun2 aenv (e -> e -> e)    -- combination function
    -> [Operand]                    -- this thread's initial value
    -> [Name]                       -- shared memory array that intermediate and input values are stored in
    -> Operand                      -- number of elements to reduce [0,n) :: Int32
    -> Operand                      -- thread identifier, such as thread lane or threadIdx.x
    -> Operand                      -- where this thread stores its values in shared memory (threadIdx.x)
    -> CodeGen [Operand]            -- variables storing the final reduction value
reduceWarpTree dev ty combine x0 sdata n ix tid
  = foldM reduce x0
  $ map pow2 [v, v-1 .. 0]
  where
    v           = P.floor (P.logBase 2 (P.fromIntegral (CUDA.warpSize dev) :: Double))

    pow2 :: Int32 -> Int32
    pow2 x      = 2 ^ x

    reduce :: [Operand] -> Int32 -> CodeGen [Operand]
    reduce xs step = do
      _then     <- newBlock ("reduceWarpTree.then" ++ show step)
      _exit     <- newBlock ("reduceWarpTree.exit" ++ show step)

      -- if ( ix + step < n )
      o         <- add int32 ix (constOp $ num int32 step)
      c         <- lt int32 o n
      top       <- cbr c _then _exit

      -- then { xs := xs `combine` sdata [ tid + step ] }
      setBlock _then
      i         <- add int32 tid (constOp $ num int32 step)
      ys        <- readArray sdata i
      xs'       <- combine xs ys
      bot       <- br _exit

      setBlock _exit
      zipWithM phi' ty [[(t,top), (b,bot)] | t <- xs | b <- xs' ]

