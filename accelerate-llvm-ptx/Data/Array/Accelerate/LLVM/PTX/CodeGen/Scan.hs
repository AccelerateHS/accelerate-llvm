{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Scan (

  mkFold,
  mkFold1,

) where

import Data.Typeable
import Control.Monad hiding (when)

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import qualified Data.Array.Accelerate.LLVM.CodeGen.Loop as Loop

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base as PTXBase
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Loop

import Foreign.CUDA.Analysis as CUDA


-- __global__ void scan(float *g_idata, float *g_odata, int n) {
--   __shared__ float temp[2*BLOCKSIZE];
--   int tid = threadIdx.x;
--   int pout = 0, pin = 1;
--
--   temp[pout * n + tid] = g_idata[tid];
--   __syncthreads()
--
--   for (int offset = 1; offset < n; offset *= 2) {
--     pout = 1 - pout;
--     pin  = 1 - pout;
--
--     if (tid >= offset)
--       temp[pout * n + tid] = temp[pin * n + tid] + temp[pin * n + tid - offset];
--     else
--       temp[pout * n + tid] = temp[pin * n + tid];
--
--     __syncthreads();
--   }
--   g_odata[tid] = temp[pout * n + tid];
-- }

inclusiveBlockScan
  :: forall aenv e. Elt e
  => IRFun2 PTX aenv (e -> e -> e)
  -> IRArray (Vector e)
  -> IRArray (Vector e)
  -> CodeGen (IR e)
inclusiveBlockScan combine g_idata g_odata = do
  let
    zero = ir numType (num numType 0)
    one  = ir numType (num numType 1)
    two  = ir numType (num numType 2)
  tid <- A.fromIntegral integralType numType =<< threadIdx
  bd  <- A.fromIntegral integralType numType =<< blockDim
  bi  <- A.fromIntegral integralType numType =<< blockIdx

  -- declare shared memory: __shared__ smem[2*blockDim];
  len  <- mul bd two
  smem <- sharedMem len Nothing :: CodeGen (IRArray (Vector e))

  outFlag <- zero
  inFlag  <- one

  -- read global data: x = g_idata[blockDim * blockIdx + threadIdx]
  i <- A.fromIntegral integralType numType =<< globalThreadIdx
  x <- readArray g_idata i

  -- write data to shared memory: smem[outFlag * BlockDim + threadIdx] = x
  outPos0 <- mul outFlag bd
  outPos1 <- add outPos0 tid
  writeVolatileArray smem outPos1 x
  __syncthreads

  Loop.for one
    (\offset -> lt integralType offset bd)
    (\offset -> mul integralType offset two)
    (\offset -> do
      outFlag <- sub one outFlag
      inFlag  <- sub one outFlag

      outPos0 <- mul outFlag bd
      outPos1 <- add outPos0 tid
      inPos0  <- mul inFlag  bd
      inPos1  <- add inPos0  tid
      inPos2  <- sub inPos1  offset

      ifThenElse
        (gte scalarType tid offset)
        (do
          x <- readVolatileArray smem inPos1
          y <- readVolatileArray smem inPos2
          z <- app2 combine x y
          writeVolatileArray smem outPos1 z
        )
        (do
          x <- readVolatileArray smem inPos1
          writeVolatileArray smem outPos1 x
        )

      __syncthreads
    )

  x <- readVolatileArray smem outPos1
  writeVolatileArray tid x
