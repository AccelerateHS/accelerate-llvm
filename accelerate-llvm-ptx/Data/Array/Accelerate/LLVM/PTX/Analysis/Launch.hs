{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Analysis.Launch (

  launchConfig, determineOccupancy

) where

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Analysis.Shape

-- library
import Foreign.CUDA.Analysis                            as CUDA


-- | Determine kernel launch parameters for the given array computation (as well
-- as compiled function module). This consists of the thread block size, number
-- of blocks, and dynamically allocated shared memory (bytes), respectively.
--
-- For most operations, this selects the minimum block size that gives maximum
-- occupancy, and the grid size limited to the maximum number of physically
-- resident blocks. Hence, kernels may need to process multiple elements per
-- thread. Scan operations select the largest block size of maximum occupancy.
--
launchConfig
    :: DelayedOpenAcc aenv a
    -> DeviceProperties         -- the device being executed on
    -> Occupancy                -- kernel occupancy information
    -> ( Int                    -- block size
       , Int -> Int             -- number of blocks for input problem size (grid)
       , Int )                  -- shared memory (bytes)
launchConfig acc dev@DeviceProperties{..} occ =
  case acc of
    Delayed{}     -> $internalError "launchConfig" "encountered delayed array"
    Manifest pacc ->
      let cta       = activeThreads occ `div` activeThreadBlocks occ
          maxGrid   = multiProcessorCount * activeThreadBlocks occ
          smem      = sharedMem dev pacc cta
      in
      (cta, \n -> maxGrid `min` gridSize dev pacc n cta, smem)


-- | Determine maximal occupancy statistics for the given kernel / device
-- combination.
--
determineOccupancy
    :: DelayedOpenAcc aenv a
    -> DeviceProperties
    -> Int                      -- maximum number of threads per block
    -> Int                      -- registers per thread
    -> Int                      -- static shared memory per thread (bytes)
    -> Occupancy
determineOccupancy acc dev maxBlock registers static_smem =
  case acc of
    Delayed{}     -> $internalError "determineOccupancy" "encountered delayed array"
    Manifest pacc ->
      let
          dynamic_smem = sharedMem dev pacc
          (_, occ)     = blockSize dev pacc maxBlock registers (\threads -> static_smem + dynamic_smem threads)
      in
      occ


-- |
-- Determine an optimal thread block size for a given array computation. Fold
-- requires blocks with a power-of-two number of threads. Scans select the
-- largest size thread block possible, because if only one thread block is
-- needed we can calculate the scan in a single pass, rather than three.
--
blockSize
    :: DeviceProperties
    -> PreOpenAcc DelayedOpenAcc aenv a
    -> Int                      -- maximum number of threads per block
    -> Int                      -- number of registers used
    -> (Int -> Int)             -- shared memory as a function of thread block size (bytes)
    -> (Int, Occupancy)
blockSize dev acc lim regs smem =
  optimalBlockSizeBy dev (filter (<= lim) . strategy) (const regs) smem
  where
    strategy = case acc of
      Fold _ _ _        -> incPow2
      Fold1 _ _         -> incPow2
      Scanl _ _ _       -> incWarp
      Scanl' _ _ _      -> incWarp
      Scanl1 _ _        -> incWarp
      Scanr _ _ _       -> incWarp
      Scanr' _ _ _      -> incWarp
      Scanr1 _ _        -> incWarp
      _                 -> decWarp


-- |
-- Determine the number of blocks of the given size necessary to process the
-- given array expression. This should understand things like #elements per
-- thread for the various kernels.
--
-- The 'size' parameter is typically the number of elements in the array, except
-- for the following instances:
--
--  * foldSeg: the number of segments; require one warp per segment
--
--  * fold: for multidimensional reductions, this is the size of the shape tail
--          for 1D reductions this is the total number of elements
--
gridSize :: DeviceProperties -> PreOpenAcc DelayedOpenAcc aenv a -> Int -> Int -> Int
gridSize DeviceProperties{..} pacc size cta =
  case pacc of
    FoldSeg{}                           -> split pacc (size * warpSize) cta
    Fold1Seg{}                          -> split pacc (size * warpSize) cta
    Fold{}
      | preAccDim delayedDim pacc == 0  -> split pacc size cta
      | otherwise                       -> 1 `max` size
    Fold1{}
      | preAccDim delayedDim pacc == 0  -> split pacc size cta
      | otherwise                       -> 1 `max` size
    _                                   -> split pacc size cta

split :: acc aenv a -> Int -> Int -> Int
split acc size cta = (size `between` eltsPerThread acc) `between` cta
  where
    between arr n   = 1 `max` ((n + arr - 1) `div` n)
    eltsPerThread _ = 1


-- |
-- Analyse the given array expression, returning an estimate of dynamic shared
-- memory usage as a function of thread block size. This can be used by the
-- occupancy calculator to optimise kernel launch shape.
--
sharedMem :: DeviceProperties -> PreOpenAcc DelayedOpenAcc aenv a -> Int -> Int
sharedMem DeviceProperties{..} acc blockDim =
  let warps = blockDim `div` warpSize
  in
  case acc of
    -- non-computation forms
    Alet{}          -> $internalError "sharedMem" "Let"
    Avar{}          -> $internalError "sharedMem" "Avar"
    Apply{}         -> $internalError "sharedMem" "Apply"
    Acond{}         -> $internalError "sharedMem" "Acond"
    Awhile{}        -> $internalError "sharedMem" "Awhile"
    Atuple{}        -> $internalError "sharedMem" "Atuple"
    Aprj{}          -> $internalError "sharedMem" "Aprj"
    Use{}           -> $internalError "sharedMem" "Use"
    Unit{}          -> $internalError "sharedMem" "Unit"
    Reshape{}       -> $internalError "sharedMem" "Reshape"
    Aforeign{}      -> $internalError "sharedMem" "Aforeign"

    -- skeleton nodes
    Generate{}      -> 0
    Transform{}     -> 0
    Replicate{}     -> 0
    Slice{}         -> 0
    Map{}           -> 0
    ZipWith{}       -> 0
    Permute{}       -> 0
    Backpermute{}   -> 0
    Stencil{}       -> 0
    Stencil2{}      -> 0
    Fold _ x _      -> sizeOf (delayedExpType x) * (warps * (1 + (warpSize + (warpSize `div` 2))))
    Fold1 _ a       -> sizeOf (delayedAccType a) * (warps * (1 + (warpSize + (warpSize `div` 2))))
    FoldSeg _ x _ _ -> sizeOf (delayedExpType x) * blockDim + (warps * 8)
    Fold1Seg _ a _  -> sizeOf (delayedAccType a) * blockDim + (warps * 8)
    Scanl _ x _     -> sizeOf (delayedExpType x) * blockDim
    Scanr _ x _     -> sizeOf (delayedExpType x) * blockDim
    Scanl' _ x _    -> sizeOf (delayedExpType x) * blockDim
    Scanr' _ x _    -> sizeOf (delayedExpType x) * blockDim
    Scanl1 _ a      -> sizeOf (delayedAccType a) * blockDim
    Scanr1 _ a      -> sizeOf (delayedAccType a) * blockDim

