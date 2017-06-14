-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
-- Copyright   : [2008..2017] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Analysis.Launch (

  DeviceProperties, Occupancy, LaunchConfig,
  simpleLaunchConfig, launchConfig,
  multipleOf,

) where

-- library
import Foreign.CUDA.Analysis                            as CUDA


-- Kernel annotation for the PTX backend consists of the launch configuration
--
-- data instance KernelAnn PTX = ANN_PTX LaunchConfig

-- | Given information about the resource usage of the compiled kernel,
-- determine the optimum launch parameters.
--
type LaunchConfig
  =  Int                    -- maximum #threads per block
  -> Int                    -- #registers per thread
  -> Int                    -- #bytes of static shared memory
  -> ( Occupancy
     , Int                  -- thread block size
     , Int -> Int           -- grid size required to process the given input size
     , Int                  -- #bytes dynamic shared memory
     )

-- | Analytics for a simple kernel which requires no additional shared memory or
-- have other constraints on launch configuration. The smallest thread block
-- size, in increments of a single warp, with the highest occupancy is used.
--
simpleLaunchConfig :: DeviceProperties -> LaunchConfig
simpleLaunchConfig dev = launchConfig dev (decWarp dev) (const 0) multipleOf


-- | Determine the optimal kernel launch configuration for a kernel.
--
launchConfig
    :: DeviceProperties     -- ^ Device architecture to optimise for
    -> [Int]                -- ^ Thread block sizes to consider
    -> (Int -> Int)         -- ^ Shared memory (#bytes) as a function of thread block size
    -> (Int -> Int -> Int)  -- ^ Determine grid size for input size 'n' (first arg) over thread blocks of size 'm' (second arg)
    -> LaunchConfig
launchConfig dev candidates dynamic_smem grid_size maxThreads registers static_smem =
  let
      (cta, occ)  = optimalBlockSizeOf dev (filter (<= maxThreads) candidates) (const registers) smem
      maxGrid     = multiProcessorCount dev * activeThreadBlocks occ
      grid n      = maxGrid `min` grid_size n cta
      smem n      = static_smem + dynamic_smem n
  in
  ( occ, cta, grid, dynamic_smem cta )


-- | The next highest multiple of 'y' from 'x'.
--
multipleOf :: Int -> Int -> Int
multipleOf x y = ((x + y - 1) `quot` y)

