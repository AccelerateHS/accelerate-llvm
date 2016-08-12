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



-- Scan an array of arbitrary rank along the innermost dimension only.
--
mkScanDim
    :: forall aenv sh e. (Shape sh, Elt e)
    =>          DeviceProperties                                -- ^ properties of the target GPU
    ->          Gamma         aenv                              -- ^ array environment
    ->          IRFun2    PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe   (IRExp PTX aenv e)                               -- ^ seed element, if this is an exclusive scan
    ->          IRDelayed PTX aenv (Array (sh :. Int) e)        -- ^ input data
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkFoldDim dev aenv combine mseed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "fold" (paramGang ++ paramOut ++ paramEnv) $ do

    -- If the innermost dimension is smaller than the number of threads in the
    -- block, those threads will never contribute to the output.
    tid <- threadIdx
    sz  <- A.fromIntegral integralType numType . indexHead =<< delayedExtent
    when (A.lt scalarType tid sz) $ do

      -- Thread blocks iterate over the outer dimensions.
      --
      gd    <- gridDim
      bid   <- blockIdx
      seg0  <- A.add numType start bid
      for seg0 (\seg -> A.lt scalarType seg end) (\seg -> A.add numType seg gd) $ \seg -> do

        -- Wait for threads to catch up before starting this segment. We could
        -- also place this at the bottom of the loop, but here allows threads to
        -- exit quickly on the last iteration.
        __syncthreads

        -- Step 1: scan on each block to get local sums
        from  <- A.mul numType seg  sz          -- first linear index this block will scan
        to    <- A.add numType from sz          -- last linear index this block will scan (exclusive)

        i0    <- A.add numType from tid
        x0    <- app1 delayedLinearIndex =<< A.fromIntegral integralType numType i0
        bd    <- blockDim
        r0    <- if A.gte scalarType sz bd
                   then scanBlockSMem dev combine Nothing x0
                   else scanBlockSMem dev combine (Just sz) x0

        -- Step 2: scan over the last element in a block
        next  <- A.add numType from bd
        r     <- iter next r0 (\i -> A.lt scalarType i to) (\i -> A.add numType i bd) $ \offset r -> do

          -- Wait for all threads to catch up before starting the next stripe
          __syncthreads

          -- Threads cooperatively scan this stripe of the input
          i     <- A.add numType offset tid
          i'    <- A.fromIntegral integralType numType i
          valid <- A.sub numType to offset
          r'    <- if A.gte scalarType valid bd
                      -- All threads of the block are valid, so we can avoid
                      -- bounds checks.
                      then do
                        x <- app1 delayedLinearIndex i'
                        scanBlockSMem dev combine Nothing x

                      -- Otherwise we require bounds checks when reading the
                      -- input and during the scan.
                      else
                      if A.lt scalarType i to
                        then do
                          x <- app1 delayedLinearIndex i'
                          scanBlockSMem dev combine (Just valid) x
                        else
                          return r

          if A.eq scalarType tid $ lift (blockDim - 1)
            then app2 combine r r'
            else return r'

        -- Step 3: Write the scan aggregate result to each thread
        --
        x' <- if A.neq scalarType tid $ lift (blockDim - 1)
                then app2 combine r0 r
                else return r


        -- Step 4: Every thread writes the scan result of this dimension to
        -- memory.
        --
        writeArray arrOut seg =<< return x'

    return_


-- Efficient threadblock-wide scan using the specified operator.
--
-- Requires dynamically allocated memory: (#warps * (1 + 1.5 * warp size)).
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.2/cub/block/specializations/block_scan_warp_scans.cuh
--
scanBlockSMem
    :: forall aenv e. Elt e
    => DeviceProperties                                         -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
    -> Maybe (IR Int32)                                         -- ^ number of valid elements (may be less than block size)
    -> IR e                                                     -- ^ calling thread's input element
    -> CodeGen (IR e)                                           -- ^ thread-block-wide scan using the specified operator (lane 0 only)
scanBlockSMem dev combine size = warpScan >=> warpAggregate
  where
    int32 :: Integral a => a -> IR Int32
    int32 = lift . P.fromIntegral

    -- Temporary storage required for each warp
    bytes           = sizeOf (eltType (undefined::e))
    warp_smem_elems = CUDA.warpSize dev + (CUDA.warpSize dev `div` 2)

    -- Step 1: Scan in every warp
    --
    warpScan :: IR e -> CodeGen (IR e)
    warpScan input = do
      -- Allocate (1.5 * warpSize) elements of shared memory for each warp
      wid   <- warpId
      skip  <- A.mul numType wid (int32 (warp_smem_elems * bytes))
      smem  <- sharedMem (int32 warp_smem_elems) skip

      -- Are we doing bounds checking for this warp?
      --
      case size of
        -- The entire thread block is valid, so skip bounds checks.
        Nothing ->
          scanWarpSMem dev combine smem Nothing input

        -- Otherwise check how many elements are valid for this warp. If it is
        -- full then we can still skip bounds checks for it.
        Just n -> do
          offset <- A.mul numType wid (int32 (CUDA.warpSize dev))
          valid  <- A.sub numType n offset
          if A.gte scalarType valid (int32 (CUDA.warpSize dev))
            then scanWarpSMem dev combine smem Nothing input
            else scanWarpSMem dev combine smem (Just valid) input

    -- Step 2: Aggregate per-warp scan
    --
    warpAggregate :: IR e -> CodeGen (IR e)
    warpAggregate input = do
      -- Allocate #warps elements of shared memory
      bid   <- blockDim
      warps <- A.quot integralType bid (int32 (CUDA.warpSize dev))
      skip  <- A.mul numType warps (int32 (warp_smem_elems * bytes))
      smem  <- sharedMem warps skip

      -- Share the per-lane aggregates
      wid   <- warpId
      lane  <- laneId
      lastLane <- lift (CUDA.warpSize dev - 1)
      when (A.eq scalarType lane lastLane) $ do
        writeArray smem wid input

      -- Wait for each warp to finish its local scan
      __syncthreads

      -- Whether or not the partial belonging to the current thread is valid
      valid tid =
        case size of
          Nothing -> return (lift True)
          Just s  -> A.lt scalarType tid s -- threadId < size

      -- -- Unfold the aggregate process as a recursive code generation function.
      recursiveAggregate :: Int -> IR e -> IR e -> CodeGen (IR e)
      recursiveAggregate step partial blockAggregate
        | step >= warps  = return x
        | otherwise     = do
          inclusive <- app2 combine blockAggregate partial
          partial'  <- if A.eq scalarType warpId warps
                           then if valid threadId
                                   then return inclusive
                                   else return blockAggregate
                           else return partial
          blockAggregate' <- app2 combine blockAggregate =<<
                              readArray smem step
          recursiveAggregate (step+1) partial' blockAggregate'

      recursiveAggregate 1 input =<< readArray smem 0


scanWarpSMem
  :: forall aenv e. Elt e
  => DeviceProperties
  -> IRFun2 PTX aenv (e -> e -> e)
  -> IRArray (Vector e)
  -> Maybe (IR Int32)
  -> IR e
  -> CodeGen (IR e)
scanWarpSMem dev combine smem size = scanStep 0
  where
    log2 :: Double -> Double
    log2 = P.logBase 2

    -- Number steps required to scan warp
    steps = P.floor . log2 . P.fromIntegral . CUDA.warpSize $ dev

  valid i =
    case size of
      Nothing -> return (lift True)
      Just n  -> A.lt scalarType i n

  -- unfold the scan as a recursive code generation function
  scanStep :: Int -> IR e -> CodeGen (IR e)
  scanStep step x
    | step >= steps               = return x
    | offset <- 1 `P.shiftL` step = do
      -- share input through buffer
      lane <-laneId
      idx  <- A.add numType lane (lift 16) -- lane_id + HALF_WARP_SIZE
      writeVolatileArray smem idx x

      -- update input if in range
      i    <- A.sub numType idx (lift offset)
      x'   <- if valid i
                 then app2 combine x =<< readVolatileArray smem i
                 else return x

      scanStep (step + 1) x'
