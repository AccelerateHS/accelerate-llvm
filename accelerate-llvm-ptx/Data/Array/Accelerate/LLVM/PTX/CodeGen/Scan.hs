{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RebindableSyntax    #-}

-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Scan
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Scan (

  mkScan,

) where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Sugar                            ( Array, Scalar, Vector, Shape, Z, (:.), Elt(..) )

-- accelerate-llvm-*
import LLVM.General.AST.Type.Representation

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Loop                      as Loop
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Target

-- cuda
import Foreign.CUDA.Analysis                                        ( DeviceProperties )
import qualified Foreign.CUDA.Analysis                              as CUDA

import Control.Applicative                                          ( (<$>), (<*>) )
import Control.Monad                                                ( (>=>) )
import Data.String                                                  ( fromString )
import Data.Typeable
import Data.Bits                                                    as P
import Prelude                                                      as P


-- Scan an array of arbitrary rank along the innermost dimension only.
--
mkScan
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Array (sh :. Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array (sh :. Int) e))
mkScan (deviceProperties . ptxContext -> dev) aenv f z acc
  = mkScanAll dev aenv f (Just z) acc

mkScanAll
    :: forall aenv sh e. (Shape sh, Elt e)
    =>          DeviceProperties                                -- ^ properties of the target GPU
    ->          Gamma         aenv                              -- ^ array environment
    ->          IRFun2    PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe   (IRExp PTX aenv e)                               -- ^ seed element, if this is an exclusive scan
    ->          IRDelayed PTX aenv (Array (sh :. Int) e)        -- ^ input data
    -> CodeGen (IROpenAcc PTX aenv (Array (sh :. Int) e))
mkScanAll dev aenv combine mseed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "scan" (paramGang ++ paramOut ++ paramEnv) $ do

    gd       <- gridDim
    bid      <- blockIdx
    bd       <- blockDim
    tid      <- threadIdx
    sz       <- A.fromIntegral integralType numType . indexHead =<< delayedExtent -- size of input array

    -- separate the array by blockDim
    a0       <- A.add numType sz bd
    a1       <- A.add numType a0 (lift 1)
    numBlock <- A.quot integralType a1 bd -- numBlock = (size + blockDim - 1) / blockDim

    start'   <- return (lift 0)
    end'     <- return numBlock

    when (A.lt scalarType tid sz) $ do

      -- Thread blocks iterate over the entire array
      --
      seg0  <- A.add numType start' bid
      Loop.for seg0 (\seg -> A.lt scalarType seg end') (\seg -> A.add numType seg gd) $ \seg -> do

        -- Wait for threads to catch up before starting this segment. We could
        -- also place this at the bottom of the loop, but here allows threads to
        -- exit quickly on the last iteration.
        __syncthreads

        -- Step 1: scan on each block to get local sums
        from  <- A.mul numType seg  bd          -- first linear index this block will scan
        toTmp <- A.add numType from bd
        to    <- if A.lt scalarType sz toTmp    -- last linear index this block will scan (exclusive)
                    then return sz
                    else return toTmp
        valid <- A.sub numType to from          -- number of valid elements

        i     <- A.add numType from tid
        x     <- app1 delayedLinearIndex =<< A.fromIntegral integralType numType i
        r1    <- scanBlockSMem dev combine (Just valid) x
        writeArray arrOut i r1

        -- Step 2: Write the scan aggregate result to each thread
        --

        -- x' <- if A.neq scalarType tid $ lift (blockDim - 1)
        --         then app2 combine x r
        --         else return r

        -- Step 3: Every thread writes the scan result of this dimension to
        -- memory.
        --

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
      lastLane <- return (int32 (CUDA.warpSize dev - 1))
      when (A.eq scalarType lane lastLane) $ do
        writeArray smem wid input

      -- Wait for each warp to finish its local scan
      __syncthreads

      -- -- Whether or not the partial belonging to the current thread is valid
      -- valid tid =
      --   case size of
      --     Nothing -> return (lift True)
      --     Just s  -> A.lt scalarType tid s -- threadId < size

      warpNum <- return (P.fromIntegral . CUDA.warpSize $ dev)
      element <- readArray smem (int32 0)
      recursiveAggregate 1 warpNum input combine size smem element


isThreadValid :: IR Int32 -> Maybe (IR Int32) -> CodeGen (IR Bool)
isThreadValid tid size =
  case size of
    Nothing -> return (lift True)
    Just n  -> A.lt scalarType tid n


-- Unfold the aggregate warps process as a recursive code generation function.
recursiveAggregate :: forall aenv e. Elt e
                   => Int32 -> Int32 -> IR e
                   -> IRFun2 PTX aenv (e -> e -> e)
                   -> Maybe (IR Int32) -> IRArray (Vector e)
                   -> IR e -> CodeGen (IR e)
recursiveAggregate step warps partial combine size smem blockAggregate
  | step >= warps = return partial
  | otherwise    = do
      inclusive <- app2 combine blockAggregate partial
      wid       <- warpId
      tid       <- threadIdx
      partial'  <- if A.eq scalarType wid (lift warps)
                       then
                           if isThreadValid tid size
                               then return inclusive
                               else return blockAggregate
                       else return partial
      blockAggregate' <- app2 combine blockAggregate =<<
        readArray smem (lift step)
      recursiveAggregate (step+1) warps partial' combine size smem blockAggregate'


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
