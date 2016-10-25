{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
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

  mkScanl, mkScanl1, mkScanl',
  mkScanr, mkScanr1, mkScanr',

) where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Sugar                            ( Array, Scalar, Vector, Elt, eltType )

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

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch

-- cuda
import Foreign.CUDA.Analysis                                        ( DeviceProperties )
import qualified Foreign.CUDA.Analysis                              as CUDA

import Control.Applicative                                          ( (<$>), (<*>) )
import Control.Monad                                                ( (>=>) )
import Data.String                                                  ( fromString )
import Data.Typeable
import Data.Bits                                                    as P
import Prelude                                                      as P


data Direction = L | R

-- 'Data.List.scanl' style left-to-right exclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation.
--
-- > scanl (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 11) [10,10,11,13,16,20,25,31,38,46,55]
--
mkScanl
    :: Elt e
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Vector e)
    -> CodeGen (IROpenAcc PTX aenv (Vector e))
mkScanl (deviceProperties . ptxContext -> dev) aenv combine seed arr =
  foldr1 (+++) <$> sequence [ mkScanP1 L dev aenv combine (Just seed) arr
                            -- , mkScanP2 L ...
                            -- , mkScanP3 L ...
                            ]

-- 'Data.List.scanl1' style left-to-right inclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation. The array must not be empty.
--
-- > scanl1 (+) (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 10) [0,1,3,6,10,15,21,28,36,45]
--
mkScanl1
    :: forall aenv e. Elt e
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRDelayed PTX aenv (Vector e)
    -> CodeGen (IROpenAcc PTX aenv (Vector e))
mkScanl1 (deviceProperties . ptxContext -> dev) aenv combine arr =
  error "TODO: mkScanl1"


-- Variant of 'scanl' where the final result is returned in a separate array.
--
-- > scanr' (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> ( Array (Z :. 10) [10,10,11,13,16,20,25,31,38,46]
--       , Array Z [55]
--       )
--
mkScanl'
    :: forall aenv e. Elt e
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Vector e)
    -> CodeGen (IROpenAcc PTX aenv (Vector e, Scalar e))
mkScanl' (deviceProperties . ptxContext -> dev) aenv combine seed arr =
  error "TODO: mkScanl'"


-- 'Data.List.scanr' style right-to-left exclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation.
--
-- > scanr (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 11) [55,55,54,52,49,45,40,34,27,19,10]
--
mkScanr
    :: forall aenv e. Elt e
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Vector e)
    -> CodeGen (IROpenAcc PTX aenv (Vector e))
mkScanr (deviceProperties . ptxContext -> dev) aenv combine seed arr =
  error "TODO: mkScanr"

-- 'Data.List.scanr1' style right-to-left inclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation. The array must not be empty.
--
-- > scanr (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 10) [45,45,44,42,39,35,30,24,17,9]
--
mkScanr1
    :: forall aenv e. Elt e
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRDelayed PTX aenv (Vector e)
    -> CodeGen (IROpenAcc PTX aenv (Vector e))
mkScanr1 (deviceProperties . ptxContext -> dev) aenv combine arr =
  error "TODO: mkScanr1"

-- Variant of 'scanr' where the final result is returned in a separate array.
--
-- > scanr' (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> ( Array (Z :. 10) [55,54,52,49,45,40,34,27,19,10]
--       , Array Z [55]
--       )
--
mkScanr'
    :: forall aenv e. Elt e
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Vector e)
    -> CodeGen (IROpenAcc PTX aenv (Vector e, Scalar e))
mkScanr' (deviceProperties . ptxContext -> dev) aenv combine seed arr =
  error "mkScanr'"


-- Core implementation
-- -------------------

-- Parallel scan, step 1.
--
-- Threads scan a stripe of the input into a temporary array, incorporating the
-- initial element and any fused functions along the way. The final reduction
-- result of this chunk is written into a separate array, which will be used to
-- compute the carry-in value.
--
mkScanP1
    :: forall aenv e. Elt e
    => Direction
    -> DeviceProperties                             -- ^ properties of the target GPU
    -> Gamma aenv                                   -- ^ array environment
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe (IRExp PTX aenv e)                     -- ^ seed element, if this is an exclusive scan
    -> IRDelayed PTX aenv (Vector e)                -- ^ input data
    -> CodeGen (IROpenAcc PTX aenv (Vector e))
mkScanP1 dir dev aenv combine mseed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
      paramEnv                  = envParam aenv
      --
      config                    = launchConfig dev (CUDA.incPow2 dev) smem const
      smem n                    = warps * per_warp * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `div` ws
          per_warp  = ws + ws `div` 2
          bytes     = sizeOf (eltType (undefined :: e))
  in
  makeOpenAccWith config "scanP1" (paramGang ++ paramOut ++ paramEnv) $ do
    tid <- threadIdx
    x   <- app1 delayedLinearIndex =<< A.fromIntegral integralType numType tid
    r   <- scanBlockSMem dev combine Nothing x
    writeArray arrOut tid r

    -- gd       <- gridDim
    -- bid      <- blockIdx
    -- bd       <- blockDim
    -- tid      <- threadIdx
    -- sz       <- A.fromIntegral integralType numType . indexHead =<< delayedExtent -- size of input array

    -- -- Divide the array by blockDim
    -- a0       <- A.add numType sz bd
    -- a1       <- A.sub numType a0 (lift 1)
    -- numBlock <- A.quot integralType a1 bd -- numBlock = (size + blockDim - 1) / blockDim

    -- start'   <- return (lift 0)
    -- end'     <- return numBlock

    -- when (A.lt scalarType tid sz) $ do

    --   -- Thread blocks iterate over the entire array
    --   seg0  <- A.add numType start' bid         -- bid
    --   Loop.for seg0 (\seg -> A.lt scalarType seg end') (\seg -> A.add numType seg gd) $ \seg -> do
    --     -- bid, bid + sg, bid + 2*sg ...

    --     -- Wait for threads to catch up before starting this segment. We could
    --     -- also place this at the bottom of the loop, but here allows threads to
    --     -- exit quickly on the last iteration.
    --     __syncthreads

    --     from  <- A.mul numType seg  bd          -- first linear index this block will scan
    --     toTmp <- A.add numType from bd
    --     to    <- if A.lt scalarType sz toTmp    -- last  linear index this block will scan
    --                 then return sz
    --                 else return toTmp
    --     valid <- A.sub numType to from          -- number of valid elements

    --     i     <- A.add numType from tid
    --     x     <- app1 delayedLinearIndex =<< A.fromIntegral integralType numType i
    --     r1    <- scanBlockSMem dev combine (Just valid) x
    --     writeArray arrOut i r1

    return_


{--
-- Step 2: Gather the last element in every block to a temporary array
--
mkScanAllP2
    :: forall aenv sh e. (Shape sh, Elt e)
    =>          DeviceProperties                                -- ^ properties of the target GPU
    ->          Gamma         aenv                              -- ^ array environment
    ->          IRFun2    PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe   (IRExp PTX aenv e)                               -- ^ seed element, if this is an exclusive scan
    -> CodeGen (IROpenAcc PTX aenv (Array (sh :. Int) e))
mkScanAllP2 dev aenv combine mseed =
  let
      (start, end, paramGang)   = gangParam
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Array sh e))
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh e))
      paramEnv                  = envParam aenv
  in
  makeOpenAccWith (simpleLaunchConfig dev) "scanP2" (paramGang ++ paramTmp ++ paramOut ++ paramEnv) $ do
    bd          <- blockDim
    lastElement <- A.sub numType bd (lift 1)

    tid         <- threadIdx
    bid         <- blockIdx
    x           <- readArray arrOut tid
    when (A.eq scalarType tid lastElement) $ do
       writeArray arrTmp bid x

    return_
--}

{--
-- Step 3: Every thread writes the combine result to memory
--
mkScanAllP3
    :: forall aenv sh e. (Shape sh, Elt e)
    =>          DeviceProperties                                -- ^ properties of the target GPU
    ->          Gamma         aenv                              -- ^ array environment
    ->          IRFun2    PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe   (IRExp PTX aenv e)                               -- ^ seed element, if this is an exclusive scan
    -> CodeGen (IROpenAcc PTX aenv (Array (sh :. Int) e))
mkScanAllP3 dev aenv combine mseed =
  let
      (start, end, paramGang)   = gangParam
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Array sh e))
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh e))
      paramEnv                  = envParam aenv
  in
  makeOpenAccWith (simpleLaunchConfig dev) "scanP3" (paramGang ++ paramTmp ++ paramOut ++ paramEnv) $ do
    tid      <- threadIdx
    bid      <- blockIdx
    when (A.gt scalarType bid (lift 0)) $ do
      x <- readArray arrOut tid
      y <- readArray arrTmp =<< A.sub numType bid (lift 1)
      z <- app2 combine x y
      writeArray arrOut tid z

    return_
--}


-- Efficient block-wide (inclusive) scan using the specified operator.
--
-- Each block requires (#warps * (1 + 1.5*warp size)) elements of dynamically
-- allocated shared memory.
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.4/cub/block/specializations/block_scan_warp_scans.cuh
--
scanBlockSMem
    :: forall aenv e. Elt e
    => DeviceProperties                             -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe (IR Int32)                             -- ^ number of valid elements (may be less than block size)
    -> IR e                                         -- ^ calling thread's input element
    -> CodeGen (IR e)
scanBlockSMem dev combine size = warpScan >=> warpPrefix
  where
    int32 :: Integral a => a -> IR (Int32)
    int32 = lift . P.fromIntegral

    -- Temporary storage required for each warp
    warp_smem_elems = CUDA.warpSize dev + (CUDA.warpSize dev `div` 2)
    warp_smem_bytes = warp_smem_elems  * sizeOf (eltType (undefined::e))

    -- Step 1: Scan in every warp
    warpScan :: IR e -> CodeGen (IR e)
    warpScan input = do
      -- Allocate (1.5 * warpSize) elements of shared memory for each warp
      -- (individually addressable by each warp)
      wid   <- warpId
      skip  <- A.mul numType wid (int32 warp_smem_bytes)
      smem  <- dynamicSharedMem (int32 warp_smem_elems) skip
      scanWarpSMem dev combine smem input

    -- Step 2: Collect the aggregate results of each warp to compute the prefix
    -- values for each warp and combine with the partial result to compute each
    -- thread's final value.
    warpPrefix :: IR e -> CodeGen (IR e)
    warpPrefix input = do
      -- Allocate #warps elements of shared memory
      bd    <- blockDim
      warps <- A.quot integralType bd (int32 (CUDA.warpSize dev))
      skip  <- A.mul numType warps (int32 warp_smem_bytes)
      smem  <- dynamicSharedMem warps skip

      -- Share warp aggregates
      wid   <- warpId
      lane  <- laneId
      when (A.eq scalarType lane (int32 (CUDA.warpSize dev - 1))) $ do
        writeArray smem wid input

      -- Wait for each warp to finish its local scan and share the aggregate
      __syncthreads

      -- Compute the prefix value for this warp and add to the partial result.
      -- This step is not required for the first warp, which has no carry-in.
      if A.eq scalarType wid (lift 0)
        then return input
        else do
          -- Every thread sequentially scans the warp aggregates to compute
          -- their prefix value. We do this sequentially, but could also have
          -- warp 0 do it cooperatively if we limit thread block sizes to
          -- (warp size ^ 2).
          steps  <- case size of
                      Nothing -> return wid
                      Just n  -> A.min scalarType wid =<< A.quot integralType n (int32 (CUDA.warpSize dev))

          p0     <- readArray smem (lift 0 :: IR Int32)
          prefix <- iterFromStepTo (lift 1) (lift 1) steps p0 $ \step x ->
                      app2 combine x =<< readArray smem step

          app2 combine prefix input


-- Efficient warp-wide (inclusive) scan using the specified operator.
--
-- Each warp requires 48 (1.5 x warp size) elements of shared memory. The
-- routine assumes that it is allocated individually per-warp (i.e. can be
-- indexed in the range [0, warp size)).
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.4/cub/warp/specializations/warp_scan_smem.cuh
--
scanWarpSMem
    :: forall aenv e. Elt e
    => DeviceProperties                             -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> IRArray (Vector e)                           -- ^ temporary storage array in shared memory (1.5 x warp size elements)
    -> IR e                                         -- ^ calling thread's input element
    -> CodeGen (IR e)
scanWarpSMem dev combine smem = scan 0
  where
    log2 :: Double -> Double
    log2 = P.logBase 2

    -- Number of steps required to scan warp
    steps     = P.floor (log2 (P.fromIntegral (CUDA.warpSize dev)))
    halfWarp  = P.fromIntegral (CUDA.warpSize dev `div` 2)

    -- Unfold the scan as a recursive code generation function
    scan :: Int -> IR e -> CodeGen (IR e)
    scan step x
      | step >= steps               = return x
      | offset <- 1 `P.shiftL` step = do
          -- share partial result through shared memory buffer
          lane <- laneId
          i    <- A.add numType lane (lift halfWarp)
          writeArray smem i x

          -- update partial result if in range
          x'   <- if A.gte scalarType lane (lift offset)
                    then do
                      i' <- A.sub numType i (lift offset)     -- lane + HALF_WARP - offset
                      x' <- readArray smem i'
                      app2 combine x' x

                    else
                      return x

          scan (step+1) x'

