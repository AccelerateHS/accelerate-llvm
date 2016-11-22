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
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.Analysis.Match
import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Loop
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Target

import LLVM.General.AST.Type.Representation

import qualified Foreign.CUDA.Analysis                              as CUDA

import Control.Applicative
import Control.Monad                                                ( (>=>) )
import Data.String                                                  ( fromString )
import Data.Coerce                                                  as Safe
import Data.Bits                                                    as P
import Prelude                                                      as P hiding ( last )


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
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array (sh:.Int) e))
mkScanl ptx@(deviceProperties . ptxContext -> dev) aenv combine seed arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = foldr1 (+++) <$> sequence [ mkScanAllP1 L dev aenv combine (Just seed) arr
                              , mkScanAllP2 L dev aenv combine
                              , mkScanAllP3 L dev aenv combine (Just seed)
                              , mkScanFill ptx aenv seed
                              ]
  --
  | otherwise
  = error "TODO: multidimensional scanl"


-- 'Data.List.scanl1' style left-to-right inclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation. The array must not be empty.
--
-- > scanl1 (+) (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 10) [0,1,3,6,10,15,21,28,36,45]
--
mkScanl1
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRDelayed PTX aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array (sh:.Int) e))
mkScanl1 (deviceProperties . ptxContext -> dev) aenv combine arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = foldr1 (+++) <$> sequence [ mkScanAllP1 L dev aenv combine Nothing arr
                              , mkScanAllP2 L dev aenv combine
                              , mkScanAllP3 L dev aenv combine Nothing
                              ]
  --
  | otherwise
  = error "TODO: multidimensional scanl1"


-- Variant of 'scanl' where the final result is returned in a separate array.
--
-- > scanr' (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> ( Array (Z :. 10) [10,10,11,13,16,20,25,31,38,46]
--       , Array Z [55]
--       )
--
mkScanl'
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array (sh:.Int) e, Array sh e))
mkScanl' (deviceProperties . ptxContext -> _dev) _aenv _combine _seed _arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = error "TODO: mkScanl'All"
  --
  | otherwise
  = error "TODO: multidimensional scanl'"


-- 'Data.List.scanr' style right-to-left exclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation.
--
-- > scanr (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 11) [55,55,54,52,49,45,40,34,27,19,10]
--
mkScanr
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array (sh:.Int) e))
mkScanr ptx@(deviceProperties . ptxContext -> dev) aenv combine seed arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = foldr1 (+++) <$> sequence [ mkScanAllP1 R dev aenv combine (Just seed) arr
                              , mkScanAllP2 R dev aenv combine
                              , mkScanAllP3 R dev aenv combine (Just seed)
                              , mkScanFill ptx aenv seed
                              ]
  --
  | otherwise
  = error "TODO: multidimensional scanr"


-- 'Data.List.scanr1' style right-to-left inclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation. The array must not be empty.
--
-- > scanr (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 10) [45,45,44,42,39,35,30,24,17,9]
--
mkScanr1
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRDelayed PTX aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array (sh:.Int) e))
mkScanr1 (deviceProperties . ptxContext -> dev) aenv combine arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = foldr1 (+++) <$> sequence [ mkScanAllP1 R dev aenv combine Nothing arr
                              , mkScanAllP2 R dev aenv combine
                              , mkScanAllP3 R dev aenv combine Nothing
                              ]
  --
  | otherwise
  = error "TODO: multidimensional scanr1"


-- Variant of 'scanr' where the final result is returned in a separate array.
--
-- > scanr' (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> ( Array (Z :. 10) [55,54,52,49,45,40,34,27,19,10]
--       , Array Z [55]
--       )
--
mkScanr'
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array (sh:.Int) e, Array sh e))
mkScanr' (deviceProperties . ptxContext -> _dev) _aenv _combine _seed _arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = error "mkScanr'All"
  --
  | otherwise
  = error "TODO: multidimensional scanr'"


-- Core implementation (device wide scan)
-- --------------------------------------
--
-- This is a classic two-pass algorithm which proceeds in two phases and
-- requires ~4n data movement to global memory. In future we would like to
-- replace this with a single pass algorithm.
--

-- Parallel scan, step 1.
--
-- Threads scan a stripe of the input into a temporary array, incorporating the
-- initial element and any fused functions on the way. The final reduction
-- result of this chunk is written to a separate array.
--
mkScanAllP1
    :: forall aenv e. Elt e
    => Direction
    -> DeviceProperties                             -- ^ properties of the target GPU
    -> Gamma aenv                                   -- ^ array environment
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe (IRExp PTX aenv e)                     -- ^ seed element, if this is an exclusive scan
    -> IRDelayed PTX aenv (Vector e)                -- ^ input data
    -> CodeGen (IROpenAcc PTX aenv (Vector e))
mkScanAllP1 dir dev aenv combine mseed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Vector e))
      paramEnv                  = envParam aenv
      --
      config                    = launchConfig dev (CUDA.incWarp dev) smem const
      smem n                    = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `div` ws
          per_warp  = ws + ws `div` 2
          bytes     = sizeOf (eltType (undefined :: e))
  in
  makeOpenAccWith config "scanP1" (paramGang ++ paramOut ++ paramTmp ++ paramEnv) $ do

    len <- A.fromIntegral integralType numType . indexHead =<< delayedExtent

    -- A thread block scans a non-empty stripe of the input, storing the final
    -- block-wide aggregate into a separate array
    --
    -- For exclusive scans, thread 0 of segment 0 must incorporate the initial
    -- element into the input and output. Threads shuffle their indices
    -- appropriately.
    --
    bid <- blockIdx
    gd  <- gridDim
    s0  <- A.add numType start bid

    -- iterating over thread-block-wide segments
    imapFromStepTo s0 gd end $ \chunk -> do

      bd  <- blockDim
      inf <- A.mul numType chunk bd

      -- index i* is the index that this thread will read data from. Recall that
      -- the supremum index is exclusive
      tid <- threadIdx
      i0  <- case dir of
               L -> A.add numType inf tid
               R -> do x <- A.sub numType len inf
                       y <- A.sub numType x tid
                       z <- A.sub numType y (lift 1)
                       return z

      -- index j* is the index that we write to. Recall that for exclusive scans
      -- the output array is one larger than the input; the initial element will
      -- be written into this spot by thread 0 of the first thread block.
      j0  <- case mseed of
               Nothing -> return i0
               Just _  -> case dir of
                            L -> A.add numType i0 (lift 1)
                            R -> return i0

      -- If this thread has input, read data and participate in thread-block scan
      let valid i = case dir of
                      L -> A.lt  scalarType i len
                      R -> A.gte scalarType i (lift 0)

      when (valid i0) $ do
        x0 <- app1 delayedLinearIndex =<< A.fromIntegral integralType numType i0
        x1 <- case mseed of
                Nothing   -> return x0
                Just seed ->
                  if A.eq scalarType tid (lift 0) `A.land` A.eq scalarType chunk (lift 0)
                    then do
                      z <- seed
                      case dir of
                        L -> writeArray arrOut (lift 0 :: IR Int32) z >> app2 combine z x0
                        R -> writeArray arrOut len                  z >> app2 combine x0 z
                    else
                      return x0

        n  <- A.sub numType len inf
        x2 <- if A.gte scalarType n bd
                then scanBlockSMem dir dev combine Nothing  x1
                else scanBlockSMem dir dev combine (Just n) x1

        -- Write this thread's scan result to memory
        writeArray arrOut j0 x2

        -- The last thread also writes its result---the aggregate for this
        -- thread block---to the temporary partial sums array. This is only
        -- necessary for full blocks in a multi-block scan; the final
        -- partially-full tile does not have a successor block.
        last <- A.sub numType bd (lift 1)
        when (A.gt scalarType gd (lift 1) `land` A.eq scalarType tid last) $
          case dir of
            L -> writeArray arrTmp chunk x2
            R -> do u <- A.sub numType end chunk
                    v <- A.sub numType u (lift 1)
                    writeArray arrTmp v x2

    return_


-- Parallel scan, step 2
--
-- A single thread block performs a scan of the per-block aggregates computed in
-- step 1. This gives the per-block prefix which must be added to each element
-- in step 3.
--
mkScanAllP2
    :: forall aenv e. Elt e
    => Direction
    -> DeviceProperties                             -- ^ properties of the target GPU
    -> Gamma aenv                                   -- ^ array environment
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> CodeGen (IROpenAcc PTX aenv (Vector e))
mkScanAllP2 dir dev aenv combine =
  let
      (start, end, paramGang)   = gangParam
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Vector e))
      paramEnv                  = envParam aenv
      --
      config                    = launchConfig dev (CUDA.incWarp dev) smem grid
      grid _ _                  = 1
      smem n                    = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `div` ws
          per_warp  = ws + ws `div` 2
          bytes     = sizeOf (eltType (undefined :: e))
  in
  makeOpenAccWith config "scanP2" (paramGang ++ paramTmp ++ paramEnv) $ do

    -- The first and last threads of the block need to communicate the
    -- block-wide aggrate as a carry-in value across iterations.
    --
    -- We could optimise this a bit if we can get a pointer directly to shared
    -- memory area the last thread uses to compute its local scan.
    carry <- staticSharedMem 1

    bd    <- blockDim
    imapFromStepTo start bd end $ \offset -> do

      -- Index of the partial sums array that this thread will process.
      tid <- threadIdx
      i0  <- case dir of
               L -> A.add numType offset tid
               R -> do x <- A.sub numType end offset
                       y <- A.sub numType x tid
                       z <- A.sub numType y (lift 1)
                       return z

      let valid i = case dir of
                      L -> A.lt  scalarType i end
                      R -> A.gte scalarType i start

      when (valid i0) $ do

        __syncthreads

        x0 <- readArray arrTmp i0
        x1 <- if A.gt scalarType offset (lift 0) `land` A.eq scalarType tid (lift 0)
                then do
                  c <- readArray carry (lift 0 :: IR Int32)
                  case dir of
                    L -> app2 combine c x0
                    R -> app2 combine x0 c
                else do
                  return x0

        n  <- A.sub numType end offset
        x2 <- if A.gte scalarType n bd
                then scanBlockSMem dir dev combine Nothing  x1
                else scanBlockSMem dir dev combine (Just n) x1

        -- Update the temporary array with this thread's result
        writeArray arrTmp i0 x2

        -- The last thread writes the carry-out value. If the last thread is not
        -- active, then this must be the last stripe anyway.
        last <- A.sub numType bd (lift 1)
        when (A.eq scalarType tid last) $
          writeArray carry (lift 0 :: IR Int32) x2

    return_


-- Parallel scan, step 3.
--
-- Threads combine every element of the partial block results with the carry-in
-- value computed in step 2.
--
mkScanAllP3
    :: forall aenv e. Elt e
    => Direction
    -> DeviceProperties                             -- ^ properties of the target GPU
    -> Gamma aenv                                   -- ^ array environment
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe (IRExp PTX aenv e)                     -- ^ seed element, if this is an exclusive scan
    -> CodeGen (IROpenAcc PTX aenv (Vector e))
mkScanAllP3 dir dev aenv combine mseed =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Vector e))
      paramEnv                  = envParam aenv
      --
      -- Threads don't actually require these resources, but it ensures that we
      -- get the same thread block configuration as phases 1 and 2, which is
      -- what we actually require.
      config                    = launchConfig dev (CUDA.incWarp dev) smem const
      smem n                    = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `div` ws
          per_warp  = ws + ws `div` 2
          bytes     = sizeOf (eltType (undefined :: e))
  in
  makeOpenAccWith config "scanP3" (paramGang ++ paramTmp ++ paramOut ++ paramEnv) $ do

    len <- A.fromIntegral integralType numType (indexHead (irArrayShape arrOut))

    bid <- blockIdx
    gd  <- gridDim
    c0  <- A.add numType start bid
    imapFromStepTo c0 gd end $ \chunk -> do

      -- What is the index of this chunk's carry-in value?
      cix   <- case dir of
                 L -> return chunk
                 R -> A.sub numType end chunk

      carry <- readArray arrTmp cix

      -- Add the carry-in to the successor chunk
      bd  <- blockDim
      a   <- A.add numType chunk (lift 1)
      inf <- A.mul numType a bd

      tid <- threadIdx
      i0  <- case dir of
               L -> do
                 x <- A.add numType inf tid
                 case mseed of
                   Nothing -> return x
                   Just _  -> A.add numType x (lift 1)

               R -> do
                 x <- A.sub numType len inf
                 y <- A.sub numType x tid
                 z <- A.sub numType y (lift 1)
                 return z

      let valid i = case dir of
                      L -> A.lt  scalarType i len
                      R -> A.gte scalarType i (lift 0)

      when (valid i0) $ do
        v <- readArray arrOut i0
        u <- case dir of
               L -> app2 combine carry v
               R -> app2 combine v carry
        writeArray arrOut i0 u

    return_


-- Parallel scan, auxiliary
--
-- If this is an exclusive scan of an empty array, we just  fill the result with
-- the seed element.
--
mkScanFill
    :: (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRExp PTX aenv e
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkScanFill ptx aenv seed =
  mkGenerate ptx aenv (IRFun1 (const seed))

mkScan'Fill
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRExp PTX aenv e
    -> CodeGen (IROpenAcc PTX aenv (Array (sh:.Int) e, Array sh e))
mkScan'Fill ptx aenv seed =
  Safe.coerce <$> (mkGenerate ptx aenv (IRFun1 (const seed)) :: CodeGen (IROpenAcc PTX aenv (Array sh e)))

-- Efficient block-wide (inclusive) scan using the specified operator.
--
-- Each block requires (#warps * (1 + 1.5*warp size)) elements of dynamically
-- allocated shared memory.
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.4/cub/block/specializations/block_scan_warp_scans.cuh
--
scanBlockSMem
    :: forall aenv e. Elt e
    => Direction
    -> DeviceProperties                             -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe (IR Int32)                             -- ^ number of valid elements (may be less than block size)
    -> IR e                                         -- ^ calling thread's input element
    -> CodeGen (IR e)
scanBlockSMem dir dev combine nelem = warpScan >=> warpPrefix
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
      scanWarpSMem dir dev combine smem input

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
          steps  <- case nelem of
                      Nothing -> return wid
                      Just n  -> A.min scalarType wid =<< A.quot integralType n (int32 (CUDA.warpSize dev))

          p0     <- readArray smem (lift 0 :: IR Int32)
          prefix <- iterFromStepTo (lift 1) (lift 1) steps p0 $ \step x -> do
                      y <- readArray smem step
                      case dir of
                        L -> app2 combine x y
                        R -> app2 combine y x

          case dir of
            L -> app2 combine prefix input
            R -> app2 combine input prefix


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
    => Direction
    -> DeviceProperties                             -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> IRArray (Vector e)                           -- ^ temporary storage array in shared memory (1.5 x warp size elements)
    -> IR e                                         -- ^ calling thread's input element
    -> CodeGen (IR e)
scanWarpSMem dir dev combine smem = scan 0
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
                      case dir of
                        L -> app2 combine x' x
                        R -> app2 combine x x'

                    else
                      return x

          scan (step+1) x'

