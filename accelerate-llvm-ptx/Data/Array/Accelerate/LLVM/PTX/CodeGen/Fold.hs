{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold
  where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Sugar                            ( Array, Scalar, Vector, Shape, Z, (:.), Elt(..) )

-- accelerate-llvm-*
import Data.Array.Accelerate.LLVM.Analysis.Match
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

import LLVM.AST.Type.Representation

-- cuda
import qualified Foreign.CUDA.Analysis                              as CUDA

import Control.Applicative                                          ( (<$>), (<*>) )
import Control.Monad                                                ( (>=>), (<=<) )
import Data.String                                                  ( fromString )
import Data.Bits                                                    as P
import Prelude                                                      as P


-- Reduce an array along the innermost dimension. The reduction function must be
-- associative to allow for an efficient parallel implementation, but the
-- initial element does /not/ need to be a neutral element of operator.
--
-- TODO: Specialise for commutative operations (such as (+)) and those with
--       a neutral element {(+), 0}
--
mkFold
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Array (sh :. Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkFold ptx@(deviceProperties . ptxContext -> dev) aenv f z acc
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = (+++) <$> mkFoldAll  dev aenv f (Just z) acc
          <*> mkFoldFill ptx aenv z

  | otherwise
  = (+++) <$> mkFoldDim  dev aenv f (Just z) acc
          <*> mkFoldFill ptx aenv z


-- Reduce a non-empty array along the innermost dimension. The reduction
-- function must be associative to allow for an efficient parallel
-- implementation.
--
-- TODO: Specialise for commutative operations (such as (+)) and those with
--       a neutral element {(+), 0}
--
mkFold1
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRDelayed PTX aenv (Array (sh :. Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkFold1 (deviceProperties . ptxContext -> dev) aenv f acc
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = mkFoldAll dev aenv f Nothing acc

  | otherwise
  = mkFoldDim dev aenv f Nothing acc


-- Reduce an array to a single element.
--
-- Since reductions consume arrays that have been fused into them, parallel
-- reduction requires two separate kernels. At an example, take vector dot
-- product:
--
-- > dotp xs ys = fold (+) 0 (zipWith (*) xs ys)
--
-- 1. The first pass reads in the fused array data, in this case corresponding
--    to the function (\i -> (xs!i) * (ys!i)).
--
-- 2. The second pass reads in the manifest array data from the first step and
--    directly reduces the array. This can be done recursively in-place until
--    only a single element remains.
--
-- In both phases, thread blocks cooperatively reduce a stripe of the input (one
-- element per thread) to a single element, which is stored to the output array.
--
mkFoldAll
    :: forall aenv e. Elt e
    =>          DeviceProperties                                -- ^ properties of the target GPU
    ->          Gamma         aenv                              -- ^ array environment
    ->          IRFun2    PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe   (IRExp     PTX aenv e)                           -- ^ seed element, if this is an exclusive reduction
    ->          IRDelayed PTX aenv (Vector e)                   -- ^ input data
    -> CodeGen (IROpenAcc PTX aenv (Scalar e))
mkFoldAll dev aenv combine mseed acc =
  foldr1 (+++) <$> sequence [ mkFoldAllS  dev aenv combine mseed acc
                            , mkFoldAllM1 dev aenv combine       acc
                            , mkFoldAllM2 dev aenv combine mseed
                            ]


-- Reduction to an array to a single element, for small arrays which can be
-- processed by a single thread block.
--
mkFoldAllS
    :: forall aenv e. Elt e
    =>          DeviceProperties                                -- ^ properties of the target GPU
    ->          Gamma         aenv                              -- ^ array environment
    ->          IRFun2    PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe   (IRExp     PTX aenv e)
    ->          IRDelayed PTX aenv (Vector e)                   -- ^ input data
    -> CodeGen (IROpenAcc PTX aenv (Scalar e))
mkFoldAllS dev aenv combine mseed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Scalar e))
      paramEnv                  = envParam aenv
      --
      config                    = launchConfig dev (CUDA.incWarp dev) smem multipleOf
      smem n                    = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `div` ws
          per_warp  = ws + ws `div` 2
          bytes     = sizeOf (eltType (undefined :: e))
  in
  makeOpenAccWith config "foldAllS" (paramGang ++ paramOut ++ paramEnv) $ do

    tid <- threadIdx
    bd  <- blockDim

    -- We can assume that there is only a single thread block
    i0  <- A.add numType start tid
    sz  <- A.sub numType end start
    when (A.lt scalarType i0 sz) $ do

      -- Thread reads initial element and then participates in block-wide
      -- reduction.
      x0 <- app1 delayedLinearIndex =<< A.fromIntegral integralType numType i0
      r0 <- if A.eq scalarType sz bd
              then reduceBlockSMem dev combine Nothing   x0
              else reduceBlockSMem dev combine (Just sz) x0

      when (A.eq scalarType tid (lift 0)) $
        writeArray arrOut tid =<<
          case mseed of
            Nothing -> return r0
            Just z  -> flip (app2 combine) r0 =<< z   -- Note: initial element on the left

    return_


-- Reduction of an entire array to a single element. This kernel implements step
-- one for reducing large arrays which must be processed by multiple thread
-- blocks.
--
mkFoldAllM1
    :: forall aenv e. Elt e
    =>          DeviceProperties                                -- ^ properties of the target GPU
    ->          Gamma         aenv                              -- ^ array environment
    ->          IRFun2    PTX aenv (e -> e -> e)                -- ^ combination function
    ->          IRDelayed PTX aenv (Vector e)                   -- ^ input data
    -> CodeGen (IROpenAcc PTX aenv (Scalar e))
mkFoldAllM1 dev aenv combine IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
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
  makeOpenAccWith config "foldAllM1" (paramGang ++ paramTmp ++ paramEnv) $ do

    -- Each thread block cooperatively reduces a stripe of the input and stores
    -- that value into a temporary array at a corresponding index. Since the
    -- order of operations remains fixed, this method supports non-commutative
    -- reductions.
    --
    tid   <- threadIdx
    bd    <- blockDim
    sz    <- i32 . indexHead =<< delayedExtent

    imapFromTo start end $ \seg -> do

      -- Wait for all threads to catch up before beginning the stripe
      __syncthreads

      -- Bounds of the input array we will reduce between
      from  <- A.mul numType seg  bd
      step  <- A.add numType from bd
      to    <- A.min scalarType sz step

      -- Threads cooperatively reduce this stripe
      reduceFromTo dev from to combine
        (app1 delayedLinearIndex <=< A.fromIntegral integralType numType)
        (when (A.eq scalarType tid (lift 0)) . writeArray arrTmp seg)

    return_


-- Reduction of an array to a single element, (recursive) step 2 of multi-block
-- reduction algorithm.
--
mkFoldAllM2
    :: forall aenv e. Elt e
    =>          DeviceProperties
    ->          Gamma         aenv
    ->          IRFun2    PTX aenv (e -> e -> e)
    -> Maybe   (IRExp     PTX aenv e)
    -> CodeGen (IROpenAcc PTX aenv (Scalar e))
mkFoldAllM2 dev aenv combine mseed =
  let
      (start, end, paramGang)   = gangParam
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Vector e))
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
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
  makeOpenAccWith config "foldAllM2" (paramGang ++ paramTmp ++ paramOut ++ paramEnv) $ do

    -- Threads cooperatively reduce a stripe of the input (temporary) array
    -- output from the first phase, storing the results into another temporary.
    -- When only a single thread block remains, we have reached the final
    -- reduction step and add the initial element (for exclusive reductions).
    --
    tid   <- threadIdx
    bd    <- blockDim
    gd    <- gridDim
    sz    <- i32 . indexHead $ irArrayShape arrTmp

    imapFromTo start end $ \seg -> do

      -- Wait for all threads to catch up before beginning the stripe
      __syncthreads

      -- Bounds of the input we will reduce between
      from  <- A.mul numType seg  bd
      step  <- A.add numType from bd
      to    <- A.min scalarType sz step

      -- Threads cooperatively reduce this stripe
      reduceFromTo dev from to combine (readArray arrTmp) $ \r ->
        when (A.eq scalarType tid (lift 0)) $
          writeArray arrOut seg =<<
            case mseed of
              Nothing -> return r
              Just z  -> if A.eq scalarType gd (lift 1)
                           then flip (app2 combine) r =<< z   -- Note: initial element on the left
                           else return r

    return_


-- Reduce an array of arbitrary rank along the innermost dimension only.
--
-- For simplicity, each element of the output (reduction along an
-- innermost-dimension index) is computed by a single thread block, meaning we
-- don't have to worry about inter-block synchronisation. A more balanced method
-- would be a segmented reduction (specialised, since the length of each segment
-- is known a priori).
--
mkFoldDim
    :: forall aenv sh e. (Shape sh, Elt e)
    =>          DeviceProperties                                -- ^ properties of the target GPU
    ->          Gamma         aenv                              -- ^ array environment
    ->          IRFun2    PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe   (IRExp     PTX aenv e)                           -- ^ seed element, if this is an exclusive reduction
    ->          IRDelayed PTX aenv (Array (sh :. Int) e)        -- ^ input data
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkFoldDim dev aenv combine mseed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh e))
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
  makeOpenAccWith config "fold" (paramGang ++ paramOut ++ paramEnv) $ do

    -- If the innermost dimension is smaller than the number of threads in the
    -- block, those threads will never contribute to the output.
    tid <- threadIdx
    sz  <- i32 . indexHead =<< delayedExtent
    when (A.lt scalarType tid sz) $ do

      -- Thread blocks iterate over the outer dimensions, each thread block
      -- cooperatively reducing along each outermost index to a single value.
      --
      imapFromTo start end $ \seg -> do

        -- Wait for threads to catch up before starting this segment. We could
        -- also place this at the bottom of the loop, but here allows threads to
        -- exit quickly on the last iteration.
        __syncthreads

        -- Step 1: initialise local sums
        from  <- A.mul numType seg  sz          -- first linear index this block will reduce
        to    <- A.add numType from sz          -- last linear index this block will reduce (exclusive)

        i0    <- A.add numType from tid
        x0    <- app1 delayedLinearIndex =<< A.fromIntegral integralType numType i0
        bd    <- blockDim
        r0    <- if A.gte scalarType sz bd
                   then reduceBlockSMem dev combine Nothing   x0
                   else reduceBlockSMem dev combine (Just sz) x0

        -- Step 2: keep walking over the input
        next  <- A.add numType from bd
        r     <- iterFromStepTo next bd to r0 $ \offset r -> do

          -- Wait for all threads to catch up before starting the next stripe
          __syncthreads

          -- Threads cooperatively reduce this stripe of the input
          i     <- A.add numType offset tid
          i'    <- A.fromIntegral integralType numType i
          valid <- A.sub numType to offset
          r'    <- if A.gte scalarType valid bd
                      -- All threads of the block are valid, so we can avoid
                      -- bounds checks.
                      then do
                        x <- app1 delayedLinearIndex i'
                        reduceBlockSMem dev combine Nothing x

                      -- Otherwise we require bounds checks when reading the
                      -- input and during the reduction.
                      else
                      if A.lt scalarType i to
                        then do
                          x <- app1 delayedLinearIndex i'
                          reduceBlockSMem dev combine (Just valid) x
                        else
                          return r

          if A.eq scalarType tid (lift 0)
            then app2 combine r r'
            else return r'

        -- Step 3: Thread 0 writes the aggregate reduction of this dimension to
        -- memory. If this is an exclusive fold, combine with the initial element.
        --
        when (A.eq scalarType tid (lift 0)) $
          writeArray arrOut seg =<<
            case mseed of
              Nothing -> return r
              Just z  -> flip (app2 combine) r =<< z  -- Note: initial element on the left

    return_


-- Exclusive reductions over empty arrays (of any dimension) fill the lower
-- dimensions with the initial element.
--
mkFoldFill
    :: (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRExp PTX aenv e
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkFoldFill ptx aenv seed =
  mkGenerate ptx aenv (IRFun1 (const seed))


-- Efficient threadblock-wide reduction using the specified operator. The
-- aggregate reduction value is stored in thread zero. Supports non-commutative
-- operators.
--
-- Requires dynamically allocated memory: (#warps * (1 + 1.5 * warp size)).
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.2/cub/block/specializations/block_reduce_warp_reductions.cuh
--
reduceBlockSMem
    :: forall aenv e. Elt e
    => DeviceProperties                                         -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
    -> Maybe (IR Int32)                                         -- ^ number of valid elements (may be less than block size)
    -> IR e                                                     -- ^ calling thread's input element
    -> CodeGen (IR e)                                           -- ^ thread-block-wide reduction using the specified operator (lane 0 only)
reduceBlockSMem dev combine size = warpReduce >=> warpAggregate
  where
    int32 :: Integral a => a -> IR Int32
    int32 = lift . P.fromIntegral

    -- Temporary storage required for each warp
    bytes           = sizeOf (eltType (undefined::e))
    warp_smem_elems = CUDA.warpSize dev + (CUDA.warpSize dev `div` 2)

    -- Step 1: Reduction in every warp
    --
    warpReduce :: IR e -> CodeGen (IR e)
    warpReduce input = do
      -- Allocate (1.5 * warpSize) elements of shared memory for each warp
      wid   <- warpId
      skip  <- A.mul numType wid (int32 (warp_smem_elems * bytes))
      smem  <- dynamicSharedMem (int32 warp_smem_elems) skip

      -- Are we doing bounds checking for this warp?
      --
      case size of
        -- The entire thread block is valid, so skip bounds checks.
        Nothing ->
          reduceWarpSMem dev combine smem Nothing input

        -- Otherwise check how many elements are valid for this warp. If it is
        -- full then we can still skip bounds checks for it.
        Just n -> do
          offset <- A.mul numType wid (int32 (CUDA.warpSize dev))
          valid  <- A.sub numType n offset
          if A.gte scalarType valid (int32 (CUDA.warpSize dev))
            then reduceWarpSMem dev combine smem Nothing      input
            else reduceWarpSMem dev combine smem (Just valid) input

    -- Step 2: Aggregate per-warp reductions
    --
    warpAggregate :: IR e -> CodeGen (IR e)
    warpAggregate input = do
      -- Allocate #warps elements of shared memory
      bd    <- blockDim
      warps <- A.quot integralType bd (int32 (CUDA.warpSize dev))
      skip  <- A.mul numType warps (int32 (warp_smem_elems * bytes))
      smem  <- dynamicSharedMem warps skip

      -- Share the per-lane aggregates
      wid   <- warpId
      lane  <- laneId
      when (A.eq scalarType lane (lift 0)) $ do
        writeArray smem wid input

      -- Wait for each warp to finish its local reduction
      __syncthreads

      -- Update the total aggregate. Thread 0 just does this sequentially (as is
      -- done in CUB), but we could also do this cooperatively (better for
      -- larger thread blocks?)
      tid   <- threadIdx
      if A.eq scalarType tid (lift 0)
        then do
          steps <- case size of
                     Nothing -> return warps
                     Just n  -> do
                       a <- A.add numType n (int32 (CUDA.warpSize dev - 1))
                       b <- A.quot integralType a (int32 (CUDA.warpSize dev))
                       return b
          iterFromStepTo (lift 1) (lift 1) steps input $ \step x ->
            app2 combine x =<< readArray smem step
        else
          return input


-- Efficient warp-wide reduction using shared memory. The aggregate reduction
-- value for the warp is stored in thread lane zero.
--
-- Each warp requires 48 (1.5 x warp size) elements of shared memory. The
-- routine assumes that is is allocated individually per-warp (i.e. can be
-- indexed in the range [0,warp size)).
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.2/cub/warp/specializations/warp_reduce_smem.cuh#L128
--
reduceWarpSMem
    :: forall aenv e. Elt e
    => DeviceProperties                                         -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
    -> IRArray (Vector e)                                       -- ^ temporary storage array in shared memory (1.5 warp size elements)
    -> Maybe (IR Int32)                                         -- ^ number of items that will be reduced by this warp, otherwise all lanes are valid
    -> IR e                                                     -- ^ calling thread's input element
    -> CodeGen (IR e)                                           -- ^ warp-wide reduction using the specified operator (lane 0 only)
reduceWarpSMem dev combine smem size = reduce 0
  where
    log2 :: Double -> Double
    log2  = P.logBase 2

    -- Number steps required to reduce warp
    steps = P.floor . log2 . P.fromIntegral . CUDA.warpSize $ dev

    -- Return whether the index is valid. Assume that constant branches are
    -- optimised away.
    valid i =
      case size of
        Nothing -> return (lift True)
        Just n  -> A.lt scalarType i n

    -- Unfold the reduction as a recursive code generation function.
    reduce :: Int -> IR e -> CodeGen (IR e)
    reduce step x
      | step >= steps               = return x
      | offset <- 1 `P.shiftL` step = do
          -- share input through buffer
          lane <- laneId
          writeArray smem lane x

          -- update input if in range
          i   <- A.add numType lane (lift offset)
          x'  <- if valid i
                   then app2 combine x =<< readArray smem i
                   else return x

          reduce (step+1) x'


-- Efficient warp reduction using __shfl_up instruction (compute >= 3.0)
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.2/cub/warp/specializations/warp_reduce_shfl.cuh#L310
--
-- reduceWarpShfl
--     :: IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
--     -> IR e                                                     -- ^ this thread's input value
--     -> CodeGen (IR e)                                           -- ^ final result
-- reduceWarpShfl combine input =
--   error "TODO: PTX.reduceWarpShfl"


-- Reduction loops
-- ---------------

reduceFromTo
    :: Elt a
    => DeviceProperties
    -> IR Int32                                 -- ^ starting index
    -> IR Int32                                 -- ^ final index (exclusive)
    -> (IRFun2 PTX aenv (a -> a -> a))          -- ^ combination function
    -> (IR Int32 -> CodeGen (IR a))             -- ^ function to retrieve element at index
    -> (IR a -> CodeGen ())                     -- ^ what to do with the value
    -> CodeGen ()
reduceFromTo dev from to combine get set = do

  tid   <- threadIdx
  bd    <- blockDim

  valid <- A.sub numType to from
  i     <- A.add numType from tid

  _     <- if A.gte scalarType valid bd
             then do
               -- All threads in the block will participate in the reduction, so
               -- we can avoid bounds checks
               x <- get i
               r <- reduceBlockSMem dev combine Nothing x
               set r

               return (IR OP_Unit :: IR ())     -- unsightly, but free
             else do
               -- Only in-bounds threads can read their input and participate in
               -- the reduction
               when (A.lt scalarType i to) $ do
                 x <- get i
                 r <- reduceBlockSMem dev combine (Just valid) x
                 set r

               return (IR OP_Unit :: IR ())

  return ()



-- Utilities
-- ---------

i32 :: IR Int -> CodeGen (IR Int32)
i32 = A.fromIntegral integralType numType


imapFromTo
    :: IR Int32
    -> IR Int32
    -> (IR Int32 -> CodeGen ())
    -> CodeGen ()
imapFromTo start end body = do
  bid <- blockIdx
  gd  <- gridDim
  i0  <- A.add numType start bid
  imapFromStepTo i0 gd end body

