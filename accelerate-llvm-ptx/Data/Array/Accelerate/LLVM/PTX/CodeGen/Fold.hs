{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
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

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold (

  mkFold,
  mkFold1,

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

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base

-- cuda
import Foreign.CUDA.Analysis                                        ( DeviceProperties )
import qualified Foreign.CUDA.Analysis                              as CUDA

import Data.Bits                                                    as P
import Data.String                                                  ( fromString )
import Data.Typeable
import Prelude                                                      as P


-- Reduce an array along the innermost dimension. The reduction function must be
-- associative to allow for an efficient parallel implementation, but the
-- initial element does /not/ need to be a neutral element of operator.
--
-- TODO: Specialise for commutative operations (such as (+))
--
mkFold
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Array (sh :. Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkFold (deviceProperties . ptxContext -> dev) aenv f z acc
  | Just REFL <- matchShapeType (undefined::sh) (undefined::Z)
  = mkFoldAll dev aenv f (Just z) acc

  | otherwise
  = mkFoldDim dev aenv f (Just z) acc


-- Reduce a non-empty array along the innermost dimension. The reduction
-- function must be associative to allow for an efficient parallel
-- implementation.
--
-- TODO: Specialise for commutative operations (such as (+))
--
mkFold1
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRDelayed PTX aenv (Array (sh :. Int) e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkFold1 (deviceProperties . ptxContext -> dev) aenv f acc
  | Just REFL <- matchShapeType (undefined::sh) (undefined::Z)
  = mkFoldAll dev aenv f Nothing acc

  | otherwise
  = mkFoldDim dev aenv f Nothing acc


-- Reduce an array of arbitrary rank to a single element.
--
mkFoldAll
    :: forall aenv e. Elt e
    =>          DeviceProperties                                -- ^ properties of the target GPU
    ->          Gamma            aenv                           -- ^ array environment
    ->          IRFun2    PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe   (IRExp     PTX aenv e)                           -- ^ seed element, if this is an exclusive reduction
    ->          IRDelayed PTX aenv (Vector e)                   -- ^ input data
    -> CodeGen (IROpenAcc PTX aenv (Scalar e))
mkFoldAll _dev _aenv _combine _mseed IRDelayed{..} =
  error "TODO: PTX.mkFoldAll"


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
    -> Maybe   (IRExp PTX aenv e)                               -- ^ seed element, if this is an exclusive reduction
    ->          IRDelayed PTX aenv (Array (sh :. Int) e)        -- ^ input data
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkFoldDim dev aenv combine mseed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "fold" (paramGang ++ paramOut ++ paramEnv) $ do
    bd    <- blockDim
    gd    <- gridDim
    ws    <- warpSize
    tid   <- threadIdx
    bid   <- blockIdx

    bytes <- A.mul numType bd (lift $ P.fromIntegral (sizeOf (eltType (undefined::e))))
    nw    <- A.quot integralType bd ws      -- number of warps per block
    smem1 <- sharedMem bd Nothing           -- shared memory for per-warp reduction
    smem2 <- sharedMem nw (Just bytes)      -- shared memory for per-block reduction

    sz'   <- indexHead <$> delayedExtent    -- size of the innermost dimension
    sz    <- A.fromIntegral integralType numType sz'

    -- If the innermost dimension is smaller than the number of threads in the
    -- block, those threads will never contribute to the result and can exit
    -- immediately.
    unless (A.lt scalarType tid sz)
      return_

    -- Thread blocks iterate over the outer dimensions, each thread block
    -- cooperatively reducing along each outermost index to a single value.
    --
    seg0  <- A.add numType start bid
    for seg0 (\seg -> A.lt scalarType seg end) (\seg -> A.add numType seg gd) $ \seg -> do

      from  <- A.mul numType seg sz           -- first linear index this block will reduce
      to    <- A.add numType from sz          -- last linear index this block will reduce (exclusive)

      i0    <- A.add numType from tid
      x0    <- app1 delayedLinearIndex =<< A.fromIntegral integralType numType i0
      r0    <- reduceBlockSMem dev combine smem1 smem2 sz x0

      next  <- A.add numType from bd
      r     <- iter next r0 (\i -> A.lt scalarType i to) (\i -> A.add numType i bd) $ \off acc -> do

        remain  <- A.sub numType to off
        if A.lt scalarType tid remain
           then do
             i <- A.add numType off tid
             x <- app1 delayedLinearIndex =<< A.fromIntegral integralType numType i
             r <- reduceBlockSMem dev combine smem1 smem2 remain x

             if A.eq scalarType tid (lift 0)
                then app2 combine acc r
                else return r
           else
             return acc

      -- Write this result to memory. If this is an exclusive scan, combine with
      -- the initial element.
      --
      when (A.eq scalarType tid (lift 0)) $
        writeArray arrOut seg =<<
          case mseed of
            Nothing -> return r
            Just z  -> flip (app2 combine) r =<< z  -- Note: initial element on the left

    return_


-- Utilities
-- ---------

-- Match reified shape types
--
matchShapeType
    :: forall sh sh'. (Shape sh, Shape sh')
    => sh
    -> sh'
    -> Maybe (sh :=: sh')
matchShapeType _ _
  | Just REFL <- matchTupleType (eltType (undefined::sh)) (eltType (undefined::sh'))
  = gcast REFL

matchShapeType _ _
  = Nothing



-- __global__ void reduceBlockSMem(int *g_idata, int *g_odata, combine) {
--   __shared__ int smem[blockDim.x];
--   unsigned int tid = threadIdx.x;
--   unsigned int i = blockIdx.x * blockDim.x + threadIdx.x;
--   smem[tid] = g_idata[i];
--   __syncthreads();
--
--   for (unsigned int s = blockDim.x / 2; s > 32; s >>= 1) {
--    if (tid < s) {
--      smem[tid] = combine(smem[tid], smem[tid + s]);
--    }
--    __syncthreads();
--   }
--
--   if (tid < 32)
--    reduceWarpSMem(combine, smem);
--   if (tid == 0)
--    g_odata[blockIdx.x] = smem[0];
-- }

{--
reduceBlockSMem
  :: forall aenv e. Elt e
  => DeviceProperties                                         -- ^ properties of the target device
  -> IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
  -> IRArray (Vector e)                                       -- ^ input  data array
  -> IRArray (Vector e)                                       -- ^ output data array
  -> CodeGen (IR e)                                           -- ^ final result
reduceBlockSMem dev combine g_idata g_odata = do
  let
    zero = ir numType (num numType 0)
    two  = ir numType (num numType 2)
  tid  <- threadIdx
  bd   <- blockDim
  bi   <- blockIdx
  ws   <- warpSize

  -- declare smem first
  smem <- sharedMem bd Nothing :: CodeGen (IRArray (Vector e))

  -- read input data to smem
  i    <- globalThreadIdx
  x    <- readArray g_idata i
  writeVolatileArray smem tid x
  __syncthreads


  -- for loop
  start <- A.quot integralType bd two
  Loop.for start
           (\s -> gt scalarType s ws)
           (\s -> A.quot integralType s two)
           (\s -> do
             when (lt scalarType tid s) $ do
               i <- add numType tid s
               x <- readVolatileArray smem tid
               y <- readVolatileArray smem i
               z <- app2 combine x y
               writeVolatileArray smem tid z

             __syncthreads
           )

  -- reduceWarpSMem
  when (lt scalarType tid ws) $ do
    void $ reduceWarpSMem combine smem

  when (eq scalarType tid zero) $ do
    x <- readVolatileArray smem tid
    void $ writeArray g_odata bi x

  readVolatileArray smem tid
--}


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


-- Efficient thread block wide reduction using shared memory. The return value
-- is only valid for thread zero.
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.2/cub/block/specializations/block_reduce_warp_reductions.cuh#L191
--
reduceBlockSMem
    :: forall aenv e. Elt e
    => DeviceProperties                                         -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
    -> IRArray (Vector e)                                       -- ^ temporary storage used by individual warp reductions
    -> IRArray (Vector e)                                       -- ^ temporary storage used for block-wide reduction
    -> IR Int32                                                 -- ^ number of items that will be reduced by this _thread block_
    -> IR e                                                     -- ^ calling thread's input element
    -> CodeGen (IR e)                                           -- ^ thread-block-wide reduction using the specified operator (lane 0 only)
reduceBlockSMem dev combine smem1 smem2 num_valid input = do

  -- Compute a per-warp reduction
  r    <- reduceWarpSMem dev combine smem1 num_valid input

  -- Share the per-warp reductions
  lane <- laneId
  wid  <- warpId
  when (A.eq scalarType lane (lift 0)) $ do
    writeVolatileArray smem2 wid r

  -- Avoid an extra __syncthreads by storing the per-warp reductions into
  -- a separate shared memory array.
  __syncthreads

  -- How many warps were actively computing part of the reduction?
  a   <- A.add  numType      num_valid (lift . P.fromIntegral $ CUDA.warpSize dev - 1)
  b   <- A.quot integralType a         (lift . P.fromIntegral $ CUDA.warpSize dev)

  -- Reduce the per-warp aggregates to a single value.
  --
  -- TODO: Assumes that this can be done in a single step, implying the maximum
  -- thread block size we support is 32 * 32 = 1024. Need to relax this.
  if A.eq scalarType wid (lift 0)
    then reduceWarpSMem dev combine smem1 b =<< readVolatileArray smem2 lane
    else return r


-- Efficient warp reduction using shared memory. The return value is only valid
-- for thread lane zero.
--
-- This is a little awkward because we don't have individual shared memory
-- allocations for each warp, rather a single allocation for the thread block.
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.2/cub/warp/specializations/warp_reduce_smem.cuh#L128
--
-- TODO: Add specialisation for when all elements are valid, thus avoiding an
--       inner branch.
--
reduceWarpSMem
    :: forall aenv e. Elt e
    => DeviceProperties                                         -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
    -> IRArray (Vector e)                                       -- ^ temporary storage array in shared memory
    -> IR Int32                                                 -- ^ number of items that will be reduced by this _thread block_
    -> IR e                                                     -- ^ calling thread's input element
    -> CodeGen (IR e)                                           -- ^ warp-wide reduction using the specified operator (lane 0 only)
reduceWarpSMem dev combine smem num_valid = reduce 0
  where
    log2 :: Double -> Double
    log2  = P.logBase 2

    -- number steps required to reduce warp
    steps = P.floor . log2 . P.fromIntegral . CUDA.warpSize $ dev

    -- Unfold the reduction as a recursive code generation function.
    reduce :: Int -> IR e -> CodeGen (IR e)
    reduce step x
      | step >= steps               = return x
      | offset <- 1 `P.shiftL` step = do
          -- share input through buffer
          tid <- threadIdx
          writeVolatileArray smem tid x

          -- update input if in range
          i    <- A.add numType tid (lift offset)
          x'   <- if A.lt scalarType i num_valid
                    then app2 combine x =<< readVolatileArray smem i
                    else return x

          reduce (step+1) x'


{--
-- roll version
reduceWarpSMem
    :: IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
    -> IRArray (Vector e)                                       -- ^ values in shared memory buffer to reduce
    -> CodeGen (IR e)                                           -- ^ final result
reduceWarpSMem combine smem = do
  let
    zero = ir numType (num numType 0)
    two  = ir numType (num numType 2)
  --
  tid   <- threadIdx
  ws    <- warpSize
  start <- A.quot integralType ws two

  Loop.for start
           (\offset -> gt scalarType offset zero)
           (\offset -> A.quot integralType offset two)
           (\offset -> do
             i <- add numType tid offset
             x <- readVolatileArray smem tid
             y <- readVolatileArray smem i
             z <- app2 combine x y
             writeVolatileArray smem tid z
           )
  readVolatileArray smem tid
--}

