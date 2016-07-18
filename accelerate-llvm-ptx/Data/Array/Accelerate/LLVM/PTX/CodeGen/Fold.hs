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

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold (

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

import Foreign.CUDA.Analysis as CUDA


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
mkFoldAll dev aenv combine mseed IRDelayed{..} =
  error "TODO: PTX.mkFoldAll"


-- Reduce an array of arbitrary rank along the innermost dimension only.
--
-- For simplicity, each element of the output (reduction along an
-- innermost-dimension index) is computed by a single thread block, meaning we
-- don't have to worry about inter-block synchronisation.
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
    error "TODO: PTX.mkFoldDim"



{--
  __shared__ tmp[]

  // #elements in innermost dimension
  int n = indexHead delayedExtent

  // Threads in a block cooperatively reduce along one dimension. The loop bounds are
  // essentially [0, product (indexTail delayedExtent)), but are passed in as
  // parameters to the kernel
  for (dim = start + blockIdx.x; dim < end; dim += gridDim.x) {

    // Assuming exclusive scan in the example
    if (threadIdx.x == 0) {
      carry = seed
    }

    base = dim * n;

    for (i = threadIdx.x; i < n; i+= blockDim.x) {
      tmp[threadIdx.x] = delayedLinearIndex (base + i);

      // threads in the block cooperatively reduce the shared memory array tmp.
      // The final reduction result is stored at tmp[0].
      reduceBlock tmp

      // First thread carries in the result from the previous chunk
      if (threadIdx.x == 0) {
        carry = combine carry tmp[0]
      }
    }

    // First thread writes the final result to the output array
    if (threadIdx.x == 0) {
      out[dim] = carry
    }
  }
--}


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


reduceBlockSMem
  :: forall aenv e. Elt e
  => IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
  -> IRArray (Vector e)                                       -- ^ input  data array
  -> IRArray (Vector e)                                       -- ^ output data array
  -> CodeGen (IR e)                                           -- ^ final result
reduceBlockSMem combine g_idata g_odata = do
  let
    zero = ir numType (num numType 0)
    two  = ir numType (num numType 2)
  tid  <- threadIdx
  bd   <- blockDim
  bi   <- blockIdx
  ws   <- PTXBase.warpSize

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

-- Efficient warp reduction using __shfl_up instruction (compute >= 3.0)
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.2/cub/warp/specializations/warp_reduce_shfl.cuh#L310
--
-- TLM: Actually this might be tricky because the __shfl_up() operation is
--      defined in the sm_30_intrinsics.hpp header file using raw assembly.
--
reduceWarpShfl
    :: IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
    -> IR e                                                     -- ^ this thread's input value
    -> CodeGen (IR e)                                           -- ^ final result
reduceWarpShfl combine input =
  error "TODO: PTX.reduceWarpShfl"


-- Efficient warp reduction using shared memory
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.2/cub/warp/specializations/warp_reduce_smem.cuh#L128
--
-- pseudo code:
--  reduceWarpSMem(combine, smem) {
--    for (offset = warpSize / 2; offset > 0; offset /= 2) {
--      smem[tid] = combine(smem[tid], smem[tid + offset]);
--    }
--    return smem[0];
--  }


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
  ws    <- PTXBase.warpSize
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

-- -- unroll version
-- reduceWarpSMem
--     :: DeviceProperties
--     -> IRFun2 PTX aenv (e -> e -> e)                            -- ^ combination function
--     -> IRArray (Vector e)                                       -- ^ values in shared memory buffer to reduce
--     -> CodeGen (IR e)                                           -- ^ final result
-- reduceWarpSMem dev combine smem = do
--     let
--       steps = takeWhile (> 0)
--         $ iterate (`P.quot` 2)
--         $ CUDA.warpSize dev `P.quot` 2
--     tid <- A.fromIntegral integralType numType =<< threadIdx
--     -- Generate an unrolled loop
--     forM_ steps $ \(ir numType . num numType -> offset) -> do
--       i <- add numType tid offset
--       x <- readVolatileArray smem tid
--       y <- readVolatileArray smem i
--       z <- app2 combine x y
--       writeVolatileArray smem tid z
--
--     readVolatileArray smem tid

