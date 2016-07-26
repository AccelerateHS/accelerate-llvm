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
  = (+++) <$> mkFoldDim dev aenv f (Just z) acc
          <*> mkFoldFill aenv z


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

    -- If the innermost dimension is smaller than the number of threads in the
    -- block, those threads will never contribute to the output.
    tid <- threadIdx
    sz  <- A.fromIntegral integralType numType . indexHead =<< delayedExtent
    when (A.lt scalarType tid sz) $ do

      -- Thread blocks iterate over the outer dimensions, each thread block
      -- cooperatively reducing along each outermost index to a single value.
      --
      gd    <- gridDim
      bid   <- blockIdx
      seg0  <- A.add numType start bid
      for seg0 (\seg -> A.lt scalarType seg end) (\seg -> A.add numType seg gd) $ \seg -> do

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
                   then reduceBlockSMem dev combine Nothing x0
                   else reduceBlockSMem dev combine (Just sz) x0

        -- Step 2: keep walking over the input
        next  <- A.add numType from bd
        r     <- iter next r0 (\i -> A.lt scalarType i to) (\i -> A.add numType i bd) $ \offset r -> do

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
    => Gamma aenv
    -> IRExp PTX aenv e
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkFoldFill aenv seed =
  mkGenerate aenv (IRFun1 (const seed))



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
      smem  <- sharedMem (int32 warp_smem_elems) skip

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
            then reduceWarpSMem dev combine smem Nothing input
            else reduceWarpSMem dev combine smem (Just valid) input

    -- Step 2: Aggregate per-warp reductions
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
      when (A.eq scalarType lane (lift 0)) $ do
        writeArray smem wid input

      -- Wait for each warp to finish its local reduction
      __syncthreads

      -- Update the total aggregate. Thread 0 just does this sequentially (as is
      -- done in CUB), but we could also do this cooperatively (better for
      -- larger thread blocks?)
      tid   <- threadIdx
      if A.eq scalarType tid (lift 0)
        then
          let valid step =
                case size of
                  Nothing -> return (lift True)
                  Just n  -> flip (A.lt scalarType) n =<< A.mul numType step (int32 (CUDA.warpSize dev))
          in
          iter (lift 1)
               input
               (flip (A.lt scalarType) warps)
               (flip (A.add numType) (lift 1))
               (\step x -> if valid step
                             then app2 combine x =<< readArray smem step
                             else return x)
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
          writeVolatileArray smem lane x

          -- update input if in range
          i   <- A.add numType lane (lift offset)
          x'  <- if valid i
                   then app2 combine x =<< readVolatileArray smem i
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

