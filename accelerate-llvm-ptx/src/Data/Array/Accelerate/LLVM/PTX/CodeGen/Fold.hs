{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold
  where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Sugar                            ( Array, Scalar, Vector, Shape, Z, (:.), Elt(..), DIM0, DIM1 )

-- accelerate-llvm-*
import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Loop                      as Loop
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
import Data.Array.Accelerate.LLVM.PTX.Target

import LLVM.AST.Type.Representation

-- cuda
import qualified Foreign.CUDA.Analysis                              as CUDA

import Control.Monad                                                ( (>=>) )
import Control.Monad.State                                          ( gets )
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
    => Gamma          aenv
    -> IRFun2     PTX aenv (e -> e -> e)
    -> IRExp      PTX aenv e
    -> MIRDelayed PTX aenv (Array (sh :. Int) e)
    -> CodeGen    PTX      (IROpenAcc PTX aenv (Array sh e))
mkFold aenv f z acc
  | Just Refl <- matchShapeType @sh @Z
  = (+++) <$> mkFoldAll  aenv f (Just z) acc
          <*> mkFoldFill aenv z

  | otherwise
  = (+++) <$> mkFoldDim  aenv f (Just z) acc
          <*> mkFoldFill aenv z


-- Reduce a non-empty array along the innermost dimension. The reduction
-- function must be associative to allow for an efficient parallel
-- implementation.
--
-- TODO: Specialise for commutative operations (such as (+)) and those with
--       a neutral element {(+), 0}
--
mkFold1
    :: forall aenv sh e. (Shape sh, Elt e)
    => Gamma          aenv
    -> IRFun2     PTX aenv (e -> e -> e)
    -> MIRDelayed PTX aenv (Array (sh :. Int) e)
    -> CodeGen    PTX      (IROpenAcc PTX aenv (Array sh e))
mkFold1 aenv f acc
  | Just Refl <- matchShapeType @sh @Z
  = mkFoldAll aenv f Nothing acc

  | otherwise
  = mkFoldDim aenv f Nothing acc


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
    => Gamma          aenv                      -- ^ array environment
    -> IRFun2     PTX aenv (e -> e -> e)        -- ^ combination function
    -> MIRExp     PTX aenv e                    -- ^ (optional) initial element for exclusive reductions
    -> MIRDelayed PTX aenv (Vector e)           -- ^ input data
    -> CodeGen    PTX      (IROpenAcc PTX aenv (Scalar e))
mkFoldAll aenv combine mseed macc = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  foldr1 (+++) <$> sequence [ mkFoldAllS  dev aenv combine mseed macc
                            , mkFoldAllM1 dev aenv combine       macc
                            , mkFoldAllM2 dev aenv combine mseed
                            ]


-- Reduction to an array to a single element, for small arrays which can be
-- processed by a single thread block.
--
mkFoldAllS
    :: forall aenv e. Elt e
    => DeviceProperties                         -- ^ properties of the target GPU
    -> Gamma          aenv                      -- ^ array environment
    -> IRFun2     PTX aenv (e -> e -> e)        -- ^ combination function
    -> MIRExp     PTX aenv e                    -- ^ (optional) initial element for exclusive reductions
    -> MIRDelayed PTX aenv (Vector e)           -- ^ input data
    -> CodeGen    PTX      (IROpenAcc PTX aenv (Scalar e))
mkFoldAllS dev aenv combine mseed marr =
  let
      (arrOut, paramOut)  = mutableArray @DIM0 "out"
      (arrIn,  paramIn)   = delayedArray @DIM1 "in" marr
      paramEnv            = envParam aenv
      --
      config              = launchConfig dev (CUDA.incWarp dev) smem multipleOf multipleOfQ
      smem n              = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `P.quot` ws
          per_warp  = ws + ws `P.quot` 2
          bytes     = sizeOf (eltType @e)
  in
  makeOpenAccWith config "foldAllS" (paramOut ++ paramIn ++ paramEnv) $ do

    tid     <- threadIdx
    bd      <- blockDim

    sh      <- delayedExtent arrIn
    end     <- shapeSize sh

    -- We can assume that there is only a single thread block
    start'  <- return (lift 0)
    end'    <- i32 end
    i0      <- A.add numType start' tid
    sz      <- A.sub numType end' start'
    when (A.lt singleType i0 sz) $ do

      -- Thread reads initial element and then participates in block-wide
      -- reduction.
      x0 <- app1 (delayedLinearIndex arrIn) =<< int i0
      r0 <- if A.eq singleType sz bd
              then reduceBlockSMem dev combine Nothing   x0
              else reduceBlockSMem dev combine (Just sz) x0

      when (A.eq singleType tid (lift 0)) $
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
    => DeviceProperties                         -- ^ properties of the target GPU
    -> Gamma          aenv                      -- ^ array environment
    -> IRFun2     PTX aenv (e -> e -> e)        -- ^ combination function
    -> MIRDelayed PTX aenv (Vector e)           -- ^ input data
    -> CodeGen    PTX      (IROpenAcc PTX aenv (Scalar e))
mkFoldAllM1 dev aenv combine marr =
  let
      (arrTmp, paramTmp)  = mutableArray @DIM1 "tmp"
      (arrIn,  paramIn)   = delayedArray @DIM1 "in" marr
      paramEnv            = envParam aenv
      start               = lift 0
      --
      config              = launchConfig dev (CUDA.incWarp dev) smem const [|| const ||]
      smem n              = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `P.quot` ws
          per_warp  = ws + ws `P.quot` 2
          bytes     = sizeOf (eltType @e)
  in
  makeOpenAccWith config "foldAllM1" (paramTmp ++ paramIn ++ paramEnv) $ do

    -- Each thread block cooperatively reduces a stripe of the input and stores
    -- that value into a temporary array at a corresponding index. Since the
    -- order of operations remains fixed, this method supports non-commutative
    -- reductions.
    --
    tid   <- threadIdx
    bd    <- int =<< blockDim
    sz    <- indexHead <$> delayedExtent arrIn
    end   <- shapeSize (irArrayShape arrTmp)

    imapFromTo start end $ \seg -> do

      -- Wait for all threads to catch up before beginning the stripe
      __syncthreads

      -- Bounds of the input array we will reduce between
      from  <- A.mul numType    seg  bd
      step  <- A.add numType    from bd
      to    <- A.min singleType sz   step

      -- Threads cooperatively reduce this stripe
      reduceFromTo dev from to combine
        (app1 (delayedLinearIndex arrIn))
        (when (A.eq singleType tid (lift 0)) . writeArray arrTmp seg)

    return_


-- Reduction of an array to a single element, (recursive) step 2 of multi-block
-- reduction algorithm.
--
mkFoldAllM2
    :: forall aenv e. Elt e
    => DeviceProperties
    -> Gamma       aenv
    -> IRFun2  PTX aenv (e -> e -> e)
    -> MIRExp  PTX aenv e
    -> CodeGen PTX      (IROpenAcc PTX aenv (Scalar e))
mkFoldAllM2 dev aenv combine mseed =
  let
      (arrTmp, paramTmp)  = mutableArray @DIM1 "tmp"
      (arrOut, paramOut)  = mutableArray @DIM1 "out"
      paramEnv            = envParam aenv
      start               = lift 0
      --
      config              = launchConfig dev (CUDA.incWarp dev) smem const [|| const ||]
      smem n              = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `P.quot` ws
          per_warp  = ws + ws `P.quot` 2
          bytes     = sizeOf (eltType @e)
  in
  makeOpenAccWith config "foldAllM2" (paramTmp ++ paramOut ++ paramEnv) $ do

    -- Threads cooperatively reduce a stripe of the input (temporary) array
    -- output from the first phase, storing the results into another temporary.
    -- When only a single thread block remains, we have reached the final
    -- reduction step and add the initial element (for exclusive reductions).
    --
    tid   <- threadIdx
    gd    <- gridDim
    bd    <- int =<< blockDim
    sz    <- return $ indexHead (irArrayShape arrTmp)
    end   <- shapeSize (irArrayShape arrOut)

    imapFromTo start end $ \seg -> do

      -- Wait for all threads to catch up before beginning the stripe
      __syncthreads

      -- Bounds of the input we will reduce between
      from  <- A.mul numType    seg  bd
      step  <- A.add numType    from bd
      to    <- A.min singleType sz   step

      -- Threads cooperatively reduce this stripe
      reduceFromTo dev from to combine (readArray arrTmp) $ \r ->
        when (A.eq singleType tid (lift 0)) $
          writeArray arrOut seg =<<
            case mseed of
              Nothing -> return r
              Just z  -> if A.eq singleType gd (lift 1)
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
    => Gamma aenv                                     -- ^ array environment
    -> IRFun2     PTX aenv (e -> e -> e)              -- ^ combination function
    -> MIRExp     PTX aenv e                          -- ^ (optional) seed element, if this is an exclusive reduction
    -> MIRDelayed PTX aenv (Array (sh :. Int) e)      -- ^ input data
    -> CodeGen    PTX      (IROpenAcc PTX aenv (Array sh e))
mkFoldDim aenv combine mseed marr = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  --
  let
      (arrOut, paramOut)  = mutableArray @sh        "out"
      (arrIn,  paramIn)   = delayedArray @(sh:.Int) "in" marr
      paramEnv            = envParam aenv
      --
      config              = launchConfig dev (CUDA.incWarp dev) smem const [|| const ||]
      smem n              = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `P.quot` ws
          per_warp  = ws + ws `P.quot` 2
          bytes     = sizeOf (eltType @e)
  --
  makeOpenAccWith config "fold" (paramOut ++ paramIn ++ paramEnv) $ do

    -- If the innermost dimension is smaller than the number of threads in the
    -- block, those threads will never contribute to the output.
    tid   <- threadIdx
    sz    <- indexHead <$> delayedExtent arrIn
    sz'   <- i32 sz

    when (A.lt singleType tid sz') $ do

      start <- return (lift 0)
      end   <- shapeSize (irArrayShape arrOut)

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

        i0    <- A.add numType from =<< int tid
        x0    <- app1 (delayedLinearIndex arrIn) i0
        bd    <- blockDim
        r0    <- if A.gte singleType sz' bd
                   then reduceBlockSMem dev combine Nothing    x0
                   else reduceBlockSMem dev combine (Just sz') x0

        -- Step 2: keep walking over the input
        bd'   <- int bd
        next  <- A.add numType from bd'
        r     <- iterFromStepTo next bd' to r0 $ \offset r -> do

          -- Wait for all threads to catch up before starting the next stripe
          __syncthreads

          -- Threads cooperatively reduce this stripe of the input
          i   <- A.add numType offset =<< int tid
          v'  <- A.sub numType to offset
          r'  <- if A.gte singleType v' bd'
                   -- All threads of the block are valid, so we can avoid
                   -- bounds checks.
                   then do
                     x <- app1 (delayedLinearIndex arrIn) i
                     y <- reduceBlockSMem dev combine Nothing x
                     return y

                   -- Otherwise, we require bounds checks when reading the input
                   -- and during the reduction. Note that even though only the
                   -- valid threads will contribute useful work in the
                   -- reduction, we must still have all threads enter the
                   -- reduction procedure to avoid synchronisation divergence.
                   else do
                     x <- if A.lt singleType i to
                            then app1 (delayedLinearIndex arrIn) i
                            else let
                                     go :: TupleType a -> Operands a
                                     go TypeRunit       = OP_Unit
                                     go (TypeRpair a b) = OP_Pair (go a) (go b)
                                     go (TypeRscalar t) = ir' t (undef t)
                                 in
                                 return . IR $ go (eltType @e)

                     v <- i32 v'
                     y <- reduceBlockSMem dev combine (Just v) x
                     return y

          if A.eq singleType tid (lift 0)
            then app2 combine r r'
            else return r'

        -- Step 3: Thread 0 writes the aggregate reduction of this dimension to
        -- memory. If this is an exclusive fold, combine with the initial element.
        --
        when (A.eq singleType tid (lift 0)) $
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
    => Gamma       aenv
    -> IRExp   PTX aenv e
    -> CodeGen PTX      (IROpenAcc PTX aenv (Array sh e))
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
    => DeviceProperties                         -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)            -- ^ combination function
    -> Maybe (IR Int32)                         -- ^ number of valid elements (may be less than block size)
    -> IR e                                     -- ^ calling thread's input element
    -> CodeGen PTX (IR e)                       -- ^ thread-block-wide reduction using the specified operator (lane 0 only)
reduceBlockSMem dev combine size = warpReduce >=> warpAggregate
  where
    int32 :: Integral a => a -> IR Int32
    int32 = lift . P.fromIntegral

    -- Temporary storage required for each warp
    bytes           = sizeOf (eltType @e)
    warp_smem_elems = CUDA.warpSize dev + (CUDA.warpSize dev `P.quot` 2)

    -- Step 1: Reduction in every warp
    --
    warpReduce :: IR e -> CodeGen PTX (IR e)
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
          if A.gte singleType valid (int32 (CUDA.warpSize dev))
            then reduceWarpSMem dev combine smem Nothing      input
            else reduceWarpSMem dev combine smem (Just valid) input

    -- Step 2: Aggregate per-warp reductions
    --
    warpAggregate :: IR e -> CodeGen PTX (IR e)
    warpAggregate input = do
      -- Allocate #warps elements of shared memory
      bd    <- blockDim
      warps <- A.quot integralType bd (int32 (CUDA.warpSize dev))
      skip  <- A.mul numType warps (int32 (warp_smem_elems * bytes))
      smem  <- dynamicSharedMem warps skip

      -- Share the per-lane aggregates
      wid   <- warpId
      lane  <- laneId
      when (A.eq singleType lane (lift 0)) $ do
        writeArray smem wid input

      -- Wait for each warp to finish its local reduction
      __syncthreads

      -- Update the total aggregate. Thread 0 just does this sequentially (as is
      -- done in CUB), but we could also do this cooperatively (better for
      -- larger thread blocks?)
      tid   <- threadIdx
      if A.eq singleType tid (lift 0)
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
    => DeviceProperties                         -- ^ properties of the target device
    -> IRFun2 PTX aenv (e -> e -> e)            -- ^ combination function
    -> IRArray (Vector e)                       -- ^ temporary storage array in shared memory (1.5 warp size elements)
    -> Maybe (IR Int32)                         -- ^ number of items that will be reduced by this warp, otherwise all lanes are valid
    -> IR e                                     -- ^ calling thread's input element
    -> CodeGen PTX (IR e)                       -- ^ warp-wide reduction using the specified operator (lane 0 only)
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
        Just n  -> A.lt singleType i n

    -- Unfold the reduction as a recursive code generation function.
    reduce :: Int -> IR e -> CodeGen PTX (IR e)
    reduce step x
      | step >= steps = return x
      | otherwise     = do
          let offset = lift (1 `P.shiftL` step)

          -- share input through buffer
          lane <- laneId
          writeArray smem lane x

          __syncwarp

          -- update input if in range
          i   <- A.add numType lane offset
          x'  <- if valid i
                   then app2 combine x =<< readArray smem i
                   else return x

          __syncwarp

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
    -> IR Int                                   -- ^ starting index
    -> IR Int                                   -- ^ final index (exclusive)
    -> (IRFun2 PTX aenv (a -> a -> a))          -- ^ combination function
    -> (IR Int -> CodeGen PTX (IR a))           -- ^ function to retrieve element at index
    -> (IR a -> CodeGen PTX ())                 -- ^ what to do with the value
    -> CodeGen PTX ()
reduceFromTo dev from to combine get set = do

  tid   <- int =<< threadIdx
  bd    <- int =<< blockDim

  valid <- A.sub numType to from
  i     <- A.add numType from tid

  _     <- if A.gte singleType valid bd
             then do
               -- All threads in the block will participate in the reduction, so
               -- we can avoid bounds checks
               x <- get i
               r <- reduceBlockSMem dev combine Nothing x
               set r

               return (lift ())
             else do
               -- Only in-bounds threads can read their input and participate in
               -- the reduction
               when (A.lt singleType i to) $ do
                 x <- get i
                 v <- i32 valid
                 r <- reduceBlockSMem dev combine (Just v) x
                 set r

               return (lift ())

  return ()



-- Utilities
-- ---------

i32 :: IR Int -> CodeGen PTX (IR Int32)
i32 = A.fromIntegral integralType numType

int :: IR Int32 -> CodeGen PTX (IR Int)
int = A.fromIntegral integralType numType

imapFromTo
    :: IR Int
    -> IR Int
    -> (IR Int -> CodeGen PTX ())
    -> CodeGen PTX ()
imapFromTo start end body = do
  bid <- int =<< blockIdx
  gd  <- int =<< gridDim
  i0  <- A.add numType start bid
  imapFromStepTo i0 gd end body

