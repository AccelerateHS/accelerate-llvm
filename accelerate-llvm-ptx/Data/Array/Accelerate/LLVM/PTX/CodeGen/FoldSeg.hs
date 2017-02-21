{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.FoldSeg
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.FoldSeg
  where

-- accelerate
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Sugar                            ( Array, Segments, Shape(rank), (:.), Elt(..) )

-- accelerate-llvm-*
import LLVM.General.AST.Type.Representation

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
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold                  ( reduceBlockSMem, imapFromTo )
-- import Data.Array.Accelerate.LLVM.PTX.CodeGen.Queue
import Data.Array.Accelerate.LLVM.PTX.Context
import Data.Array.Accelerate.LLVM.PTX.Target

-- cuda
import qualified Foreign.CUDA.Analysis                              as CUDA

import Control.Applicative                                          ( (<$>), (<*>) )
import Control.Monad                                                ( void )
import Data.String                                                  ( fromString )
import Prelude                                                      as P


-- Segmented reduction along the innermost dimension of an array. Performs one
-- reduction per segment of the source array.
--
mkFoldSeg
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRExp     PTX aenv e
    -> IRDelayed PTX aenv (Array (sh :. Int) e)
    -> IRDelayed PTX aenv (Segments i)
    -> CodeGen (IROpenAcc PTX aenv (Array (sh :. Int) e))
mkFoldSeg (deviceProperties . ptxContext -> dev) aenv combine seed arr seg =
  mkFoldSegP dev aenv combine (Just seed) arr seg
  -- (+++) <$> mkQueueInit dev
  --       <*> mkFoldSegP dev aenv combine (Just seed) arr seg

-- Segmented reduction along the innermost dimension of an array, where /all/
-- segments are non-empty.
--
mkFold1Seg
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    => PTX
    -> Gamma         aenv
    -> IRFun2    PTX aenv (e -> e -> e)
    -> IRDelayed PTX aenv (Array (sh :. Int) e)
    -> IRDelayed PTX aenv (Segments i)
    -> CodeGen (IROpenAcc PTX aenv (Array (sh :. Int) e))
mkFold1Seg (deviceProperties . ptxContext -> dev) aenv combine arr seg =
  mkFoldSegP dev aenv combine Nothing arr seg
  -- (+++) <$> mkQueueInit dev
  --       <*> mkFoldSegP dev aenv combine Nothing arr seg


-- This implementation assumes that the segments array represents the offset
-- indices to the source array, rather than the lengths of each segment. The
-- segment-offset approach is required for parallel implementations.
--
-- For simplicity, each segment is computed by a single thread block, meaning we
-- don't have to worry about inter-block synchronisation.
--
mkFoldSegP
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    => DeviceProperties
    -> Gamma aenv
    -> IRFun2 PTX aenv (e -> e -> e)
    -> Maybe (IRExp PTX aenv e)
    -> IRDelayed PTX aenv (Array (sh :. Int) e)
    -> IRDelayed PTX aenv (Segments i)
    -> CodeGen (IROpenAcc PTX aenv (Array (sh :. Int) e))
mkFoldSegP dev aenv combine mseed arr seg =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array (sh :. Int) e))
      paramEnv                  = envParam aenv
      --
      config                    = launchConfig dev (CUDA.incWarp dev) dsmem const
      dsmem n                   = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `div` ws
          per_warp  = ws + ws `div` 2
          bytes     = sizeOf (eltType (undefined :: e))
  in
  makeOpenAccWith config "foldSeg" (paramGang ++ paramOut ++ paramEnv) $ do

    -- We use a dynamically scheduled work queue in order to evenly distribute
    -- the uneven workload, due to the variable length of each segment, over the
    -- available thread blocks.
    -- queue <- globalWorkQueue

    -- All threads in the block need to know what the start and end indices of
    -- this segment are in order to participate in the reduction. We use
    -- variables in __shared__ memory to communicate these values between
    -- threads in the block. Furthermore, by using a 2-element array, we can
    -- have the first two threads of the block read the start and end indices as
    -- a single coalesced read, since they will be sequential in the
    -- segment-offset array.
    --
    smem  <- staticSharedMem 2

    -- Compute the number of segments and size of the innermost dimension. These
    -- are required if we are reducing a rank-2 or higher array, to properly
    -- compute the start and end indices of the portion of the array this thread
    -- block reduces. Note that this is a segment-offset array computed by
    -- 'scanl (+) 0' of the segment length array, so its size has increased by
    -- one.
    --
    sz    <- i32 . indexHead =<< delayedExtent arr
    ss    <- do n <- i32 . indexHead =<< delayedExtent seg
                A.sub numType n (lift 1)

    -- Each thread block cooperatively reduces a segment.
    -- s0    <- dequeue queue (lift 1)
    -- for s0 (\s -> A.lt scalarType s end) (\_ -> dequeue queue (lift 1)) $ \s -> do
    imapFromTo start end $ \s -> do

      -- The first two threads of the block determine the indices of the
      -- segments array that we will reduce between and distribute those values
      -- to the other threads in the block.
      tid <- threadIdx
      when (A.lt scalarType tid (lift 2)) $ do
        i <- case rank (undefined::sh) of
               0 -> return s
               _ -> A.rem integralType s ss
        j <- A.add numType i tid
        v <- app1 (delayedLinearIndex seg) =<< A.fromIntegral integralType numType j
        writeArray smem tid =<< i32 v

      -- Once all threads have caught up, begin work on the new segment.
      __syncthreads

      u <- readArray smem (lift 0 :: IR Int32)
      v <- readArray smem (lift 1 :: IR Int32)

      -- Determine the index range of the input array we will reduce over.
      -- Necessary for multidimensional segmented reduction.
      (inf,sup) <- A.unpair <$> case rank (undefined::sh) of
                                  0 -> return (A.pair u v)
                                  _ -> do q <- A.quot integralType s ss
                                          a <- A.mul numType q sz
                                          A.pair <$> A.add numType u a <*> A.add numType v a

      void $
        if A.eq scalarType inf sup
          -- This segment is empty. If this is an exclusive reduction the
          -- first thread writes out the initial element for this segment.
          then do
            case mseed of
              Nothing -> return (IR OP_Unit :: IR ())
              Just z  -> do
                when (A.eq scalarType tid (lift 0)) $ writeArray arrOut s =<< z
                return (IR OP_Unit)

          -- This is a non-empty segment.
          else do
            -- Step 1: initialise local sums
            --
            -- NOTE: We require all threads to enter this branch and execute the
            -- first step, even if they do not have a valid element and must
            -- return 'undef'. If we attempt to skip this entire section for
            -- non-participating threads (i.e. 'when (i0 < sup)'), it seems that
            -- those threads die and will not participate in the computation of
            -- _any_ further segment. I'm not sure if this is a CUDA oddity
            -- (e.g. we must have all threads convergent on __syncthreads) or
            -- a bug in NVPTX.
            --
            bd <- blockDim
            i0 <- A.add numType inf tid
            x0 <- if A.lt scalarType i0 sup
                    then app1 (delayedLinearIndex arr) =<< A.fromIntegral integralType numType i0
                    else let
                             go :: TupleType a -> Operands a
                             go UnitTuple       = OP_Unit
                             go (PairTuple a b) = OP_Pair (go a) (go b)
                             go (SingleTuple t) = ir' t (undef t)
                         in
                         return . IR $ go (eltType (undefined::e))

            v0 <- A.sub numType sup inf
            r0 <- if A.gte scalarType v0 bd
                    then reduceBlockSMem dev combine Nothing   x0
                    else reduceBlockSMem dev combine (Just v0) x0

            -- Step 2: keep walking over the input
            nxt <- A.add numType inf bd
            r   <- iterFromStepTo nxt bd sup r0 $ \offset r -> do

                     -- Wait for threads to catch up before starting the next stripe
                     __syncthreads

                     i' <- A.add numType offset tid
                     v' <- A.sub numType sup offset
                     r' <- if A.gte scalarType v' bd
                             -- All threads in the block are in bounds, so we
                             -- can avoid bounds checks.
                             then do
                               x' <- app1 (delayedLinearIndex arr) =<< A.fromIntegral integralType numType i'
                               reduceBlockSMem dev combine Nothing x'

                             -- Not all threads are valid.
                             else
                             if A.lt scalarType i' sup
                               then do
                                 x' <- app1 (delayedLinearIndex arr) =<< A.fromIntegral integralType numType i'
                                 reduceBlockSMem dev combine (Just v') x'
                               else
                                 return r

                     -- first thread incorporates the result from the previous
                     -- iteration
                     if A.eq scalarType tid (lift 0)
                       then app2 combine r r'
                       else return r'

            -- Step 3: Thread zero writes the aggregate reduction for this
            -- segment to memory. If this is an exclusive fold combine with the
            -- initial element as well.
            when (A.eq scalarType tid (lift 0)) $
             writeArray arrOut s =<<
               case mseed of
                 Nothing -> return r
                 Just z  -> flip (app2 combine) r =<< z  -- Note: initial element on the left

            return (IR OP_Unit)

    return_


i32 :: IsIntegral a => IR a -> CodeGen (IR Int32)
i32 = A.fromIntegral integralType numType

