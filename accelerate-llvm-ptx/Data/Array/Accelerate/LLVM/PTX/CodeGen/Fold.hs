{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold (

  mkFold, mkFold1,

) where

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.Quote.LLVM

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Shape
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic            as A
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.PTX.Target                    ( PTX, ptxDeviceProperties )
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base

-- CUDA
import Foreign.CUDA.Analysis.Device                             ( DeviceProperties )
import qualified Foreign.CUDA.Analysis.Device                   as CUDA

-- standard library
import Prelude                                                  as P
import Control.Monad


-- Reduce an array along the innermost dimension
--
mkFold
    :: forall ptx aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel ptx aenv (Array sh e)]
mkFold ptx aenv f z a
  -- Multidimensional fold
  | expDim (undefined::Exp aenv sh) > 0
  = mkFold' ptx aenv f z a

  -- Parallel foldAll
  | otherwise
  = mkFoldAll' ptx aenv f z a


-- Reduce an array along the innermost dimension. The innermost dimension must
-- not be empty.
--
mkFold1
    :: forall ptx aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel ptx aenv (Array sh e)]
mkFold1 ptx aenv f a
  -- Multidimensional fold
  | expDim (undefined::Exp aenv sh) > 0
  = mkFold1' ptx aenv f a

  -- Parallel foldAll
  | otherwise
  = mkFold1All' ptx aenv f a


-- Multidimensional reduction
-- --------------------------

-- Exclusive reduction of a multidimensional array along the innermost
-- dimension.
--
mkFold'
    :: (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel ptx aenv (Array sh e)]
mkFold' ptx aenv f z a = do
  [k1] <- mkFold'warp  ptx aenv f (Just z) a
  [k2] <- mkFold'block ptx aenv f (Just z) a

  return [k1,k2]


-- Inclusive reduce of a multidimensional array along the innermost dimension.
-- The dimensions must not be empty.
--
mkFold1'
    :: (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel ptx aenv (Array sh e)]
mkFold1' ptx aenv f a = do
  [k1] <- mkFold'warp  ptx aenv f Nothing a
  [k2] <- mkFold'block ptx aenv f Nothing a

  return [k1,k2]


-- Reduce a multidimensional array along the innermost dimension. Each thread
-- block reduces along one innermost dimension index. This means that the kernel
-- is somewhat biased towards arrays that are long and squat, having a large
-- number of elements along each innermost dimension, more so than number of
-- innermost indices to reduce over, since typically devices support far greater
-- threads per block than there are multiprocessors with active thread blocks.
--
-- Thus the kernel requires that the input array have at least:
--
-- > (number multiprocessors * thread blocks per multiprocessor)
--
-- innermost indices to reduce over in order to saturate the device. However
-- each innermost dimension should have at least as many elements as there are
-- threads per block.
--
mkFold'block
    :: forall ptx aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRFun2 aenv (e -> e -> e)
    -> Maybe (IRExp aenv e)
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel ptx aenv (Array sh e)]
mkFold'block (ptxDeviceProperties -> dev) aenv combine mseed IRDelayed{..} =
  let
      arrTy             = llvmOfTupleType (eltType (undefined::e))
      arrOut            = arrayData  (undefined::Array sh e) "out"
      paramOut          = arrayParam (undefined::Array DIM1 e) "out"
      paramEnv          = envParam aenv
      (startSeg, lastSegment, paramGang)
                        = gangParam

      indexHead         = last -- recall: snoc-list representation for shapes & indices
      ty_acc            = llvmOfTupleType (eltType (undefined::e))
  in do
  ntid          <- blockDim
  nctaid        <- gridDim
  ctaid         <- blockIdx
  tid           <- threadIdx
  sdata         <- sharedMem (undefined::e) ntid
  segmentSize   <- A.fromIntegral int int32 . indexHead =<< delayedExtent
  --
  makeKernelQ "foldByBlock" [llgM|
    define void @foldByBlock
    (
        $params:paramGang,
        $params:paramOut,
        $params:paramEnv
    )
    {
        ; -- Splice in the terms already defined outside the quoter?!?
        ; -- Ye-gads this is ugly o_O
        ;
        $bbsM:(exec $ return ())

        ; -- If each segment has fewer elements than the number of threads in the
        ; -- block, than the out-of-bounds threads exit immediately. This means
        ; -- the first read of the array will always succeed.
        ; --
        %c1     = icmp ult i32 $opr:tid, $opr:segmentSize
        br i1 %c1, label %main.top, label %main.exit

        ; -- All threads in the block cooperatively reduce a segment. This loop
        ; -- iterates over the innermost index space.
        ; --
      main.top:
        %firstSegment = add i32 $opr:startSeg, $opr:ctaid
        br label %main.for

      main.for:
        for i32 %seg in %firstSegment to $opr:lastSegment step $opr:nctaid
        {
            %s          = mul i32 %seg, $opr:segmentSize
            %end        = add i32 %s, $opr:segmentSize
            %start      = add i32 %s, $opr:tid
            br label %nextblock

            ; -- Threads of the block sequentially read elements from the input
            ; -- array and compute a local sum
            ; --
            $bbsM:("xs" .=. delayedLinearIndex ("start" :: [Operand]))
            %ii = add i32 %start, $opr:ntid
            br label %nextblock

            for i32 %i in %ii to %end step %ntid with $types:ty_acc %xs as %acc
            {
                $bbsM:("ys" .=. delayedLinearIndex ("i" :: [Operand]))
                $bbsM:("zs" .=. combine ("acc" :: Name) ("ys" :: Name))
                $bbsM:(execRet $ return "zs")
            }

            ; -- Now cooperatively reduce the local sums to a single value
            ; --
            $bbsM:(exec $ writeVolatileArray sdata tid ("acc" :: Name))

            %min.cond   = icmp ule i32 %end, $opr:ntid
            %end1       = select i1 %min.cond, i32 %end, i32 $opr:ntid
            br label %nextblock

            $bbsM:("acc1" .=. reduceBlock dev arrTy combine ("acc" :: Name) sdata "end1" tid tid)

            ; -- The first thread writes the result (including the seed element if
            ; -- this is an exclusive reduction) back to global memory.
            ; --
            %write.cond = icmp eq i32 $opr:(tid), 0
            br i1 %write.cond, label %write, label %exit

          write:
            br label %nextblock
            $bbsM:(exec $ writeArrayWithSeed combine mseed arrOut "seg" ("acc1" :: Name))

          exit:
            ret void
        }
      main.exit:
        ret void
    }
  |]


-- Reduce a multidimensional array along the innermost dimension. Each warp
-- reduces along one innermost dimension index. Thus the kernel requires that
-- the input array have at least:
--
--  > (number multiprocessors * thread blocks per multiprocessor)
--  >   *
--  > (threads per block / warp size)
--
-- innermost indices to reduce over in order to saturate the device. However,
-- each innermost dimension is allowed to be fairly short, of the order of the
-- warp size, and still retain good performance.
--
mkFold'warp
    :: forall ptx aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRFun2 aenv (e -> e -> e)
    -> Maybe (IRExp aenv e)
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel ptx aenv (Array sh e)]
mkFold'warp (ptxDeviceProperties -> dev) aenv combine mseed IRDelayed{..} =
  let
      arrTy             = llvmOfTupleType (eltType (undefined::e))
      arrOut            = arrayData  (undefined::Array sh e) "out"
      paramOut          = arrayParam (undefined::Array DIM1 e) "out"
      paramEnv          = envParam aenv
      (startSeg, lastSegment, paramGang)
                        = gangParam

      warpSize          = constOp $ num int32 (P.fromIntegral (CUDA.warpSize dev))

      indexHead         = last -- recall: snoc-list representation for shapes & indices
      ty_acc            = llvmOfTupleType (eltType (undefined::e))
  in do
  ntid          <- blockDim
  nctaid        <- gridDim
  tid           <- threadIdx
  gid           <- globalThreadIdx
  sdata         <- sharedMem (undefined::e) ntid

  threadLane    <- A.band int32 tid (constOp $ num int32 (P.fromIntegral (CUDA.warpSize dev - 1)))
  segmentSize   <- A.fromIntegral int int32 . indexHead =<< delayedExtent

  makeKernelQ "foldByWarp" [llgM|
    define void @foldByWarp
    (
        $params:paramGang,
        $params:paramOut,
        $params:paramEnv
    )
    {
        ; -- Splice in the terms defined outside the quasi quoter.
        ; -- This makes the baby jesus cry.
        ; --
        $bbsM:(exec $ return ())

        ; -- If each segment has fewer than warpSize elements, than the out-of-bounds
        ; -- threads can exit immediately. This means the first read read of the input
        ; -- array will always succeed.
        ; --
        %entry.cond     = icmp ult i32 $opr:threadLane, $opr:segmentSize
        br i1 %entry.cond, label %main.top, label %main.exit

        ; -- Threads in a warp cooperatively reduce a segment. This loop iterates over
        ; -- the innermost index space, yielding the vector a warp should reduce.
        ; --
      main.top:
        %vectorsPerBlock = udiv i32 $opr:ntid, $opr:warpSize
        %warpIdx         = udiv i32 $opr:gid,  $opr:warpSize
        %numWarps        = mul  i32 %vectorsPerBlock, $opr:nctaid
        %firstSegment    = add  i32 $opr:startSeg, %warpIdx
        br label %main.for

      main.for:
        for i32 %seg in %firstSegment to $opr:lastSegment step %numWarps
        {
            %s          = mul i32 %seg, $opr:segmentSize
            %start      = add i32 %s, $opr:threadLane
            %end        = add i32 %s, $opr:segmentSize
            br label %nextblock

            ; -- Threads of the warp sequentially read elements from the input
            ; -- array and compute a local sum.
            ; --
            ; -- TODO: reads are not aligned to a warp boundary, so this may issue
            ; -- multiple global memory requests.
            ; --
            $bbsM:("xs" .=. delayedLinearIndex ("start" :: [Operand]))
            %start1 = add i32 %start, $opr:warpSize
            br label %nextblock

            for i32 %i in %start1 to %end step $opr:warpSize with $types:ty_acc %xs as %acc
            {
                $bbsM:("ys" .=. delayedLinearIndex ("i" :: [Operand]))
                $bbsM:("zs" .=. (combine ("acc" :: Name) ("ys" :: Name)))
                $bbsM:(execRet (return "zs"))
            }

            ; -- Now cooperatively reduce the local sums to a single value
            ; --
            $bbsM:(exec (writeVolatileArray sdata tid ("acc" :: Name)))
            %min.cond   = icmp ult i32 $opr:tid, $opr:warpSize
            %end1       = select i1 %min.cond, i32 $opr:tid, i32 $opr:warpSize

            br label %nextblock
            $bbsM:("acc1" .=. reduceWarp True dev arrTy combine ("acc" :: Name) sdata "end1" threadLane tid)

            ; -- Finally, the first thread in the wrap writes the result to memory
            ; --
            %write.cond = icmp eq i32 $opr:threadLane, 0
            br i1 %write.cond, label %write, label %exit

          write:
            br label %nextblock
            $bbsM:(exec $ writeArrayWithSeed combine mseed arrOut "seg" ("acc1" :: Name))

          exit:
            ret void
        }
        main.exit:
        ret void
    }
  |]


-- Reduction to scalar
-- -------------------

-- Reduce an array to a single element, with all threads cooperating. Since
-- reductions consume arrays fused into them, the parallel fold requires two
-- steps. At an example, take vector dot-product:
--
-- > dotp xs ys = fold (+) 0 (zipWith (*) xs ys)
--
--   1. The first pass reads in the fused array data, in this case corresponding
--      to the function (\i -> (xs!i) * (ys!i)). Thread blocks write their
--      partial result into global memory.
--
--   2. The second pass reads the partial results from step (1) and continues
--      the reduction. This step is repeated recursively until the input array
--      is small enough such that it can be processed by a single thread block,
--      to produce the final (scalar) output element.
--
-- Note: [Indexing delayed arrays]
--
-- For performance considerations, the GPU code attempts to use native integers
-- for operations such as loop counters. On the GPU, these are 32-bits wide,
-- whereas the host system is typically a 64-bit architecture. LLVM IR is
-- strongly typed, and will not automatically convert between the two integer
-- widths for us; binary operations of mixed type yield a (runtime) type error.
--
-- When loop indices are passed to generated code, we must ensure that these are
-- passed as the type of a Haskell (host) Int, so that it matches the type of
-- the generated IR (arr'). However, in the recursive step we are only directly
-- indexing the array, and so no conversion is necessary (rec).
--
-- The promotion to the host integer type in the first step is only necessary if
-- the indexing function manipulates indices in some fashion. However, at this
-- point we can not determine if this is the case, and so to be safe we must do
-- the conversion.
--
mkFoldAll'
    :: forall ptx aenv sh e. (Shape sh, Elt e)    -- really have sh ~ Z
    => PTX
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel ptx aenv (Array sh e)]
mkFoldAll' ptx aenv combine seed arr =
  let
      arrOut            = arrayData  (undefined::Array DIM1 e) "out"
      shOut             = arrayShape (undefined::Array DIM1 e) "out"
      rec               = IRDelayed
        { delayedExtent      = return (map rvalue shOut)        -- See note: [Marshalling foldAll output arrays]
        , delayedIndex       = error "mkFoldAll: delayedIndex"
        , delayedLinearIndex = \i' -> toIRExp i' >>= \[i] -> readArray arrOut i
        }

      arr'              = IRDelayed
        { delayedExtent      = delayedExtent arr
        , delayedIndex       = delayedIndex arr <=< toInt       -- See note: [Indexing delayed arrays]
        , delayedLinearIndex = delayedLinearIndex arr <=< toInt
        }
  in do
  [k1] <- mkFoldAllCore "foldAllIntro" ptx aenv combine seed arr'
  [k2] <- mkFoldAllCore "foldAllRec"   ptx aenv combine seed rec

  return [k1,k2]


-- Reduce a non-empty array to a single element, with all threads cooperating.
-- See also 'mkFoldAll''.
--
mkFold1All'
    :: forall ptx aenv sh e. (Shape sh, Elt e)  -- really have sh ~ Z
    => PTX
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel ptx aenv (Array sh e)]
mkFold1All' ptx aenv combine arr =
  let
      arrOut            = arrayData  (undefined::Array DIM1 e) "out"
      shOut             = arrayShape (undefined::Array DIM1 e) "out"
      rec               = IRDelayed
        { delayedExtent      = return (map rvalue shOut)        -- See note: [Marshalling foldAll output arrays]
        , delayedIndex       = error "mkFoldAll: delayedIndex"
        , delayedLinearIndex = \i' -> toIRExp i' >>= \[i] -> readArray arrOut i
        }

      arr'              = IRDelayed
        { delayedExtent      = delayedExtent arr
        , delayedIndex       = delayedIndex arr <=< toInt       -- See note: [Indexing delayed arrays]
        , delayedLinearIndex = delayedLinearIndex arr <=< toInt
        }
  in do
  [k1] <- mkFold1AllCore "fold1AllIntro" ptx aenv combine arr'
  [k2] <- mkFold1AllCore "fold1AllRec"   ptx aenv combine rec

  return [k1,k2]


-- The generates the exclusive 'foldAll' kernel, for both the introduction
-- (including fused producers) and recursive reduction steps.
--
-- Note: [Marshalling foldAll output arrays]
--
-- As far as the overall 'foldAll' kernel is concerned, the type of the output
-- array is a singleton array. However, for the multi-step algorithm we actually
-- want the _intermediate_ output array to be a _vector_. This allows the
-- recursive step to track how many iterations remain through the use of
-- 'delayedExtent', so that the reduction can determine whether or not to
-- include the seed element.
--
-- Treating the type of the intermediate array as a vector as opposed to a
-- scalar means that a shape component for the array will be generated for the
-- function parameters.
--
mkFoldAllCore
    :: forall ptx aenv sh e. (Shape sh, Elt e)
    => Name
    -> PTX
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel ptx aenv (Array sh e)]
mkFoldAllCore name (ptxDeviceProperties -> dev) aenv combine seed IRDelayed{..} = do
  let
      (start, end, paramGang)   = gangParam
      arrTy                     = llvmOfTupleType (eltType (undefined::e))
      arrOut                    = arrayData  (undefined::Array DIM1 e) "out"
      paramOut                  = arrayParam (undefined::Array DIM1 e) "out"
      paramEnv                  = envParam aenv
  --
  tid           <- threadIdx
  gtid          <- globalThreadIdx
  ntid          <- blockDim
  ctaid         <- blockIdx
  nctaid        <- gridDim
  sdata         <- sharedMem (undefined::e) ntid
  step          <- gridSize

  makeKernelQ name [llgM|
    define void @mkFoldAllCore
    (
        $params:paramGang,
        $params:paramOut,
        $params:paramEnv
    )
    {
        ; -- Splice in the terms defined outside the quasi quoter.
        ; -- Every time we do this, a fairy dies.
        ; --
        $bbsM:(exec $ return ())

        ; -- If this is an exclusive reduction of an empty shape, then just
        ; -- initialise the output array with the seed element and exit
        ; -- immediately. Doing this here means that we can make some assumptions
        ; -- about the liveliness of threads in the main loop.
        ; --
        %c1 = icmp eq i32 $opr:start, $opr:end
        br i1 %c1, label %empty.top, label %main.top

      empty.top:
        %c2 = icmp eq i32 $opr:tid, 0
        br i1 %c1, label %empty.seed, label %main.exit

      empty.seed:
        br label %nextblock
        $bbsM:(exec $ writeArray arrOut ctaid =<< seed)
        br label %main.exit

        ; -- Each thread sequentially reduces multiple elements. This reduces the
        ; -- overall cost of the algorithm while keeping the work complexity O(n)
        ; -- and the step complexity O(log n). See also Brent's Theorem
        ; -- optimisation.
        ; --
        ; -- The number of elements reduced sequentially is determined by the
        ; -- number of active thread blocks. More blocks result in a larger grid
        ; -- size, hence fewer elements per thread.
        ; --
      main.top:
        %start1 = add i32 $opr:start, $opr:gtid
        %c3     = icmp ult i32 %start1, $opr:end
        br i1 %c3, label %reduce, label %main.exit

      reduce:
        %start2 = add i32 %start1, $opr:(step)
        br label %nextblock

        $bbsM:("xs" .=. delayedLinearIndex ("start1" :: [Operand]))

        for i32 %i in %start2 to $opr:end with $types:arrTy %xs as %acc
        {
            $bbsM:("ys" .=. delayedLinearIndex ("i" :: [Operand]))
            $bbsM:("zs" .=. combine ("ys" :: Name) ("acc" :: Name))
            $bbsM:(execRet $ return "zs")
        }

        $bbsM:(exec $ writeVolatileArray sdata tid ("acc" :: Name))

        ; -- Each thread puts its local sum into shared memory, then cooperatively
        ; -- reduces the shared array to a single value.
        ; --
        ; -- end' = min((end - start) - blockIdx * blockDim, blockDim)
        ; --
        %u        = mul i32 $opr:ctaid, $opr:ntid
        %v        = sub i32 $opr:end,   $opr:start
        %w        = sub i32 %v, %u
        %min.cond = icmp ult i32 $opr:ntid, %w
        %end1     = select i1 %min.cond, i32 $opr:ntid, i32 %w
        br label %nextblock

        $bbsM:("acc1" .=. reduceBlock dev arrTy combine ("acc" :: Name) sdata "end1" tid tid)

        ; -- The first thread writes the result of this block back into global memory
        ; --
        %c4 = icmp eq i32 $opr:tid, 0
        br i1 %c4, label %finish, label %main.exit

      finish:
        ; -- If we are the last phase of a multiblock reduction
        ; --
        %c5 = icmp eq i32 $opr:nctaid, 1
        br i1 %c5, label %finish.if.then, label %finish.if.exit

      finish.if.then:
        br label %nextblock
        ; -- ...then include the seed element when writing the final result...
        ; --
        $bbsM:("seed" .=. seed)
        $bbsM:("acc2" .=. combine ("seed" :: Name) ("acc1" :: Name))

        ; -- ...otherwise just write the result of the parallel reduction
      finish.if.exit:

        ; -- ===================================================================
        ; --            MISSING ABILITY TO INSERT PHI NODES?!?
        ; -- ===================================================================
        br label %main.exit

      main.exit:
        ret void

;--        br label %nextblock
;--
;--        $bbsM:("seed1" .=. seed)
;--        $bbsM:("acc2"  .=. combine ("acc1" :: Name) ("seed1" :: Name))
;--        $bbsM:(do
;--                  x1 <- toIRExp ("acc1" :: Name)
;--                  x2 <- toIRExp ("acc2" :: Name)
;--                  "r" .=. zipWithM (\f t -> instr $ Select "c5" t f []) x1 x2)
;--        $bbsM:(exec $ writeArray arrOut ctaid ("r" :: Name))
;--
;--        main.exit:
;--        ret void
      }
  |]

{-
  in
  makeKernel name (paramGang ++ paramOut ++ paramEnv) $ do
    tid         <- threadIdx
    ntid        <- blockDim
    ctaid       <- blockIdx
    nctaid      <- gridDim
    sdata       <- sharedMem (undefined::e) ntid

    -- If this is an exclusive reduction of an empty shape, then just initialise
    -- the output array with the seed element and exit immediately. Doing this
    -- here means that we can make some assumptions about the liveliness of
    -- threads in the main loop.
    --
    emptyTop    <- newBlock "empty.top"
    emptySeed   <- newBlock "empty.seed"
    main        <- newBlock "main.top"
    exit        <- newBlock "main.exit"
    c1          <- eq int32 start end
    _           <- cbr c1 emptyTop main

    setBlock emptyTop
    c2          <- eq int32 tid (constOp (num int32 0))
    _           <- cbr c2 emptySeed exit

    setBlock emptySeed
    writeArray arrOut ctaid =<< seed
    _           <- br exit

    -- Each thread sequentially reduces multiple elements. This reduces the
    -- overall cost of the algorithm while keeping the work complexity O(n) and
    -- the step complexity O(log n). See also Brent's Theorem optimisation.
    --
    -- The number of elements reduced sequentially is determined by the number
    -- of active thread blocks. More blocks result in a larger grid size, hence
    -- fewer elements per thread.
    --
    setBlock main
    reduce      <- newBlock "reduceBlockSeq"
    i           <- add int32 start =<< globalThreadIdx
    c3          <- lt int32 i end
    _           <- cbr c3 reduce exit

    setBlock reduce
    xs          <- delayedLinearIndex [i]
    step        <- gridSize
    start'      <- add int32 i step
    ys          <- iterFromTo start' end arrTy xs $ \i' acc -> do
                      xs' <- delayedLinearIndex [i']
                      combine xs' acc

    writeVolatileArray sdata tid ys

    -- Each thread puts its local sum into shared memory, then cooperatively
    -- reduces the shared array to a single value.
    --
    -- end' = min((end - start) - blockIdx * blockDim, blockDim)
    --
    u           <- A.mul int32 ctaid ntid
    v           <- A.sub int32 end start
    w           <- A.sub int32 v u
    end'        <- A.min int32 ntid w
    ys'         <- reduceBlock dev arrTy combine ys sdata end' tid tid

    -- The first thread writes the result of this block into global memory.
    --
    finish      <- newBlock "finish"
    ifThen      <- newBlock "finish.if.then"
    ifExit      <- newBlock "finish.if.exit"

    c4          <- eq int32 tid (constOp (num int32 0))
    _           <- cbr c4 finish exit

    -- If we are the last phase of a recursive multi-block reduction...
    setBlock finish
    c5          <- eq int32 nctaid (constOp (num int32 1))
    top         <- cbr c5 ifThen ifExit

    -- ...then include the seed element when writing the final result...
    setBlock ifThen
    xs'         <- seed
    ys''        <- combine xs' ys'
    bot         <- br ifExit

    -- ...otherwise just write the result of the parallel reduction.
    setBlock ifExit
    r           <- zipWithM phi' arrTy [[(t,top), (b,bot)] | t <- ys' | b <- ys'' ]
    writeArray arrOut ctaid r
    _           <- br exit

    -- Done
    setBlock exit
    return_
-}

-- Generate the inclusive 'fold1All' kernel, for both the introduction
-- (including fused producers) and recursive reduction steps. See also
-- 'mkFoldAllCore'.
--
mkFold1AllCore
    :: forall ptx aenv sh e. (Shape sh, Elt e)
    => Name
    -> PTX
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel ptx aenv (Array sh e)]
mkFold1AllCore name (ptxDeviceProperties -> dev) aenv combine IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      arrTy                     = llvmOfTupleType (eltType (undefined::e))
      arrOut                    = arrayData  (undefined::Array DIM1 e) "out"
      paramOut                  = arrayParam (undefined::Array DIM1 e) "out"
      paramEnv                  = envParam aenv
  in do
  tid         <- threadIdx
  gtid        <- globalThreadIdx
  ntid        <- blockDim
  ctaid       <- blockIdx
  sdata       <- sharedMem (undefined::e) ntid
  step        <- gridSize
  k <- [llgM|
  define void @mkFold1AllCore (
    $params:(paramGang) ,
    $params:(paramOut) ,
    $params:(paramEnv)
    ) {
      $bbsM:(exec $ return ())               ;; splice in the BasicBlocks from above
      %start1 = add i32 $opr:(start), $opr:(gtid)
      %c1 = icmp eq i32 %start1, $opr:(end)
      br i1 %c1, label %main.top, label %main.exit

      main.top:
      %start2 = add i32 %start1, $opr:(step)
      br label %nextblock
      $bbsM:("xs" .=. delayedLinearIndex ("start1" :: [Operand]))
      for i32 %i in %start2 to $opr:(end) with $types:(arrTy) %xs as %acc {
        $bbsM:("ys" .=. delayedLinearIndex ("i" :: [Operand]))
        $bbsM:("zs" .=. combine ("ys" :: Name) ("acc" :: Name))
        $bbsM:(execRet $ return "zs")
      }

      $bbsM:(exec $ writeVolatileArray sdata tid ("acc" :: Name))

      %u = mul i32 $opr:(ctaid), $opr:(ntid)
      %v = sub i32 $opr:(end), $opr:(start)
      %w = sub i32 %v, %u
      %end1.cond = icmp ult i32 $opr:(ntid), %w
      %end1 = select i1 %end1.cond, i32 $opr:(ntid), i32 %w
      br label %nextblock
      $bbsM:("acc1" .=. reduceBlock dev arrTy combine ("acc" :: Name) sdata "end1" tid tid)

      %c2 = icmp eq i32 $opr:(tid), 0
      br i1 %c2, label %finish, label %main.exit

      finish:
      br label %nextblock
      $bbsM:(exec $ writeArray arrOut ctaid ("acc1" :: Name))

      main.exit:
      ret void
    }
  |]
  addMetadata "nvvm.annotations" [ Just $ global name
                                 , Just $ MetadataStringOperand "kernel"
                                 , Just $ constOp (num int32 1) ]
  let k1 = k { name = name }
  return $ [Kernel k1]
{-
  in
  makeKernel name (paramGang ++ paramOut ++ paramEnv) $ do
    tid         <- threadIdx
    ntid        <- blockDim
    ctaid       <- blockIdx
    sdata       <- sharedMem (undefined::e) ntid

    -- The shape is guaranteed to be non-empty. The execution phase will raise a
    -- runtime exception if the user calls fold1 on an empty shape.
    --
    main        <- newBlock "main.top"
    exit        <- newBlock "main.exit"
    i           <- add int32 start =<< globalThreadIdx
    c1          <- lt int32 i end
    _           <- cbr c1 main exit

    setBlock main
    xs          <- delayedLinearIndex [i]
    step        <- gridSize
    start'      <- add int32 i step
    ys          <- iterFromTo start' end arrTy xs $ \i' acc -> do
                      xs' <- delayedLinearIndex [i']
                      combine xs' acc

    writeVolatileArray sdata tid ys

    -- Each thread puts its local sum into shared memory, then cooperatively
    -- reduces the shared array to a single value.
    --
    -- end' = min((end - start) - blockIdx * blockDim, blockDim)
    --
    u           <- A.mul int32 ctaid ntid
    v           <- A.sub int32 end start
    w           <- A.sub int32 v u
    end'        <- A.min int32 ntid w
    ys'         <- reduceBlock dev arrTy combine ys sdata end' tid tid

    -- The first thread writes the result of this block back into global memory
    --
    finish      <- newBlock "finish"
    c2          <- eq int32 tid (constOp (num int32 0))
    _           <- cbr c2 finish exit

    setBlock finish
    writeArray arrOut ctaid ys'
    _           <- br exit

    setBlock exit
    return_
-}


-- Reduction primitives
-- ====================

reduceBlock
    :: IROperand a
    => DeviceProperties
    -> [Type]                           -- type of element 'e'
    -> IRFun2 aenv (e -> e -> e)        -- combination function
    -> a                                -- this thread's initial value
    -> [Name]                           -- shared memory array that intermediate and input values are stored in
    -> Operand                          -- number of elements to reduce [0,n) :: Int32
    -> Operand                          -- thread identifier, such as thread lane or threadIdx.x
    -> Operand                          -- where this thread stores its values in shared memory (threadIdx.x)
    -> CodeGen [Operand]                -- variables storing the final reduction value
reduceBlock dev ty combine x0 sdata n ix tid
  | shflOK dev ty = error "butterfly reduction"
  | otherwise     = do x0' <- toIRExp x0
                       reduceBlockTree dev ty combine x0' sdata n ix tid


reduceWarp
    :: IROperand a
    => Bool                         -- is the number of elements in range [0,warpSize) ?
    -> DeviceProperties             -- properties of the target GPU
    -> [Type]                       -- type of element 'e'
    -> IRFun2 aenv (e -> e -> e)    -- combination function
    -> a                            -- this thread's initial value
    -> [Name]                       -- shared memory array that intermediate and input values are stored in
    -> Operand                      -- number of elements to reduce [0,n) :: Int32
    -> Operand                      -- thread identifier, such as thread lane or threadIdx.x
    -> Operand                      -- where this thread stores its values in shared memory (threadIdx.x)
    -> CodeGen [Operand]            -- variables storing the final reduction value
reduceWarp half dev ty combine x0 sdata n ix tid
  | shflOK dev ty = error "butterfly reduction"
  | otherwise     = do x0' <- toIRExp x0
                       reduceWarpTree half dev ty combine x0' sdata n ix tid


-- Butterfly reduction
-- -------------------

-- Since warp synchronous programming is not supported on current hardware, we
-- require thread and memory synchronisation operations whenever we communicate
-- via shared memory, even at sub-warp granularity.
--
-- The Kepler architecture (compute 3.0) introduced warp shuffle instructions,
-- which permit the exchange of variables between threads within a warp without
-- the use of shared memory. Thus, this might improve reduction performance on
-- architectures which support it, since the instruction should have higher
-- throughput than access to shared memory (32ops/clock/multiprocessor) and
-- requires only a single synchronisation for every 64 warps (2048 threads in a
-- block), in order to communicate the partial result of each warp via shared
-- memory.
--
-- <http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#warp-shuffle-functions>
--

shflOK :: DeviceProperties -> [Type] -> Bool
shflOK _dev _ty = False
--shflOK dev ty =
--  CUDA.computeCapability dev >= CUDA.Compute 3 0 && and [ bitSizeOfType t `elem` [32,64] | t <- ty ]


-- Tree reduction
-- --------------

-- Threads of a block cooperatively reduce the elements of a shared memory array
-- (AddrSpace 3). This does bounds checks at every step to ensure that
-- out-of-bounds data is not read, as the shared memory array is not initialised
-- with a neutral element.
--
reduceBlockTree
    :: DeviceProperties
    -> [Type]                           -- type of element 'e'
    -> IRFun2 aenv (e -> e -> e)        -- combination function
    -> [Operand]                        -- this thread's initial value
    -> [Name]                           -- shared memory array that intermediate and input values are stored in
    -> Operand                          -- number of elements to reduce [0,n) :: Int32
    -> Operand                          -- thread identifier, such as thread lane or threadIdx.x
    -> Operand                          -- where this thread stores its values in shared memory (threadIdx.x)
    -> CodeGen [Operand]                -- variables storing the final reduction value
reduceBlockTree dev ty combine x0 sdata n ix tid
  = foldM reduce x0
  $ map pow2 [u-1, u-2 .. v]
  where
    u = P.floor (P.logBase 2 (P.fromIntegral $ CUDA.maxThreadsPerBlock dev :: Double))
    v = P.floor (P.logBase 2 (P.fromIntegral $ CUDA.warpSize dev           :: Double))

    pow2 :: Int32 -> Int32
    pow2 x = 2 ^ x

    reduce :: [Operand] -> Int32 -> CodeGen [Operand]
    reduce xs step = do
      ifThen    <- newBlock ("reduceBlockTree.then" ++ show step)
      ifExit    <- newBlock ("reduceBlockTree.exit" ++ show step)
      __syncthreads

      if step > P.fromIntegral (CUDA.warpSize dev)
        -- Ensure that threads synchronise before _both_ reading from or writing
        -- to shared memory. Synchronising after each reduction step is not
        -- enough, because one warp could update the partial results before a
        -- different warp has had a chance to read in their data for this step.
        --
        -- Additionally, note that all threads of a warp must participate in the
        -- synchronisation. Thus, this must go outside of the test against the
        -- bounds of the array. We do a bit of extra work here, with all threads
        -- writing into shared memory whether they updated their value or not.
        then do
          i         <- add int32 tid (constOp $ num int32 step)
          c         <- lt int32 i n
          top       <- cbr c ifThen ifExit

          setBlock ifThen
          ys        <- readVolatileArray sdata i
          xs'       <- combine xs ys
          bot       <- br ifExit

          setBlock ifExit
          r         <- zipWithM phi' ty [[(t,top), (b,bot)] | t <- xs | b <- xs' ]
          __syncthreads
          writeVolatileArray sdata tid r
          return r

      -- The threads of a warp execute in lockstep, so it is only necessary to
      -- synchronise at the top to ensure all threads have written their results
      -- into shared memory.
      else do
        c         <- lt int32 tid (constOp $ num int32 (P.fromIntegral (CUDA.warpSize dev)))
        top       <- cbr c ifThen ifExit

        setBlock ifThen
        xs'       <- reduceWarpTree False dev ty combine xs sdata n ix tid
        bot       <- br ifExit

        setBlock ifExit
        zipWithM phi' ty [[(t,top), (b,bot)] | t <- xs | b <- xs' ]


-- Threads of a warp cooperatively reduce the elements of the named shared
-- memory array (AddrSpace 3). Although threads of a warp execute in lockstep,
-- we still require a memory barrier around access to shared memory to ensure
-- that the store is committed. As usual, we do bounds checks at every step to
-- ensure that only valid elements are read (the shared memory array is not
-- initialised with a neutral element).
--
-- If the number of elements to reduce is in the range [0,warpSize), then set
-- the first argument to 'True'. This means only the bottom half of the warp
-- needs to do any work. For example, this is the case if the warp operates
-- independently, as in mkFold'warp.
--
-- If the number of elements is in the range [0,2*warpSize) set the first
-- argument to 'False'. For example, this is the case for the cooperative thread
-- block reduction in mkFold'block.
--
reduceWarpTree
    :: Bool                         -- is the number of elements in range [0,warpSize) ?
    -> DeviceProperties             -- properties of the target GPU
    -> [Type]                       -- type of element 'e'
    -> IRFun2 aenv (e -> e -> e)    -- combination function
    -> [Operand]                    -- this thread's initial value
    -> [Name]                       -- shared memory array that intermediate and input values are stored in
    -> Operand                      -- number of elements to reduce [0,n) :: Int32
    -> Operand                      -- thread identifier, such as thread lane or threadIdx.x
    -> Operand                      -- where this thread stores its values in shared memory (threadIdx.x)
    -> CodeGen [Operand]            -- variables storing the final reduction value
reduceWarpTree half dev ty combine x0 sdata n ix tid
  = foldM reduce x0
  $ map pow2 segs
  where
    v    = P.floor (P.logBase 2 (P.fromIntegral (CUDA.warpSize dev) :: Double))
    segs = if half
              then [   v-1, v-2 .. 0]
              else [v, v-1      .. 0]

    pow2 :: Int32 -> Int32
    pow2 x = 2 ^ x

    reduce :: [Operand] -> Int32 -> CodeGen [Operand]
    reduce xs step = do
      ifThen    <- newBlock ("reduceWarpTree.then" ++ show step)
      ifExit    <- newBlock ("reduceWarpTree.exit" ++ show step)

      -- It is important that the memory fence operations goes outside of the
      -- branch, to ensure that it is visible by all threads in the warp.
      --
      -- If this step reduces [0,2*warpSize) elements, synchronise both memory
      -- and instructions with a full __syncthreads().
      if (step == pow2 v)
         then __syncthreads
         else __threadfence_block

      -- if ( ix + step < n )
      o         <- add int32 ix (constOp $ num int32 step)
      c         <- lt int32 o n
      top       <- cbr c ifThen ifExit

      -- then xs := xs `combine` sdata [ tid + step ]
      setBlock ifThen
      i         <- add int32 tid (constOp $ num int32 step)
      ys        <- readVolatileArray sdata i
      xs'       <- combine xs ys
      bot       <- br ifExit

      -- We don't need to do the fence or write to shared memory at the last
      -- step. The final value is returned in-register.
      setBlock ifExit
      res       <- zipWithM phi' ty [[(t,top), (b,bot)] | t <- xs | b <- xs' ]
      unless (step == pow2 0) $ do __threadfence_block
                                   writeVolatileArray sdata tid res
      return res


-- Store a value into an array at a given index. If the seed value is not
-- Nothing, then this is combined with the given value first.
--
writeArrayWithSeed
    :: (IROperand a, IROperand b)
    => (a -> b -> CodeGen a)            -- Use this function to combine the...
    -> Maybe (CodeGen a)                -- ...seed value (if any) before writing the result to
    -> [Name]                           -- ...the output array
    -> Operand                          -- ...at this index
    -> b                                -- The base value to store
    -> CodeGen ()
writeArrayWithSeed combine mseed arrOut seg ys' =
  case mseed of
    Nothing   -> writeArray arrOut seg ys'
    Just seed -> do
      xs'     <- seed
      ys''    <- combine xs' ys'
      writeArray arrOut seg ys''

