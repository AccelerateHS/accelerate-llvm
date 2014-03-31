{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Fold
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.CodeGen.Fold
  where

-- llvm-general
import LLVM.General.AST

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
import Data.Array.Accelerate.LLVM.CodeGen.Loop                  as Loop
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.NVVM.Target                   ( NVVM, nvvmDeviceProperties )
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Loop             as Loop

-- CUDA
import Foreign.CUDA.Analysis.Device                             ( DeviceProperties )
import qualified Foreign.CUDA.Analysis.Device                   as CUDA

-- standard library
import Prelude                                                  as P
import Control.Monad


-- Reduce an array along the innermost dimension
--
mkFold
    :: forall t aenv sh e. (Shape sh, Elt e)
    => NVVM
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold nvvm aenv f z a
  -- Either (1) multidimensional fold; or
  --        (2) only using one CPU, so just execute sequentially
  | expDim (undefined::Exp aenv sh) > 0
  = mkFold' nvvm aenv f z a

  -- Parallel foldAll
  | otherwise
  = mkFoldAll' nvvm aenv f z a


-- Multidimensional reduction
-- --------------------------

-- Reduce a multidimensional array along the innermost dimension.
--
mkFold'
    :: (Shape sh, Elt e)
    => NVVM
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold' nvvm aenv f z a = do
  [k1] <- mkFold'warp  nvvm aenv f z a
  [k2] <- mkFold'block nvvm aenv f z a

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
    :: forall t aenv sh e. (Shape sh, Elt e)
    => NVVM
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold'block (nvvmDeviceProperties -> dev) aenv combine seed IRDelayed{..} =
  let
      arrTy             = llvmOfTupleType (eltType (undefined::e))
      arrOut            = arrayData  (undefined::Array sh e) "out"
      shOut             = arrayShape (undefined::Array sh e) "out"
      paramOut          = arrayParam (undefined::Array DIM1 e) "out"
      paramEnv          = envParam aenv

      indexHead         = last -- recall: snoc-list representation for shapes & indices
      i32               = typeOf (int32 :: IntegralType Int32)
  in
  makeKernel "mkFoldByBlock" (paramOut ++ paramEnv) $ do
    initialiseSharedMemory

    ntid                <- blockDim
    nctaid              <- gridDim
    ctaid               <- blockIdx
    tid                 <- threadIdx
    sdata               <- sharedMem (undefined::e) ntid
    segmentSize         <- A.fromIntegral int int32 . indexHead =<< delayedExtent

    -- If each segment has fewer elements than the number of threads in the
    -- block, than the out-of-bounds threads exit immediately. This means the
    -- first read of the array will always succeed.
    --
    main                <- newBlock "main.top"
    exit                <- newBlock "main.exit"
    c1                  <- lt int32 tid segmentSize
    _                   <- cbr c1 main exit

    -- All threads in the block cooperatively reduce a segment. This loop
    -- iterates over the innermost index space.
    --
    setBlock main
    numSegments         <- shapeSize shOut

    Loop.for i32 ctaid
        (\seg -> lt  int32 seg numSegments)
        (\seg -> add int32 seg nctaid)
        (\seg -> do
                    s     <- mul int32 seg segmentSize
                    end   <- add int32 s   segmentSize
                    start <- add int32 s   tid

                    -- Threads of the block sequentially read elements from the
                    -- input array and compute a local sum
                    i     <- A.add int32 start ntid
                    xs    <- delayedLinearIndex [start]
                    ys    <- Loop.iter i32 i
                                 (\i' -> lt  int32 i' end)
                                 (\i' -> add int32 i' ntid)
                                 arrTy xs
                                 (\i' acc -> do xs' <- delayedLinearIndex [i']
                                                combine xs' acc)

                    -- Now cooperatively reduce the local sums to a single value
                    writeVolatileArray sdata tid ys
                    end'  <- A.min int32 end ntid
                    ys'   <- reduceBlock dev arrTy combine ys sdata end' tid tid

                    -- The first thread writes the result including the seed
                    -- element back to global memory
                    ifThen  <- newBlock "finish.if.then"
                    ifExit  <- newBlock "finish.if.exit"
                    c2      <- eq int32 tid (constOp $ num int32 0)
                    _       <- cbr c2 ifThen ifExit

                    setBlock ifThen
                    xs'     <- seed
                    ys''    <- combine xs' ys'
                    writeArray arrOut seg ys''
                    _       <- br ifExit

                    setBlock ifExit)

    _ <- br exit

    setBlock exit
    return_



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
    :: forall t aenv sh e. (Shape sh, Elt e)
    => NVVM
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFold'warp (nvvmDeviceProperties -> dev) aenv combine seed IRDelayed{..} =
  let
      arrTy             = llvmOfTupleType (eltType (undefined::e))
      arrOut            = arrayData  (undefined::Array sh e) "out"
      shOut             = arrayShape (undefined::Array sh e) "out"
      paramOut          = arrayParam (undefined::Array DIM1 e) "out"
      paramEnv          = envParam aenv

      warpSize          = constOp $ num int32 (P.fromIntegral (CUDA.warpSize dev))

      indexHead         = last -- recall: snoc-list representation for shapes & indices

      i32               = typeOf (int32 :: IntegralType Int32)
  in
  makeKernel "mkFoldByWarp" (paramOut ++ paramEnv) $ do
    initialiseSharedMemory

    ntid                <- blockDim
    nctaid              <- gridDim
    tid                 <- threadIdx
    gid                 <- globalThreadIdx
    sdata               <- sharedMem (undefined::e) ntid

    threadLane          <- A.band int32 tid (constOp $ num int32 (P.fromIntegral (CUDA.warpSize dev - 1)))
    segmentSize         <- A.fromIntegral int int32 . indexHead =<< delayedExtent

    -- If each segment has fewer than warpSize elements, than the out-of-bounds
    -- threads can exit immediately. This means the first read read of the input
    -- array will always succeed.
    --
    main                <- newBlock "main.top"
    exit                <- newBlock "main.exit"
    c1                  <- lt int32 threadLane segmentSize
    _                   <- cbr c1 main exit

    -- Threads in a warp cooperatively reduce a segment. This loop iterates over
    -- the innermost index space, yielding the vector a warp should reduce.
    --
    setBlock main
    numSegments         <- shapeSize shOut
    vectorsPerBlock     <- A.quot int32 ntid warpSize
    warpIdx             <- A.quot int32 gid warpSize
    numWarps            <- A.mul  int32 vectorsPerBlock nctaid

    Loop.for i32 warpIdx
        (\seg -> lt  int32 seg numSegments)
        (\seg -> add int32 seg numWarps)
        (\seg -> do
                    s       <- mul int32 seg segmentSize
                    start   <- add int32 s   threadLane
                    end     <- add int32 s   segmentSize

                    -- Threads of the warp sequentially read elements from the
                    -- input array and compute a local sum.
                    --
                    -- TODO: reads are not aligned to a warp boundary, so this
                    -- may issue multiple global memory requests.
                    i       <- A.add int32 start warpSize
                    xs      <- delayedLinearIndex [start]
                    ys      <- Loop.iter i32 i
                                   (\i' -> lt  int32 i' end)
                                   (\i' -> add int32 i' warpSize)
                                   arrTy xs
                                   (\i' acc -> do xs' <- delayedLinearIndex [i']
                                                  combine xs' acc)

                    -- Now cooperatively reduce the local sums to a single value
                    writeVolatileArray sdata tid ys
                    end'    <- A.min int32 end warpSize
                    ys'     <- reduceWarp True dev arrTy combine ys sdata end' threadLane tid

                    -- Finally, the first thread in the wrap writes the result
                    -- to memory
                    ifThen  <- newBlock "finish.if.then"
                    ifExit  <- newBlock "finish.if.exit"
                    c2      <- eq int32 threadLane (constOp $ num int32 0)
                    _       <- cbr c2 ifThen ifExit

                    setBlock ifThen
                    xs'     <- seed
                    ys''    <- combine xs' ys'
                    writeArray arrOut seg ys''
                    _       <- br ifExit

                    setBlock ifExit)

    _ <- br exit

    setBlock exit
    return_


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
mkFoldAll'
    :: forall t aenv sh e. (Shape sh, Elt e)    -- really have sh ~ Z
    => NVVM
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFoldAll' nvvm aenv combine seed arr =
  let
      arrOut            = arrayData  (undefined::Array DIM1 e) "out"
      shOut             = arrayShape (undefined::Array DIM1 e) "out"
      rec               = IRDelayed
        { delayedExtent      = return (map rvalue shOut)        -- See note: [Marshalling foldAll output arrays]
        , delayedIndex       = error "mkFoldAll: delayedIndex"
        , delayedLinearIndex = \[i] -> readArray arrOut i
        }
  in do
  [k1] <- mkFoldAllCore "foldAllIntro" nvvm aenv combine seed arr
  [k2] <- mkFoldAllCore "foldAllRec"   nvvm aenv combine seed rec

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
    :: forall t aenv sh e. (Shape sh, Elt e)
    => Name
    -> NVVM
    -> Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRExp     aenv e
    -> IRDelayed aenv (Array (sh:.Int) e)
    -> CodeGen [Kernel t aenv (Array sh e)]
mkFoldAllCore name (nvvmDeviceProperties -> dev) aenv combine seed IRDelayed{..} =
  let
      arrTy             = llvmOfTupleType (eltType (undefined::e))
      arrOut            = arrayData  (undefined::Array DIM1 e) "out"
      paramOut          = arrayParam (undefined::Array DIM1 e) "out"
      paramEnv          = envParam aenv
  in
  makeKernel name (paramOut ++ paramEnv) $ do
    initialiseSharedMemory

    tid         <- threadIdx
    ntid        <- blockDim
    ctaid       <- blockIdx
    nctaid      <- gridDim
    sdata       <- sharedMem (undefined::e) ntid

    sh          <- delayedExtent
    end         <- shapeSize sh
    step        <- gridSize

    -- If this is an exclusive reduction of an empty shape, then just initialise
    -- the output array with the seed element and exit immediately. Doing this
    -- here means that we can make some assumptions about the liveliness of
    -- threads in the main loop.
    --
    emptyTop    <- newBlock "empty.top"
    emptySeed   <- newBlock "empty.seed"
    main        <- newBlock "main.top"
    exit        <- newBlock "main.exit"
    c1          <- eq int32 end (constOp (num int32 0))
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
    i           <- globalThreadIdx
    c3          <- lt int32 i end
    _           <- cbr c3 reduce exit

    setBlock reduce
    xs          <- delayedLinearIndex [i]
    ys          <- iterFromTo step end arrTy xs $ \i' acc -> do
                      xs' <- delayedLinearIndex [i']
                      combine xs' acc

    writeVolatileArray sdata tid ys

    -- Each thread puts its local sum into shared memory, then cooperatively
    -- reduces the shared array to a single value.
    --
    u           <- A.mul int32 ctaid ntid
    v           <- A.sub int32 end u
    end'        <- A.min int32 ntid v
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



-- Reduction primitives
-- ====================

reduceBlock
    :: DeviceProperties
    -> [Type]                           -- type of element 'e'
    -> IRFun2 aenv (e -> e -> e)        -- combination function
    -> [Operand]                        -- this thread's initial value
    -> [Name]                           -- shared memory array that intermediate and input values are stored in
    -> Operand                          -- number of elements to reduce [0,n) :: Int32
    -> Operand                          -- thread identifier, such as thread lane or threadIdx.x
    -> Operand                          -- where this thread stores its values in shared memory (threadIdx.x)
    -> CodeGen [Operand]                -- variables storing the final reduction value
reduceBlock dev ty combine x0 sdata n ix tid
  | shflOK dev ty = error "butterfly reduction"
  | otherwise     = reduceBlockTree dev ty combine x0 sdata n ix tid


reduceWarp
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
reduceWarp half dev ty combine x0 sdata n ix tid
  | shflOK dev ty = error "butterfly reduction"
  | otherwise     = reduceWarpTree half dev ty combine x0 sdata n ix tid


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

