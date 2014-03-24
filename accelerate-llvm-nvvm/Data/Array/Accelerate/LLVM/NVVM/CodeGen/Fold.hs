{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Fold
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
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
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.NVVM.Target                   ( NVVM, nvvmDeviceProperties )
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Loop

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
  = error "todo: mkFold'" -- mkFold' aenv f z a

  -- Parallel foldAll
  | otherwise
  = mkFoldAll' nvvm aenv f z a


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
mkFoldAll' (nvvmDeviceProperties -> dev) aenv combine seed IRDelayed{..} =
  let
      arrOut            = arrayData  (undefined::Array sh e) "out"
      paramOut          = arrayParam (undefined::Array sh e) "out"
      paramEnv          = envParam aenv

      arrTy             = llvmOfTupleType (eltType (undefined::e))
  in
  makeKernel "foldAll" (paramOut ++ paramEnv) $ do
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
    c3          <- lt int32 tid end
    _           <- cbr c3 reduce exit

    setBlock reduce
    start       <- add int32 tid step
    xs          <- delayedLinearIndex [tid]
    ys          <- iterFromTo start end arrTy xs $ \i acc -> do
                      xs' <- delayedLinearIndex [i]
                      combine xs' acc

    writeVolatileArray sdata tid ys

    -- Each thread puts its local sum into shared memory, then cooperatively
    -- reduces the shared array to a single value.
    --
    u           <- A.mul int32 ctaid ntid
    v           <- A.sub int32 end u
    end'        <- A.min int32 ntid v
    ys'         <- reduceBlockTree dev arrTy combine ys sdata end' tid tid

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


-- Cooperative tree reduction
-- --------------------------

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
      _then     <- newBlock ("reduceBlockTree.then" ++ show step)
      _exit     <- newBlock ("reduceBlockTree.exit" ++ show step)
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
          top       <- cbr c _then _exit

          setBlock _then
          ys        <- readVolatileArray sdata i
          xs'       <- combine xs ys
          bot       <- br _exit

          setBlock _exit
          r         <- zipWithM phi' ty [[(t,top), (b,bot)] | t <- xs | b <- xs' ]
          __syncthreads
          writeVolatileArray sdata tid r
          return r

      -- The threads of a warp execute in lockstep, so it is only necessary to
      -- synchronise at the top to ensure all threads have written their results
      -- into shared memory.
      else do
        c         <- lt int32 tid =<< warpSize
        top       <- cbr c _then _exit

        setBlock _then
        xs'       <- reduceWarpTree dev ty combine xs sdata n ix tid
        bot       <- br _exit

        setBlock _exit
        zipWithM phi' ty [[(t,top), (b,bot)] | t <- xs | b <- xs' ]


-- Threads of a warp cooperatively reduce the elements of the named shared
-- memory array (AddrSpace 3). This procedure uses warp-synchronous programming
-- to avoid the use of __syncthreads(), but we still do a bounds check to ensure
-- only valid elements are read (the shared memory array is not initialised with
-- a neutral element).
--
reduceWarpTree
    :: DeviceProperties
    -> [Type]                       -- type of element 'e'
    -> IRFun2 aenv (e -> e -> e)    -- combination function
    -> [Operand]                    -- this thread's initial value
    -> [Name]                       -- shared memory array that intermediate and input values are stored in
    -> Operand                      -- number of elements to reduce [0,n) :: Int32
    -> Operand                      -- thread identifier, such as thread lane or threadIdx.x
    -> Operand                      -- where this thread stores its values in shared memory (threadIdx.x)
    -> CodeGen [Operand]            -- variables storing the final reduction value
reduceWarpTree dev ty combine x0 sdata n ix tid
  = foldM reduce x0
  $ map pow2 [v, v-1 .. 0]
  where
    v = P.floor (P.logBase 2 (P.fromIntegral (CUDA.warpSize dev) :: Double))

    pow2 :: Int32 -> Int32
    pow2 x = 2 ^ x

    reduce :: [Operand] -> Int32 -> CodeGen [Operand]
    reduce xs step = do
      _then     <- newBlock ("reduceWarpTree.then" ++ show step)
      _exit     <- newBlock ("reduceWarpTree.exit" ++ show step)

      -- if ( ix + step < n )
      o         <- add int32 ix (constOp $ num int32 step)
      c         <- lt int32 o n
      top       <- cbr c _then _exit

      -- then {
      --     xs         := xs `combine` sdata [ tid + step ]
      --     sdata[tid] := xs
      -- }
      setBlock _then
      i         <- add int32 tid (constOp $ num int32 step)
      ys        <- readVolatileArray sdata i
      xs'       <- combine xs ys
      unless (step == pow2 0) $ writeVolatileArray sdata tid xs'
      bot       <- br _exit

      setBlock _exit
      zipWithM phi' ty [[(t,top), (b,bot)] | t <- xs | b <- xs' ]

