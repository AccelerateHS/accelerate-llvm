{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute
-- Copyright   : [2014..2016] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute (

  executeAcc, executeAfun1,

) where

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Array.Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Execute

import Data.Array.Accelerate.LLVM.PTX.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import Data.Array.Accelerate.LLVM.PTX.Execute.Environment
import Data.Array.Accelerate.LLVM.PTX.Execute.Marshal
import Data.Array.Accelerate.LLVM.PTX.Compile
import Data.Array.Accelerate.LLVM.PTX.Target

import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

import Data.Range.Range                                         ( Range(..) )
import Control.Parallel.Meta                                    ( runExecutable, Finalise )

-- cuda
import qualified Foreign.CUDA.Driver                            as CUDA

-- library
import Control.Monad.State                                      ( gets, liftIO )
import Data.Int                                                 ( Int32 )
import Data.List                                                ( find )
import Data.Monoid                                              ( mempty )
import Text.Printf                                              ( printf )
import Prelude                                                  hiding ( exp, map, scanl, scanr )
import qualified Prelude                                        as P


-- Array expression evaluation
-- ---------------------------

-- Computations are evaluated by traversing the AST bottom up, and for each node
-- distinguishing between three cases:
--
--  1. If it is a Use node, we return a reference to the array data. The data
--     will already have been copied to the device during compilation of the
--     kernels.
--
--  2. If it is a non-skeleton node, such as a let binding or shape conversion,
--     then execute directly by updating the environment or similar.
--
--  3. If it is a skeleton node, then we need to execute the generated LLVM
--     code.
--
instance Execute PTX where
  map           = simpleOp
  generate      = simpleOp
  transform     = simpleOp
  backpermute   = simpleOp
  fold          = foldOp
  fold1         = fold1Op
  scanl         = scanOp
  stencil1      = stencil1Op
  stencil2      = stencil2Op


-- Skeleton implementation
-- -----------------------

-- Simple kernels just need to know the shape of the output array
--
simpleOp
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> LLVM PTX (Array sh e)
simpleOp exe gamma aenv stream sh = do
  let kernel    = case ptxKernel exe of
                    k:_ -> k
                    _   -> $internalError "simpleOp" "kernel not found"
  --
  out <- allocateRemote sh
  ptx <- gets llvmTarget
  liftIO $ executeOp ptx kernel mempty gamma aenv stream (IE 0 (size sh)) out
  return out


-- There are two flavours of fold operation:
--
--   1. If we are collapsing to a single value, then multiple thread blocks are
--      working together. Since thread blocks synchronise with each other via
--      kernel launches, each block computes a partial sum and the kernel is
--      launched recursively until the final value is reached.
--
--   2. If this is a multidimensional reduction, then each inner dimension is
--      handled by a single thread block, so no global communication is
--      necessary. Furthermore are two kernel flavours: each innermost dimension
--      can be cooperatively reduced by (a) a thread warp; or (b) a thread
--      block. Currently we always use the first, but require benchmarking to
--      determine when to select each.
--
fold1Op
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM PTX (Array sh e)
fold1Op kernel gamma aenv stream sh@(sx :. sz)
  = $boundsCheck "fold1" "empty array" (sz > 0)
  $ case size sh of
      0 -> allocateRemote sx
      _ -> foldCore kernel gamma aenv stream sh

foldOp
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM PTX (Array sh e)
foldOp kernel gamma aenv stream sh@(sx :. _)
  = foldCore kernel gamma aenv stream
  $ case size sh of
      0 -> listToShape (P.map (max 1) (shapeToList sx)) :. 0
      _ -> sh

foldCore
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM PTX (Array sh e)
foldCore kernel gamma aenv stream sh
  | rank sh == 1  = foldAllOp kernel gamma aenv stream sh
  | otherwise     = foldDimOp kernel gamma aenv stream sh

-- See note: [Marshalling foldAll output arrays]
--
foldAllOp
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM PTX (Array sh e)
foldAllOp exe gamma aenv stream sh' = do
  ptx <- gets llvmTarget
  let
      (k1,k2)   = case ptxKernel exe of
                    u:v:_       -> (u,v)
                    _           -> $internalError "foldAllOp" "kernel not found"

      foldIntro :: (sh :. Int) -> LLVM PTX (Array sh e)
      foldIntro (sh:.sz) = do
        let numElements       = size sh * sz
            numBlocks         = (kernelThreadBlocks k1) numElements
        --
        out <- allocateRemote (sh :. numBlocks)
        liftIO $ executeOp ptx k1 mempty gamma aenv stream (IE 0 numElements) out
        foldRec out

      foldRec :: Array (sh :. Int) e -> LLVM PTX (Array sh e)
      foldRec out@(Array (sh,sz) adata) =
        let numElements       = R.size sh * sz
            numBlocks         = (kernelThreadBlocks k1) numElements
        in if sz <= 1
              then do
                -- We have recursed to a single block already. Trim the
                -- intermediate working vector to the final scalar array.
                return $! Array sh adata

              else do
                -- Keep cooperatively reducing the output array in-place.
                -- Note that we must continue to update the tracked size
                -- so the recursion knows when to stop.
                liftIO $ executeOp ptx k2 mempty gamma aenv stream (IE 0 numElements) out
                foldRec $! Array (sh,numBlocks) adata
  --
  foldIntro sh'


foldDimOp
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM PTX (Array sh e)
foldDimOp exe gamma aenv stream (sh :. sz) = do
  let
      kernel
        | sz > 0    = lookupKernel "fold"     exe
        | otherwise = lookupKernel "generate" exe
  --
  out <- allocateRemote sh
  ptx <- gets llvmTarget
  liftIO $ executeOp ptx kernel mempty gamma aenv stream (IE 0 (size sh)) out
  return out


scanOp
    :: Elt e
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> DIM1
    -> LLVM PTX (Vector e)
scanOp kernel gamma aenv stream (Z :. n) =
  scanCore kernel gamma aenv stream n

scanCore
    :: forall aenv e. Elt e
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Int
    -> LLVM PTX (Vector e)
scanCore exe gamma aenv stream n = do
  ptx <- gets llvmTarget
  let
    k1 = lookupKernel "scanP1" exe
    -- k2 = lookupKernel "scanP2" exe
    -- k3 = lookupKernel "scanP3" exe
  --
  out <- allocateRemote (Z :. n)
  -- tmp <- allocateRemote (sh :. numBlocks)
  liftIO $ do
    executeOp ptx k1 mempty gamma aenv stream (IE 0 n) out
    -- executeOp ptx k2 mempty gamma aenv stream (IE 0 numElements) (tmp, out)
    -- executeOp ptx k1 mempty gamma aenv stream (IE 0 numElements) tmp
    -- executeOp ptx k3 mempty gamma aenv stream (IE 0 numElements) (tmp, out)
  return out


-- Using the defaulting instances for stencil operations (for now).
--
stencil1Op
    :: (Shape sh, Elt a, Elt b)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Array sh a
    -> LLVM PTX (Array sh b)
stencil1Op kernel gamma aenv stream arr
  = simpleOp kernel gamma aenv stream (shape arr)

stencil2Op
    :: (Shape sh, Elt a, Elt b, Elt c)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Array sh a
    -> Array sh b
    -> LLVM PTX (Array sh c)
stencil2Op kernel gamma aenv stream arr brr
  = simpleOp kernel gamma aenv stream (shape arr `intersect` shape brr)


-- Skeleton execution
-- ------------------

-- TODO: Calculate this from the device properties, say [a multiple of] the
--       maximum number of in-flight threads that the device supports.
--
defaultPPT :: Int
defaultPPT = 32768

{-# INLINE i32 #-}
i32 :: Int -> Int32
i32 = fromIntegral


-- | Retrieve the named kernel
--
lookupKernel :: String -> ExecutableR PTX -> Kernel
lookupKernel name exe =
  case find (\k -> kernelName k == name) (ptxKernel exe) of
    Just k  -> k
    Nothing -> $internalError "lookupKernel" ("not found: " ++ name)


-- Execute the function implementing this kernel.
--
executeOp
    :: Marshalable args
    => PTX
    -> Kernel
    -> Finalise
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Range
    -> args
    -> IO ()
executeOp ptx@PTX{..} kernel finish gamma aenv stream r args =
  runExecutable fillP defaultPPT r finish $ \start end _ -> do
    argv <- marshal ptx stream (i32 start, i32 end, args, (gamma,aenv))
    launch kernel stream (end-start) argv


-- Execute a device function with the given thread configuration and function
-- parameters.
--
launch :: Kernel -> Stream -> Int -> [CUDA.FunParam] -> IO ()
launch Kernel{..} stream n args =
  withLifetime stream $ \st ->
    Debug.monitorProcTime query msg (Just st) $
      CUDA.launchKernel kernelFun grid cta smem (Just st) args
  where
    cta         = (kernelThreadBlockSize, 1, 1)
    grid        = (kernelThreadBlocks n, 1, 1)
    smem        = kernelSharedMemBytes

    -- Debugging/monitoring support
    query       = if Debug.monitoringIsEnabled
                    then return True
                    else Debug.queryFlag Debug.dump_exec

    fst3 (x,_,_)         = x
    msg gpuTime wallTime = do
      Debug.addProcessorTime Debug.PTX gpuTime
      Debug.traceIO Debug.dump_exec $
        printf "exec: %s <<< %d, %d, %d >>> %s"
               kernelName (fst3 grid) (fst3 cta) smem (Debug.elapsed gpuTime wallTime)

