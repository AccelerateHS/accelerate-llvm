{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute (

  executeAcc, executeAfun1,

  executeOp,

) where

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Array.Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Execute

import Data.Array.Accelerate.LLVM.PTX.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import Data.Array.Accelerate.LLVM.PTX.Execute.Environment
import Data.Array.Accelerate.LLVM.PTX.Execute.Marshal
import Data.Array.Accelerate.LLVM.PTX.Target

import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

import Data.Range.Range                                         ( Range(..) )
import Control.Parallel.Meta                                    ( runExecutable, Finalise )

-- cuda
import qualified Foreign.CUDA.Driver                            as CUDA

-- library
import Prelude                                                  hiding ( exp, map, scanl, scanr )
import Data.Int                                                 ( Int32 )
import Data.Monoid                                              ( mempty )
import Control.Monad.State                                      ( gets, liftIO )
import Text.Printf
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
fold1Op kernel gamma aenv stream sh@(_ :. sz)
  = $boundsCheck "fold1" "empty array" (sz > 0)
  $ foldCore kernel gamma aenv stream sh

foldOp
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM PTX (Array sh e)
foldOp kernel gamma aenv stream (sh :. sz)
  = foldCore kernel gamma aenv stream ((listToShape . P.map (max 1) . shapeToList $ sh) :. sz)

foldCore
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM PTX (Array sh e)
foldCore kernel gamma aenv stream sh'@(sh :. _)
  | dim sh > 0      = simpleOp  kernel gamma aenv stream sh
  | otherwise       = foldAllOp kernel gamma aenv stream sh'

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
            numBlocks         = (kernelThreadBlocks k2) numElements
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
executeOp ptx@PTX{..} kernel after gamma aenv stream r args = do
  runExecutable fillP defaultPPT r after $ \start end _ ->
    launch kernel stream (end-start) =<< marshal ptx stream (i32 start, i32 end, args, (gamma,aenv))


-- Execute a device function with the given thread configuration and function
-- parameters.
--
launch :: Kernel -> Stream -> Int -> [CUDA.FunParam] -> IO ()
launch Kernel{..} stream n args
  = Debug.timed Debug.dump_exec msg (Just stream)
  $ CUDA.launchKernel kernelFun grid cta smem (Just stream) args
  where
    cta         = (kernelThreadBlockSize, 1, 1)
    grid        = (kernelThreadBlocks n, 1, 1)
    smem        = kernelSharedMemBytes

    fst3 (x,_,_)         = x
    msg gpuTime wallTime =
      printf "exec: %s <<< %d, %d, %d >>> %s"
             kernelName (fst3 grid) (fst3 cta) smem (Debug.elapsed gpuTime wallTime)

