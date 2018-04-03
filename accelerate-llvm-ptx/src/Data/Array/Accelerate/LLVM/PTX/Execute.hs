{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute (

  executeAcc, executeAfun,
  executeOpenAcc,

) where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.Analysis.Match
import Data.Array.Accelerate.LLVM.Execute
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch           ( multipleOf )
import Data.Array.Accelerate.LLVM.PTX.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Array.Prim                ( memsetArrayAsync )
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import Data.Array.Accelerate.LLVM.PTX.Execute.Environment
import Data.Array.Accelerate.LLVM.PTX.Execute.Marshal
import Data.Array.Accelerate.LLVM.PTX.Link
import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

import Data.Range                                               ( Range(..) )
import Control.Parallel.Meta                                    ( runExecutable )

-- cuda
import qualified Foreign.CUDA.Driver                            as CUDA

-- library
import Control.Monad                                            ( when )
import Control.Monad.State                                      ( gets, liftIO )
import Data.ByteString.Short.Char8                              ( ShortByteString, unpack )
import Data.List                                                ( find )
import Data.Maybe                                               ( fromMaybe )
import Data.Word                                                ( Word32 )
import Text.Printf                                              ( printf )
import Prelude                                                  hiding ( exp, map, sum, scanl, scanr )
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
  foldSeg       = foldSegOp
  fold1Seg      = foldSegOp
  scanl         = scanOp
  scanl1        = scan1Op
  scanl'        = scan'Op
  scanr         = scanOp
  scanr1        = scan1Op
  scanr'        = scan'Op
  permute       = permuteOp
  stencil1      = simpleOp
  stencil2      = stencil2Op
  aforeign      = aforeignOp


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
simpleOp exe gamma aenv stream sh = withExecutable exe $ \ptxExecutable -> do
  let kernel  = case functionTable ptxExecutable of
                  k:_ -> k
                  _   -> $internalError "simpleOp" "no kernels found"
  --
  out <- allocateRemote sh
  ptx <- gets llvmTarget
  liftIO $ executeOp ptx kernel gamma aenv stream (IE 0 (size sh)) out
  return out

simpleNamed
    :: (Shape sh, Elt e)
    => ShortByteString
    -> ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> LLVM PTX (Array sh e)
simpleNamed fun exe gamma aenv stream sh = withExecutable exe $ \ptxExecutable -> do
  out <- allocateRemote sh
  ptx <- gets llvmTarget
  liftIO $ executeOp ptx (ptxExecutable !# fun) gamma aenv stream (IE 0 (size sh)) out
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
fold1Op exe gamma aenv stream sh@(sx :. sz)
  = $boundsCheck "fold1" "empty array" (sz > 0)
  $ case size sh of
      0 -> allocateRemote sx    -- empty, but possibly with one or more non-zero dimensions
      _ -> foldCore exe gamma aenv stream sh

foldOp
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM PTX (Array sh e)
foldOp exe gamma aenv stream sh@(sx :. _)
  = case size sh of
      0 -> simpleNamed "generate" exe gamma aenv stream sx
      _ -> foldCore exe gamma aenv stream sh

foldCore
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM PTX (Array sh e)
foldCore exe gamma aenv stream sh
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = foldAllOp exe gamma aenv stream sh
  --
  | otherwise
  = foldDimOp exe gamma aenv stream sh


foldAllOp
    :: forall aenv e. Elt e
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> DIM1
    -> LLVM PTX (Scalar e)
foldAllOp exe gamma aenv stream (Z :. n) = withExecutable exe $ \ptxExecutable -> do
  ptx <- gets llvmTarget
  let
      ks      = ptxExecutable !# "foldAllS"
      km1     = ptxExecutable !# "foldAllM1"
      km2     = ptxExecutable !# "foldAllM2"
  --
  if kernelThreadBlocks ks n == 1
    then do
      -- The array is small enough that we can compute it in a single step
      out   <- allocateRemote Z
      liftIO $ executeOp ptx ks gamma aenv stream (IE 0 n) out
      return out

    else do
      -- Multi-kernel reduction to a single element. The first kernel integrates
      -- any delayed elements, and the second is called recursively until
      -- reaching a single element.
      let
          rec :: Vector e -> LLVM PTX (Scalar e)
          rec tmp@(Array ((),m) adata)
            | m <= 1    = return $ Array () adata
            | otherwise = do
                let s = m `multipleOf` kernelThreadBlockSize km2
                out   <- allocateRemote (Z :. s)
                liftIO $ executeOp ptx km2 gamma aenv stream (IE 0 s) (tmp, out)
                rec out
      --
      let s = n `multipleOf` kernelThreadBlockSize km1
      tmp   <- allocateRemote (Z :. s)
      liftIO $ executeOp ptx km1 gamma aenv stream (IE 0 s) tmp
      rec tmp


foldDimOp
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM PTX (Array sh e)
foldDimOp exe gamma aenv stream (sh :. sz) = withExecutable exe $ \ptxExecutable -> do
  let
      kernel  = if sz > 0
                  then ptxExecutable !# "fold"
                  else ptxExecutable !# "generate"
  --
  out <- allocateRemote sh
  ptx <- gets llvmTarget
  liftIO $ executeOp ptx kernel gamma aenv stream (IE 0 (size sh)) out
  return out


foldSegOp
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> (Z  :. Int)
    -> LLVM PTX (Array (sh :. Int) e)
foldSegOp exe gamma aenv stream (sh :. sz) (Z :. ss) = withExecutable exe $ \ptxExecutable -> do
  let
      n       = ss - 1  -- segments array has been 'scanl (+) 0'`ed
      m       = size sh * n
      foldseg = if (sz`quot`ss) < (2 * kernelThreadBlockSize foldseg_cta)
                  then foldseg_warp
                  else foldseg_cta
      --
      foldseg_cta   = ptxExecutable !# "foldSeg_block"
      foldseg_warp  = ptxExecutable !# "foldSeg_warp"
      -- qinit         = ptxExecutable !# "qinit"
  --
  out <- allocateRemote (sh :. n)
  ptx <- gets llvmTarget
  liftIO $ executeOp ptx foldseg gamma aenv stream (IE 0 m) out
  return out


scanOp
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh :. Int
    -> LLVM PTX (Array (sh:.Int) e)
scanOp exe gamma aenv stream (sz :. n) =
  case n of
    0 -> simpleNamed "generate" exe gamma aenv stream (sz :. 1)
    _ -> scanCore exe gamma aenv stream sz n (n+1)

scan1Op
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh :. Int
    -> LLVM PTX (Array (sh:.Int) e)
scan1Op exe gamma aenv stream (sz :. n)
  = $boundsCheck "scan1" "empty array" (n > 0)
  $ scanCore exe gamma aenv stream sz n n

scanCore
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> Int                    -- input size
    -> Int                    -- output size
    -> LLVM PTX (Array (sh:.Int) e)
scanCore exe gamma aenv stream sz n m
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = scanAllOp exe gamma aenv stream n m
  --
  | otherwise
  = scanDimOp exe gamma aenv stream sz m


scanAllOp
    :: forall aenv e. Elt e
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Int                    -- input size
    -> Int                    -- output size
    -> LLVM PTX (Vector e)
scanAllOp exe gamma aenv stream n m = withExecutable exe $ \ptxExecutable -> do
  let
      k1  = ptxExecutable !# "scanP1"
      k2  = ptxExecutable !# "scanP2"
      k3  = ptxExecutable !# "scanP3"
      --
      c   = kernelThreadBlockSize k1
      s   = n `multipleOf` c
  --
  ptx <- gets llvmTarget
  out <- allocateRemote (Z :. m)

  -- Step 1: Independent thread-block-wide scans of the input. Small arrays
  -- which can be computed by a single thread block will require no
  -- additional work.
  tmp <- allocateRemote (Z :. s) :: LLVM PTX (Vector e)
  liftIO $ executeOp ptx k1 gamma aenv stream (IE 0 s) (tmp, out)

  -- Step 2: Multi-block reductions need to compute the per-block prefix,
  -- then apply those values to the partial results.
  when (s > 1) $ do
    liftIO $ executeOp ptx k2 gamma aenv stream (IE 0 s)     tmp
    liftIO $ executeOp ptx k3 gamma aenv stream (IE 0 (s-1)) (tmp, out, c)

  return out


scanDimOp
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> Int
    -> LLVM PTX (Array (sh:.Int) e)
scanDimOp exe gamma aenv stream sz m = withExecutable exe $ \ptxExecutable -> do
  ptx <- gets llvmTarget
  out <- allocateRemote (sz :. m)
  liftIO $ executeOp ptx (ptxExecutable !# "scan") gamma aenv stream (IE 0 (size sz)) out
  return out


scan'Op
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh :. Int
    -> LLVM PTX (Array (sh:.Int) e, Array sh e)
scan'Op exe gamma aenv stream sh@(sz :. n) =
  case n of
    0 -> do out <- allocateRemote (sz :. 0)
            sum <- simpleNamed "generate" exe gamma aenv stream sz
            return (out, sum)
    _ -> scan'Core exe gamma aenv stream sh

scan'Core
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh :. Int
    -> LLVM PTX (Array (sh:.Int) e, Array sh e)
scan'Core exe gamma aenv stream sh
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = scan'AllOp exe gamma aenv stream sh
  --
  | otherwise
  = scan'DimOp exe gamma aenv stream sh

scan'AllOp
    :: forall aenv e. Elt e
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> DIM1
    -> LLVM PTX (Vector e, Scalar e)
scan'AllOp exe gamma aenv stream (Z :. n) = withExecutable exe $ \ptxExecutable -> do
  let
      k1  = ptxExecutable !# "scanP1"
      k2  = ptxExecutable !# "scanP2"
      k3  = ptxExecutable !# "scanP3"
      --
      c   = kernelThreadBlockSize k1
      s   = n `multipleOf` c
  --
  ptx <- gets llvmTarget
  out <- allocateRemote (Z :. n)
  tmp <- allocateRemote (Z :. s)  :: LLVM PTX (Vector e)

  -- Step 1: independent thread-block-wide scans. Each block stores its partial
  -- sum to a temporary array.
  liftIO $ executeOp ptx k1 gamma aenv stream (IE 0 s) (tmp, out)

  -- If this was a small array that was processed by a single thread block then
  -- we are done, otherwise compute the per-block prefix and apply those values
  -- to the partial results.
  if s == 1
    then case tmp of
           Array _ ad -> return (out, Array () ad)
    else do
      sum <- allocateRemote Z
      liftIO $ executeOp ptx k2 gamma aenv stream (IE 0 s)     (tmp, sum)
      liftIO $ executeOp ptx k3 gamma aenv stream (IE 0 (s-1)) (tmp, out, c)
      return (out, sum)


scan'DimOp
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh :. Int
    -> LLVM PTX (Array (sh:.Int) e, Array sh e)
scan'DimOp exe gamma aenv stream sh@(sz :. _) = withExecutable exe $ \ptxExecutable -> do
  ptx <- gets llvmTarget
  out <- allocateRemote sh
  sum <- allocateRemote sz
  liftIO $ executeOp ptx (ptxExecutable !# "scan") gamma aenv stream (IE 0 (size sz)) (out,sum)
  return (out,sum)


permuteOp
    :: (Shape sh, Shape sh', Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Bool
    -> sh
    -> Array sh' e
    -> LLVM PTX (Array sh' e)
permuteOp exe gamma aenv stream inplace shIn dfs = withExecutable exe $ \ptxExecutable -> do
  let
      n       = size shIn
      m       = size (shape dfs)
      kernel  = case functionTable ptxExecutable of
                  k:_ -> k
                  _   -> $internalError "permute" "no kernels found"
  --
  ptx <- gets llvmTarget
  out <- if inplace
           then return dfs
           else cloneArrayAsync stream dfs
  --
  case kernelName kernel of
    "permute_rmw"   -> liftIO $ executeOp ptx kernel gamma aenv stream (IE 0 n) out
    "permute_mutex" -> do
      barrier@(Array _ ad) <- allocateRemote (Z :. m) :: LLVM PTX (Vector Word32)
      memsetArrayAsync stream m 0 ad
      liftIO $ executeOp ptx kernel gamma aenv stream (IE 0 n) (out, barrier)
    _               -> $internalError "permute" "unexpected kernel image"
  --
  return out


-- Using the defaulting instances for stencil operations (for now).
--
stencil2Op
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> sh
    -> LLVM PTX (Array sh e)
stencil2Op exe gamma aenv stream sh1 sh2 =
  simpleOp exe gamma aenv stream (sh1 `intersect` sh2)


-- Foreign functions
--
aforeignOp
    :: (Arrays as, Arrays bs)
    => String
    -> (Stream -> as -> LLVM PTX bs)
    -> Stream
    -> as
    -> LLVM PTX bs
aforeignOp name asm stream arr =
  Debug.monitorProcTime query msg (Just (unsafeGetValue stream)) $
    asm stream arr
  where
    query = if Debug.monitoringIsEnabled
              then return True
              else liftIO $ Debug.getFlag Debug.dump_exec

    msg wall cpu gpu = do
      Debug.addProcessorTime Debug.PTX gpu
      Debug.traceIO Debug.dump_exec $
        printf "exec: %s %s" name (Debug.elapsed wall cpu gpu)


-- Skeleton execution
-- ------------------

-- TODO: Calculate this from the device properties, say [a multiple of] the
--       maximum number of in-flight threads that the device supports.
--
defaultPPT :: Int
defaultPPT = 32768

-- | Retrieve the named kernel
--
(!#) :: FunctionTable -> ShortByteString -> Kernel
(!#) exe name
  = fromMaybe ($internalError "lookupFunction" ("function not found: " ++ unpack name))
  $ lookupKernel name exe

lookupKernel :: ShortByteString -> FunctionTable -> Maybe Kernel
lookupKernel name ptxExecutable =
  find (\k -> kernelName k == name) (functionTable ptxExecutable)


-- Execute the function implementing this kernel.
--
executeOp
    :: Marshalable args
    => PTX
    -> Kernel
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Range
    -> args
    -> IO ()
executeOp ptx@PTX{..} kernel@Kernel{..} gamma aenv stream r args =
  runExecutable fillP kernelName defaultPPT r $ \start end _ -> do
    argv <- marshal ptx stream (start, end, args, (gamma,aenv))
    launch kernel stream (end-start) argv


-- Execute a device function with the given thread configuration and function
-- parameters.
--
launch :: Kernel -> Stream -> Int -> [CUDA.FunParam] -> IO ()
launch Kernel{..} stream n args =
  when (n > 0) $
  withLifetime stream $ \st ->
    Debug.monitorProcTime query msg (Just st) $
      CUDA.launchKernel kernelFun grid cta smem (Just st) args
  where
    cta   = (kernelThreadBlockSize, 1, 1)
    grid  = (kernelThreadBlocks n, 1, 1)
    smem  = kernelSharedMemBytes

    -- Debugging/monitoring support
    query = if Debug.monitoringIsEnabled
              then return True
              else Debug.getFlag Debug.dump_exec

    fst3 (x,_,_)      = x
    msg wall cpu gpu  = do
      Debug.addProcessorTime Debug.PTX gpu
      Debug.traceIO Debug.dump_exec $
        printf "exec: %s <<< %d, %d, %d >>> %s"
               (unpack kernelName) (fst3 grid) (fst3 cta) smem (Debug.elapsed wall cpu gpu)

