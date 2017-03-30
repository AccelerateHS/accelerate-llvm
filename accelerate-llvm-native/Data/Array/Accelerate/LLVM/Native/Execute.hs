{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeOperators            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute (

  executeAcc, executeAfun1,

) where

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Analysis.Match

import Data.Array.Accelerate.LLVM.Analysis.Match
import Data.Array.Accelerate.LLVM.Execute
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.Native.Array.Data
import Data.Array.Accelerate.LLVM.Native.Compile
import Data.Array.Accelerate.LLVM.Native.Execute.Async
import Data.Array.Accelerate.LLVM.Native.Execute.Environment
import Data.Array.Accelerate.LLVM.Native.Execute.Marshal
import Data.Array.Accelerate.LLVM.Native.Target
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

-- Use work-stealing scheduler
import Data.Range.Range                                             ( Range(..) )
import Control.Parallel.Meta                                        ( Executable(..) )
import Data.Array.Accelerate.LLVM.Native.Execute.LBS

-- library
import Data.Word                                                    ( Word8 )
import Control.Monad.State                                          ( gets )
import Control.Monad.Trans                                          ( liftIO )
import Prelude                                                      hiding ( map, sum, scanl, scanr, init )
import qualified Prelude                                            as P

import Foreign.C
import Foreign.LibFFI                                               ( Arg )
import Foreign.Ptr


-- Array expression evaluation
-- ---------------------------

-- Computations are evaluated by traversing the AST bottom up, and for each node
-- distinguishing between three cases:
--
--  1. If it is a Use node, we return a reference to the array data. Even though
--     we execute with multiple cores, we assume a shared memory multiprocessor
--     machine.
--
--  2. If it is a non-skeleton node, such as a let binding or shape conversion,
--     then execute directly by updating the environment or similar.
--
--  3. If it is a skeleton node, then we need to execute the generated LLVM
--     code.
--
instance Execute Native where
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
  stencil1      = stencil1Op
  stencil2      = stencil2Op


-- Skeleton implementation
-- -----------------------

-- Simple kernels just needs to know the shape of the output array.
--
simpleOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> LLVM Native (Array sh e)
simpleOp NativeR{..} gamma aenv () sh = do
  Native{..} <- gets llvmTarget
  liftIO $ do
    out <- allocateArray sh
    executeMain executableR $ \f ->
      executeOp defaultLargePPT fillP f gamma aenv (IE 0 (size sh)) out
    return out

simpleNamed
    :: (Shape sh, Elt e)
    => String
    -> ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> LLVM Native (Array sh e)
simpleNamed fun NativeR{..} gamma aenv () sh = do
  Native{..} <- gets llvmTarget
  liftIO $ do
    out <- allocateArray sh
    execute executableR fun $ \f ->
      executeOp defaultLargePPT fillP f gamma aenv (IE 0 (size sh)) out
    return out


-- Note: [Reductions]
--
-- There are two flavours of reduction:
--
--   1. If we are collapsing to a single value, then threads reduce strips of
--      the input in parallel, and then a single thread reduces the partial
--      reductions to a single value. Load balancing occurs over the input
--      stripes.
--
--   2. If this is a multidimensional reduction, then each inner dimension is
--      handled by a single thread. Load balancing occurs over the outer
--      dimension indices.
--
-- The entry points to executing the reduction are 'foldOp' and 'fold1Op', for
-- exclusive and inclusive reductions respectively. These functions handle
-- whether the input array is empty. If the input and output arrays are
-- non-empty, we then further dispatch (via 'foldCore') to 'foldAllOp' or
-- 'foldDimOp' for single or multidimensional reductions, respectively.
-- 'foldAllOp' in particular must execute specially whether the gang has
-- multiple worker threads which can process the array in parallel.
--

fold1Op
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM Native (Array sh e)
fold1Op kernel gamma aenv stream sh@(sx :. sz)
  = $boundsCheck "fold1" "empty array" (sz > 0)
  $ case size sh of
      0 -> liftIO $ allocateArray sx   -- empty, but possibly with non-zero dimensions
      _ -> foldCore kernel gamma aenv stream sh

foldOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM Native (Array sh e)
foldOp kernel gamma aenv stream sh@(sx :. _) =
  case size sh of
    0 -> simpleNamed "generate" kernel gamma aenv stream (listToShape (P.map (max 1) (shapeToList sx)))
    _ -> foldCore kernel gamma aenv stream sh

foldCore
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM Native (Array sh e)
foldCore kernel gamma aenv stream sh
  | Just Refl <- matchShapeType sh (undefined::DIM1)
  = foldAllOp kernel gamma aenv stream sh
  --
  | otherwise
  = foldDimOp kernel gamma aenv stream sh

foldAllOp
    :: forall aenv e. Elt e
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> DIM1
    -> LLVM Native (Scalar e)
foldAllOp NativeR{..} gamma aenv () (Z :. sz) = do
  Native{..} <- gets llvmTarget
  let
      ncpu    = gangSize
      stride  = defaultLargePPT `min` ((sz + ncpu - 1) `quot` ncpu)
      steps   = (sz + stride - 1) `quot` stride
  --
  if ncpu == 1 || sz <= defaultLargePPT
    then liftIO $ do
      -- Sequential reduction
      out <- allocateArray Z
      execute executableR "foldAllS" $ \f ->
        executeOp 1 fillS f gamma aenv (IE 0 sz) out
      return out

    else liftIO $ do
      -- Parallel reduction
      out <- allocateArray Z
      tmp <- allocateArray (Z :. steps) :: IO (Vector e)
      --
      execute  executableR "foldAllP1" $ \f1 -> do
       execute executableR "foldAllP2" $ \f2 -> do
        executeOp 1 fillP f1 gamma aenv (IE 0 steps) (sz, stride, tmp)
        executeOp 1 fillS f2 gamma aenv (IE 0 steps) (tmp, out)
      --
      return out

foldDimOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM Native (Array sh e)
foldDimOp NativeR{..} gamma aenv () (sh :. sz) = do
  Native{..} <- gets llvmTarget
  let ppt = defaultSmallPPT `max` (defaultLargePPT `quot` (max 1 sz))
  liftIO $ do
    out <- allocateArray sh
    executeMain executableR $ \f ->
      executeOp ppt fillP f gamma aenv (IE 0 (size sh)) (sz, out)
    return out

foldSegOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> (Z  :. Int)
    -> LLVM Native (Array (sh :. Int) e)
foldSegOp NativeR{..} gamma aenv () (sh :. _) (Z :. ss) = do
  Native{..} <- gets llvmTarget
  let
      ncpu               = gangSize
      kernel | ncpu == 1 = "foldSegS"
             | otherwise = "foldSegP"
      n      | ncpu == 1 = ss
             | otherwise = ss - 1   -- segments array has been 'scanl (+) 0'`ed
      ppt                = n        -- for 1D distribute evenly over threads; otherwise
  --                                -- compute all segments on an innermost dimension
  liftIO $ do
    out <- allocateArray (sh :. n)
    execute executableR kernel $ \f ->
      executeOp ppt fillP f gamma aenv (IE 0 (size (sh :. n))) out
    return out


scanOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh :. Int
    -> LLVM Native (Array (sh:.Int) e)
scanOp kernel gamma aenv stream (sz :. n) =
  case n of
    0 -> simpleNamed "generate" kernel gamma aenv stream (sz :. 1)
    _ -> scanCore kernel gamma aenv stream sz n (n+1)

scan1Op
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh :. Int
    -> LLVM Native (Array (sh:.Int) e)
scan1Op kernel gamma aenv stream (sz :. n)
  = $boundsCheck "scan1" "empty array" (n > 0)
  $ scanCore kernel gamma aenv stream sz n n

scanCore
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> Int
    -> Int
    -> LLVM Native (Array (sh:.Int) e)
scanCore NativeR{..} gamma aenv () sz n m = do
  Native{..} <- gets llvmTarget
  let
      ncpu    = gangSize
      stride  = defaultLargePPT `min` ((n + ncpu - 1) `quot` ncpu)
      steps   = (n + stride - 1) `quot` stride
      steps'  = steps - 1
  --
  if ncpu == 1 || rank sz > 0 || n <= 2 * defaultLargePPT
    then liftIO $ do
      -- Either:
      --
      --  1. Sequential scan of an array of any rank
      --
      --  2. Parallel scan of multidimensional array: threads scan along the
      --     length of the innermost dimension. Threads are scheduled over the
      --     inner dimensions.
      --
      --  3. Small 1D array. Since parallel scan requires ~4n data transfer
      --     compared to ~2n in the sequential case, it is only worthwhile if
      --     the extra cores can offset the increased bandwidth requirements.
      --
      out <- allocateArray (sz :. m)
      execute executableR "scanS" $ \f ->
        executeOp 1 fillP f gamma aenv (IE 0 (size sz)) out
      return out

    else liftIO $ do
      -- parallel one-dimensional scan
      out <- allocateArray (sz :. m)
      tmp <- allocateArray (Z  :. steps) :: IO (Vector e)
      --
      execute   executableR "scanP1" $ \f1 -> do
       execute  executableR "scanP2" $ \f2 -> do
        execute executableR "scanP3" $ \f3 -> do
          executeOp 1 fillP f1 gamma aenv (IE 0 steps) (stride, steps', out, tmp)
          executeOp 1 fillS f2 gamma aenv (IE 0 steps) tmp
          executeOp 1 fillP f3 gamma aenv (IE 0 steps') (stride, out, tmp)
      --
      return out


scan'Op
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh :. Int
    -> LLVM Native (Array (sh:.Int) e, Array sh e)
scan'Op native gamma aenv stream sh@(sz :. n) =
  case n of
    0 -> do
      out <- liftIO $ allocateArray (sz :. 0)
      sum <- simpleNamed "generate" native gamma aenv stream sz
      return (out, sum)
    --
    _ -> scan'Core native gamma aenv stream sh

scan'Core
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh :. Int
    -> LLVM Native (Array (sh:.Int) e, Array sh e)
scan'Core NativeR{..} gamma aenv () sh@(sz :. n) = do
  Native{..} <- gets llvmTarget
  let
      ncpu    = gangSize
      stride  = defaultLargePPT `min` ((n + ncpu - 1) `quot` ncpu)
      steps   = (n + stride - 1) `quot` stride
      steps'  = steps - 1
  --
  if ncpu == 1 || rank sz > 0 || n <= 2 * defaultLargePPT
    then liftIO $ do
      out <- allocateArray sh
      sum <- allocateArray sz
      execute executableR "scanS" $ \f ->
        executeOp 1 fillP f gamma aenv (IE 0 (size sz)) (out,sum)
      return (out,sum)

    else liftIO $ do
      tmp <- allocateArray (Z :. steps) :: IO (Vector e)
      out <- allocateArray sh
      sum <- allocateArray sz

      execute   executableR "scanP1" $ \f1 -> do
       execute  executableR "scanP2" $ \f2 -> do
        execute executableR "scanP3" $ \f3 -> do
          executeOp 1 fillP f1 gamma aenv (IE 0 steps)  (stride, steps', out, tmp)
          executeOp 1 fillS f2 gamma aenv (IE 0 steps)  (sum, tmp)
          executeOp 1 fillP f3 gamma aenv (IE 0 steps') (stride, out, tmp)

      return (out,sum)


-- Forward permutation, specified by an indexing mapping into an array and a
-- combination function to combine elements.
--
permuteOp
    :: (Shape sh, Shape sh', Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Bool
    -> sh
    -> Array sh' e
    -> LLVM Native (Array sh' e)
permuteOp NativeR{..} gamma aenv () inplace shIn dfs = do
  Native{..} <- gets llvmTarget
  out        <- if inplace
                  then return dfs
                  else cloneArray dfs
  let
      ncpu    = gangSize
      n       = size shIn
      m       = size (shape out)
  --
  if ncpu == 1 || n <= defaultLargePPT
    then liftIO $ do
      -- sequential permutation
      execute executableR "permuteS" $ \f ->
        executeOp 1 fillS f gamma aenv (IE 0 n) out

    else liftIO $ do
      -- parallel permutation
      symbols <- nm executableR
      if "permuteP_rmw" `elem` symbols
        then do
          execute executableR "permuteP_rmw" $ \f ->
            executeOp defaultLargePPT fillP f gamma aenv (IE 0 n) out

        else do
          barrier@(Array _ adb) <- allocateArray (Z :. m) :: IO (Vector Word8)
          memset (ptrsOfArrayData adb) 0 m
          execute executableR "permuteP_mutex" $ \f ->
            executeOp defaultLargePPT fillP f gamma aenv (IE 0 n) (out, barrier)

  return out


stencil1Op
    :: (Shape sh, Elt b)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Array sh a
    -> LLVM Native (Array sh b)
stencil1Op kernel gamma aenv stream arr =
  simpleOp kernel gamma aenv stream (shape arr)

stencil2Op
    :: (Shape sh, Elt c)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Array sh a
    -> Array sh b
    -> LLVM Native (Array sh c)
stencil2Op kernel gamma aenv stream arr brr =
  simpleOp kernel gamma aenv stream (shape arr `intersect` shape brr)


-- Skeleton execution
-- ------------------

-- Execute the given function distributed over the available threads.
--
executeOp
    :: Marshalable args
    => Int
    -> Executable
    -> (String, [Arg] -> IO ())
    -> Gamma aenv
    -> Aval aenv
    -> Range
    -> args
    -> IO ()
executeOp ppt exe (name, f) gamma aenv r args =
  runExecutable exe name ppt r $ \start end _tid ->
  monitorProcTime              $
    f =<< marshal (undefined::Native) () (start, end, args, (gamma, aenv))


-- Standard C functions
-- --------------------

memset :: Ptr Word8 -> Word8 -> Int -> IO ()
memset p w s = c_memset p (fromIntegral w) (fromIntegral s) >> return ()

foreign import ccall unsafe "string.h memset" c_memset
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)


-- Debugging
-- ---------

monitorProcTime :: IO a -> IO a
monitorProcTime = Debug.withProcessor Debug.Native

