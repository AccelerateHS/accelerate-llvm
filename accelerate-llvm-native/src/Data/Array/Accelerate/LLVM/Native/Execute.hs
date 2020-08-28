{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute (

  executeAcc,
  executeOpenAcc

) where

import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Execute
import Data.Array.Accelerate.LLVM.Execute.Async (FutureArraysR)
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.Native.Array.Data
import Data.Array.Accelerate.LLVM.Native.Execute.Async
import Data.Array.Accelerate.LLVM.Native.Execute.Divide
import Data.Array.Accelerate.LLVM.Native.Execute.Environment        ( Val )
import Data.Array.Accelerate.LLVM.Native.Execute.Marshal
import Data.Array.Accelerate.LLVM.Native.Execute.Scheduler
import Data.Array.Accelerate.LLVM.Native.Link
import Data.Array.Accelerate.LLVM.Native.Target
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

import Control.Concurrent                                           ( myThreadId )
import Control.Monad.State                                          ( gets )
import Control.Monad.Trans                                          ( liftIO )
import Data.ByteString.Short                                        ( ShortByteString )
import Data.IORef                                                   ( newIORef, readIORef, writeIORef )
import Data.List                                                    ( find )
import Data.Maybe                                                   ( fromMaybe )
import Data.Sequence                                                ( Seq )
import Data.Foldable                                                ( asum )
import System.CPUTime                                               ( getCPUTime )
import Text.Printf                                                  ( printf )
import qualified Data.ByteString.Short.Char8                        as S8
import qualified Data.Sequence                                      as Seq
import qualified Data.DList                                         as DL
import Prelude                                                      hiding ( map, sum, scanl, scanr, init )

import Foreign.LibFFI
import Foreign.Ptr

{-# SPECIALISE INLINE executeAcc     :: ExecAcc     Native      a ->             Par Native (FutureArraysR Native a) #-}
{-# SPECIALISE INLINE executeOpenAcc :: ExecOpenAcc Native aenv a -> Val aenv -> Par Native (FutureArraysR Native a) #-}

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
  {-# INLINE map         #-}
  {-# INLINE generate    #-}
  {-# INLINE transform   #-}
  {-# INLINE backpermute #-}
  {-# INLINE fold        #-}
  {-# INLINE foldSeg     #-}
  {-# INLINE scan        #-}
  {-# INLINE scan'       #-}
  {-# INLINE permute     #-}
  {-# INLINE stencil1    #-}
  {-# INLINE stencil2    #-}
  {-# INLINE aforeign    #-}
  map           = mapOp
  generate      = generateOp
  transform     = transformOp
  backpermute   = backpermuteOp
  fold True     = foldOp
  fold False    = fold1Op
  foldSeg i _   = foldSegOp i
  scan _ True   = scanOp
  scan _ False  = scan1Op
  scan' _       = scan'Op
  permute       = permuteOp
  stencil1      = stencil1Op
  stencil2      = stencil2Op
  aforeign      = aforeignOp


-- Skeleton implementation
-- -----------------------

-- Simple kernels just needs to know the shape of the output array.
--
{-# INLINE simpleOp #-}
simpleOp
    :: HasCallStack
    => ShortByteString
    -> ArrayR (Array sh e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Par Native (Future (Array sh e))
simpleOp name repr NativeR{..} gamma aenv sh = do
  let fun   = nativeExecutable !# name
      param = TupRsingle $ ParamRarray repr
  Native{..} <- gets llvmTarget
  future     <- new
  result     <- allocateRemote repr sh
  scheduleOp fun gamma aenv (arrayRshape repr) sh param result
    `andThen` do putIO workers future result
                 touchLifetime nativeExecutable   -- XXX: must not unload the object code early
  return future

-- Mapping over an array can ignore the dimensionality of the array and
-- treat it as its underlying linear representation.
--
{-# INLINE mapOp #-}
mapOp
    :: HasCallStack
    => Maybe (a :~: b)
    -> ArrayR (Array sh a)
    -> TypeR b
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Array sh a
    -> Par Native (Future (Array sh b))
mapOp inplace repr tp NativeR{..} gamma aenv input = do
  let fun   = nativeExecutable !# "map"
      sh    = shape input
      shr   = arrayRshape repr
      repr' = ArrayR shr tp
      param = TupRsingle (ParamRarray repr') `TupRpair` TupRsingle (ParamRarray repr)
  Native{..} <- gets llvmTarget
  future     <- new
  result     <- case inplace of
                  Just Refl -> return input
                  Nothing   -> allocateRemote repr' sh
  scheduleOp fun gamma aenv dim1 ((), size shr sh) param (result, input)
    `andThen` do putIO workers future result
                 touchLifetime nativeExecutable
  return future

{-# INLINE generateOp #-}
generateOp
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Par Native (Future (Array sh e))
generateOp = simpleOp "generate"

{-# INLINE transformOp #-}
transformOp
    :: HasCallStack
    => ArrayR (Array sh  a)
    -> ArrayR (Array sh' b)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh'
    -> Array sh a
    -> Par Native (Future (Array sh' b))
transformOp repr repr' NativeR{..} gamma aenv sh' input = do
  let fun = nativeExecutable !# "transform"
  Native{..} <- gets llvmTarget
  future     <- new
  result     <- allocateRemote repr' sh'
  let param = TupRsingle (ParamRarray repr') `TupRpair` TupRsingle (ParamRarray repr)
  scheduleOp fun gamma aenv (arrayRshape repr') sh' param (result, input)
    `andThen` do putIO workers future result
                 touchLifetime nativeExecutable
  return future

{-# INLINE backpermuteOp #-}
backpermuteOp
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ShapeR sh'
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh'
    -> Array sh e
    -> Par Native (Future (Array sh' e))
backpermuteOp (ArrayR shr tp) shr' = transformOp (ArrayR shr tp) (ArrayR shr' tp)

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
-- 'foldAllOp' in particular behaves differently whether we are evaluating the
-- array in parallel or sequentially.
--

{-# INLINE fold1Op #-}
fold1Op
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par Native (Future (Array sh e))
fold1Op repr exe gamma aenv arr@(delayedShape -> sh@(sx, sz))
  = boundsCheck "empty array" (sz > 0)
  $ case size (ShapeRsnoc $ arrayRshape repr) sh of
      0 -> newFull =<< allocateRemote repr sx    -- empty, but possibly with non-zero dimensions
      _ -> foldCore repr exe gamma aenv arr

{-# INLINE foldOp #-}
foldOp
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par Native (Future (Array sh e))
foldOp repr exe gamma aenv arr@(delayedShape -> sh@(sx, _)) =
  case size (ShapeRsnoc $ arrayRshape repr) sh of
    0 -> generateOp repr exe gamma aenv sx
    _ -> foldCore repr exe gamma aenv arr

{-# INLINE foldCore #-}
foldCore
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par Native (Future (Array sh e))
foldCore repr exe gamma aenv arr
  | ArrayR ShapeRz tp <- repr
  = foldAllOp tp exe gamma aenv arr
  --
  | otherwise
  = foldDimOp repr exe gamma aenv arr

{-# INLINE foldAllOp #-}
foldAllOp
    :: HasCallStack
    => TypeR e
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Vector e)
    -> Par Native (Future (Scalar e))
foldAllOp tp NativeR{..} gamma aenv arr = do
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- allocateRemote (ArrayR dim0 tp) ()
  let
      minsize = 4096
      splits  = numWorkers workers
      ranges  = divideWork1 splits minsize ((), 0) sh (,,)
      steps   = Seq.length ranges
      sh      = delayedShape arr
  --
  if steps <= 1
    then
      let param = TupRsingle (ParamRarray $ ArrayR dim0 tp) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray $ ArrayR dim1 tp)
      in  scheduleOpUsing ranges (nativeExecutable !# "foldAllS") gamma aenv dim1 param (result, manifest arr)
            `andThen` do putIO workers future result
                         touchLifetime nativeExecutable

    else do
      let param1 = TupRsingle (ParamRarray $ ArrayR dim1 tp) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray $ ArrayR dim1 tp)
      let param2 = TupRsingle (ParamRarray $ ArrayR dim1 tp) `TupRpair` TupRsingle (ParamRarray $ ArrayR dim0 tp)
      tmp   <- allocateRemote (ArrayR dim1 tp) ((), steps)
      job2  <- mkJobUsing (Seq.singleton (0, ((), 0), ((), steps))) (nativeExecutable !# "foldAllP2") gamma aenv dim1 param2 (tmp, result)
                 `andThen` do putIO workers future result
                              touchLifetime nativeExecutable

      job1  <- mkJobUsingIndex ranges (nativeExecutable !# "foldAllP1") gamma aenv dim1 param1 (tmp, manifest arr)
                 `andThen` do schedule workers job2

      liftIO $ schedule workers job1
  --
  return future


{-# INLINE foldDimOp #-}
foldDimOp
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par Native (Future (Array sh e))
foldDimOp repr NativeR{..} gamma aenv arr@(delayedShape -> (sh, _)) = do
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- allocateRemote repr sh
  let
      ArrayR shr tp = repr
      fun     = nativeExecutable !# "fold"
      splits  = numWorkers workers
      minsize = 1
      param   = TupRsingle (ParamRarray repr) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray $ ArrayR (ShapeRsnoc shr) tp)
  --
  scheduleOpWith splits minsize fun gamma aenv shr sh param (result, manifest arr)
    `andThen` do putIO workers future result
                 touchLifetime nativeExecutable
  return future


{-# INLINE foldSegOp #-}
foldSegOp
    :: HasCallStack
    => IntegralType i
    -> ArrayR (Array (sh, Int) e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Delayed (Segments i)
    -> Par Native (Future (Array (sh, Int) e))
foldSegOp int repr NativeR{..} gamma aenv input@(delayedShape -> (sh, _)) segments@(delayedShape -> ((), ss)) = do
  Native{..}  <- gets llvmTarget
  future      <- new
  let
      n       = ss-1
      splits  = numWorkers workers
      minsize = 1
      shR     = arrayRshape repr
      segR    = ArrayR dim1 $ TupRsingle $ SingleScalarType $ NumSingleType $ IntegralNumType int
      param   = TupRsingle (ParamRarray repr) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray segR)
  --
  result  <- allocateRemote repr (sh, n)
  scheduleOpWith splits minsize (nativeExecutable !# "foldSegP") gamma aenv shR (sh, n) param ((result, manifest input), manifest segments)
    `andThen` do putIO workers future result
                 touchLifetime nativeExecutable

  return future


{-# INLINE scanOp #-}
scanOp
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par Native (Future (Array (sh, Int) e))
scanOp repr exe gamma aenv arr@(delayedShape -> (sz, n)) =
  case n of
    0 -> generateOp repr exe gamma aenv (sz, 1)
    _ -> scanCore   repr exe gamma aenv (n+1) arr

{-# INLINE scan1Op #-}
scan1Op
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par Native (Future (Array (sh, Int) e))
scan1Op repr exe gamma aenv arr@(delayedShape -> (_, n))
  = boundsCheck "empty array" (n > 0)
  $ scanCore repr exe gamma aenv n arr

{-# INLINE scanCore #-}
scanCore
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Int        -- output size of innermost dimension
    -> Delayed (Array (sh, Int) e)
    -> Par Native (Future (Array (sh, Int) e))
scanCore repr NativeR{..} gamma aenv m input@(delayedShape -> (sz, n)) = do
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- allocateRemote repr (sz, m)
  --
  let paramA = TupRsingle $ ParamRarray repr
      param  = paramA `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
      shR    = arrayRshape (reduceRank repr)

  if isMultiDim $ arrayRshape repr
    -- This is a multidimensional scan. Each partial scan result is evaluated
    -- individually by a thread, so no inter-thread communication is required.
    then
      let
          fun     = nativeExecutable !# "scanS"
          splits  = numWorkers workers
          minsize = 1
      in
      scheduleOpWith splits minsize fun gamma aenv shR sz param (result, manifest input)
        `andThen` do putIO workers future result
                     touchLifetime nativeExecutable

    -- This is a one-dimensional scan. If the array is small just compute it
    -- sequentially using a single thread, otherwise we require multiple steps
    -- to execute it in parallel.
    else
      if n < 8192
        -- sequential execution
        then
          scheduleOpUsing (Seq.singleton (0, (), ())) (nativeExecutable !# "scanS") gamma aenv dim0 param (result, manifest input)
            `andThen` do putIO workers future result
                         touchLifetime nativeExecutable

        -- parallel execution
        else do
          let
              splits   = numWorkers workers
              minsize  = 8192
              ranges   = divideWork dim1 splits minsize ((), 0) ((), n) (,,)
              steps    = Seq.length ranges
              reprTmp  = ArrayR dim1 $ arrayRtype repr
              paramTmp = TupRsingle $ ParamRarray reprTmp
              param1   = TupRsingle ParamRint `TupRpair` paramA `TupRpair` paramTmp `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
              param3   = TupRsingle ParamRint `TupRpair` paramA `TupRpair` paramTmp
          --
          -- XXX: Should the sequential scan of the carry-in values just be
          -- executed immediately as part of the finalisation action?
          --
          tmp   <- allocateRemote (ArrayR dim1 $ arrayRtype repr) ((), steps)
          job3  <- mkJobUsingIndex ranges (nativeExecutable !# "scanP3") gamma aenv dim1 param3 ((steps, result), tmp)
                     `andThen` do putIO workers future result
                                  touchLifetime nativeExecutable
          job2  <- mkJobUsing (Seq.singleton (0, ((), 0), ((), steps))) (nativeExecutable !# "scanP2") gamma aenv dim1 paramTmp tmp
                     `andThen` schedule workers job3
          job1  <- mkJobUsingIndex ranges (nativeExecutable !# "scanP1") gamma aenv dim1 param1 (((steps, result), tmp), manifest input)
                     `andThen` schedule workers job2

          liftIO $ schedule workers job1
  --
  return future


{-# INLINE scan'Op #-}
scan'Op
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par Native (Future (Array (sh, Int) e, Array sh e))
scan'Op repr exe gamma aenv arr@(delayedShape -> (sz, n)) = do
  case n of
    0 -> do
      out     <- allocateRemote repr (sz, 0)
      sum     <- generateOp (reduceRank repr) exe gamma aenv sz
      future  <- new
      fork $ do sum' <- get sum
                put future (out, sum')
      return future
    --
    _ -> scan'Core repr exe gamma aenv arr

{-# INLINE scan'Core #-}
scan'Core
    :: forall aenv sh e. HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par Native (Future (Array (sh, Int) e, Array sh e))
scan'Core repr NativeR{..} gamma aenv input@(delayedShape -> sh@(sz, n)) = do
  let
      ArrayR shR eR   = repr
      ShapeRsnoc shR' = shR
      repr'           = ArrayR shR' eR
      paramA          = TupRsingle $ ParamRarray repr
      paramA'         = TupRsingle $ ParamRarray repr'
  --
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- allocateRemote repr  sh
  sums        <- allocateRemote repr' sz
  --
  if isMultiDim shR
    -- This is a multidimensional scan. Each partial scan result is evaluated
    -- individually by a thread, so no inter-thread communication is required.
    --
    then
      let fun     = nativeExecutable !# "scanS"
          splits  = numWorkers workers
          minsize = 1
          param   = paramA `TupRpair` paramA' `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
      in
      scheduleOpWith splits minsize fun gamma aenv shR' sz param ((result, sums), manifest input)
        `andThen` do putIO workers future (result, sums)
                     touchLifetime nativeExecutable

    -- One dimensional scan. If the array is small just compute it sequentially
    -- with a single thread, otherwise we require multiple steps to execute it
    -- in parallel.
    --
    else
      if n < 8192
        -- sequential execution
        then
          let param = paramA `TupRpair` paramA' `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
          in  scheduleOpUsing (Seq.singleton (0, (), ())) (nativeExecutable !# "scanS") gamma aenv dim0 param ((result, sums), manifest input)
                `andThen` do putIO workers future (result, sums)
                             touchLifetime nativeExecutable

        -- parallel execution
        else do
          let
              splits   = numWorkers workers
              minsize  = 8192
              ranges   = divideWork1 splits minsize ((), 0) ((), n) (,,)
              steps    = Seq.length ranges
              reprTmp  = ArrayR dim1 eR
              paramTmp = TupRsingle $ ParamRarray reprTmp
              param1   = TupRsingle ParamRint `TupRpair` paramA `TupRpair` paramTmp `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
              param2   = paramA' `TupRpair` paramTmp
              param3   = TupRsingle ParamRint `TupRpair` paramA `TupRpair` paramTmp
          --
          tmp   <- allocateRemote reprTmp ((), steps)
          job3  <- mkJobUsingIndex ranges (nativeExecutable !# "scanP3") gamma aenv dim1 param3 ((steps, result), tmp)
                     `andThen` do putIO workers future (result, sums)
                                  touchLifetime nativeExecutable
          job2  <- mkJobUsing (Seq.singleton (0, ((), 0), ((), steps))) (nativeExecutable !# "scanP2") gamma aenv dim1 param2 (sums, tmp)
                     `andThen` schedule workers job3
          job1  <- mkJobUsingIndex ranges (nativeExecutable !# "scanP1") gamma aenv dim1 param1 (((steps, result), tmp), manifest input)
                     `andThen` schedule workers job2

          liftIO $ schedule workers job1
  --
  return future

isMultiDim :: ShapeR sh -> Bool
isMultiDim (ShapeRsnoc ShapeRz) = False
isMultiDim _                    = True

-- Forward permutation, specified by an indexing mapping into an array and a
-- combination function to combine elements.
--
{-# INLINE permuteOp #-}
permuteOp
    :: forall sh e sh' aenv. HasCallStack
    => Bool
    -> ArrayR (Array sh e)
    -> ShapeR sh'
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Array sh' e
    -> Delayed (Array sh e)
    -> Par Native (Future (Array sh' e))
permuteOp inplace repr shr' NativeR{..} gamma aenv defaults@(shape -> shOut) input@(delayedShape -> shIn) = do
  let
      ArrayR shr tp = repr
      repr' = ArrayR shr' tp
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- if inplace
                   then Debug.trace Debug.dump_exec               "exec: permute/inplace"                            $ return defaults
                   else Debug.timed Debug.dump_exec (\wall cpu -> "exec: permute/clone " ++ Debug.elapsedS wall cpu) $ liftPar (cloneArray repr' defaults)
  let
      splits  = numWorkers workers
      minsize = case shr of
                  ShapeRsnoc ShapeRz              -> 4096
                  ShapeRsnoc (ShapeRsnoc ShapeRz) -> 64
                  _                               -> 16
      ranges  = divideWork shr splits minsize (empty shr) shIn (,,)
      steps   = Seq.length ranges
      paramR = TupRsingle (ParamRarray repr') `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
  --
  if steps <= 1
    -- sequential execution does not require handling critical sections
    then
      scheduleOpUsing ranges (nativeExecutable !# "permuteS") gamma aenv shr paramR (result, manifest input)
        `andThen` do putIO workers future result
                     touchLifetime nativeExecutable

    -- parallel execution
    else
      case lookupFunction "permuteP_rmw" nativeExecutable of
        -- using atomic operations
        Just f ->
          scheduleOpUsing ranges f gamma aenv shr paramR (result, manifest input)
            `andThen` do putIO workers future result
                         touchLifetime nativeExecutable

        -- uses a temporary array of spin-locks to guard the critical section
        Nothing -> do
          let m           = size shr' shOut
              reprBarrier = ArrayR dim1 $ TupRsingle scalarTypeWord8
              paramR'     = TupRsingle (ParamRarray repr') `TupRpair` TupRsingle (ParamRarray reprBarrier) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
          --
          barrier@(Array _ adb) <- allocateRemote reprBarrier ((), m) :: Par Native (Vector Word8)
          liftIO $ memset (unsafeUniqueArrayPtr adb) 0 m
          scheduleOpUsing ranges (nativeExecutable !# "permuteP_mutex") gamma aenv shr paramR' ((result, barrier), manifest input)
            `andThen` do putIO workers future result
                         touchLifetime nativeExecutable
  --
  return future


{-# INLINE stencil1Op #-}
stencil1Op
    :: HasCallStack
    => TypeR a
    -> ArrayR (Array sh b)
    -> sh
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array sh a)
    -> Par Native (Future (Array sh b))
stencil1Op tp repr halo exe gamma aenv input@(delayedShape -> sh) =
  stencilCore repr exe gamma aenv halo sh (TupRsingle $ ParamRmaybe $ ParamRarray $ ArrayR (arrayRshape repr) tp) (manifest input)

{-# INLINE stencil2Op #-}
stencil2Op
    :: forall aenv sh a b c. HasCallStack
    => TypeR a
    -> TypeR b
    -> ArrayR (Array sh c)
    -> sh
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array sh a)
    -> Delayed (Array sh b)
    -> Par Native (Future (Array sh c))
stencil2Op t1 t2 repr halo exe gamma aenv input1@(delayedShape -> sh1) input2@(delayedShape -> sh2) =
  stencilCore repr exe gamma aenv halo (intersect (arrayRshape repr) sh1 sh2) (param t1 `TupRpair` param t2) (manifest input1, manifest input2)
  where
    shr = arrayRshape repr
    param :: TypeR t -> ParamsR Native (Maybe (Array sh t))
    param = TupRsingle . ParamRmaybe . ParamRarray . ArrayR shr

{-# INLINE stencilCore #-}
stencilCore
    :: forall aenv sh e params. HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh                       -- border dimensions (i.e. index of first interior element)
    -> sh                       -- output array size
    -> ParamsR Native params
    -> params
    -> Par Native (Future (Array sh e))
stencilCore repr NativeR{..} gamma aenv halo sh paramsR params = do
  Native{..} <- gets llvmTarget
  future     <- new
  result     <- allocateRemote repr sh
  let
      shr     = arrayRshape repr
      inside  = nativeExecutable !# "stencil_inside"
      border  = nativeExecutable !# "stencil_border"

      splits  = numWorkers workers
      minsize = case shr of
                  ShapeRsnoc ShapeRz              -> 4096
                  ShapeRsnoc (ShapeRsnoc ShapeRz) -> 64
                  _                               -> 16

      ins     = divideWork shr splits minsize halo (sub sh halo) (,,)
      outs    = asum . flip fmap (stencilBorders shr sh halo) $ \(u,v) -> divideWork shr splits minsize u v (,,)

      sub :: sh -> sh -> sh
      sub a b = go shr a b
        where
          go :: ShapeR t -> t -> t -> t
          go ShapeRz          ()      ()      = ()
          go (ShapeRsnoc shr') (xa,xb) (ya,yb) = (go shr' xa ya, xb - yb)

      paramsR' = TupRsingle (ParamRarray repr) `TupRpair` paramsR
  --
  jobsInside <- mkTasksUsing ins  inside gamma aenv shr paramsR' (result, params)
  jobsBorder <- mkTasksUsing outs border gamma aenv shr paramsR' (result, params)
  let jobTasks  = jobsInside Seq.>< jobsBorder
      jobDone   = Just $ do putIO workers future result
                            touchLifetime nativeExecutable
  --
  liftIO $ schedule workers =<< timed "stencil" Job{..}
  return future

-- Compute the stencil border regions, where we may need to evaluate the
-- boundary conditions.
--
{-# INLINE stencilBorders #-}
stencilBorders
    :: forall sh. HasCallStack
    => ShapeR sh
    -> sh
    -> sh
    -> Seq (sh, sh)
stencilBorders shr sh halo = Seq.fromFunction (2 * rank shr) face
  where
    face :: Int -> (sh, sh)
    face n = go n shr sh halo

    go :: Int -> ShapeR t -> t -> t -> (t, t)
    go _ ShapeRz          ()         ()         = ((), ())
    go n (ShapeRsnoc shr') (sha, sza) (shb, szb)
      = let
            (sha', shb')  = go (n-2) shr' sha shb
            (sza', szb')
              | n <  0    = (0,       sza)
              | n == 0    = (0,       szb)
              | n == 1    = (sza-szb, sza)
              | otherwise = (szb,     sza-szb)
        in
        ((sha', sza'), (shb', szb'))

{-# INLINE aforeignOp #-}
aforeignOp
    :: HasCallStack
    => String
    -> ArraysR as
    -> ArraysR bs
    -> (as -> Par Native (Future bs))
    -> as
    -> Par Native (Future bs)
aforeignOp name _ _ asm arr = do
  wallBegin <- liftIO getMonotonicTime
  result    <- Debug.timed Debug.dump_exec (\wall cpu -> printf "exec: %s %s" name (Debug.elapsedP wall cpu)) (asm arr)
  wallEnd   <- liftIO getMonotonicTime
  liftIO $ Debug.addProcessorTime Debug.Native (wallEnd - wallBegin)
  return result


-- Skeleton execution
-- ------------------

(!#) :: HasCallStack => Lifetime FunctionTable -> ShortByteString -> Function
(!#) exe name
  = fromMaybe (internalError ("function not found: " ++ S8.unpack name))
  $ lookupFunction name exe

lookupFunction :: ShortByteString -> Lifetime FunctionTable -> Maybe Function
lookupFunction name nativeExecutable = do
  find (\(n,_) -> n == name) (functionTable (unsafeGetValue nativeExecutable))

andThen :: (Maybe a -> t) -> a -> t
andThen f g = f (Just g)

delayedShape :: Delayed (Array sh e) -> sh
delayedShape (Delayed sh) = sh
delayedShape (Manifest a) = shape a

manifest :: Delayed (Array sh e) -> Maybe (Array sh e)
manifest (Manifest a) = Just a
manifest Delayed{}    = Nothing


{-# INLINABLE scheduleOp #-}
scheduleOp
    :: HasCallStack
    => Function
    -> Gamma aenv
    -> Val aenv
    -> ShapeR sh
    -> sh
    -> ParamsR Native params
    -> params
    -> Maybe Action
    -> Par Native ()
scheduleOp fun gamma aenv shr sz paramsR params done = do
  Native{..} <- gets llvmTarget
  let
      splits  = numWorkers workers
      minsize = case shr of
                  ShapeRsnoc ShapeRz              -> 4096
                  ShapeRsnoc (ShapeRsnoc ShapeRz) -> 64
                  _                               -> 16
  --
  scheduleOpWith splits minsize fun gamma aenv shr sz paramsR params done

-- Schedule an operation over the entire iteration space, specifying the number
-- of partitions and minimum dimension size.
--
{-# INLINABLE scheduleOpWith #-}
scheduleOpWith
    :: Int            -- # subdivisions (hint)
    -> Int            -- minimum size of a dimension (must be a power of two)
    -> Function       -- function to execute
    -> Gamma aenv
    -> Val aenv
    -> ShapeR sh
    -> sh
    -> ParamsR Native params
    -> params
    -> Maybe Action   -- run after the last piece completes
    -> Par Native ()
scheduleOpWith splits minsize fun gamma aenv shr sz paramsR params done = do
  Native{..} <- gets llvmTarget
  job        <- mkJob splits minsize fun gamma aenv shr (empty shr) sz paramsR params done
  liftIO $ schedule workers job

{-# INLINABLE scheduleOpUsing #-}
scheduleOpUsing
    :: Seq (Int, sh, sh)
    -> Function
    -> Gamma aenv
    -> Val aenv
    -> ShapeR sh
    -> ParamsR Native params
    -> params
    -> Maybe Action
    -> Par Native ()
scheduleOpUsing ranges fun gamma aenv shr paramsR params jobDone = do
  Native{..} <- gets llvmTarget
  job        <- mkJobUsing ranges fun gamma aenv shr paramsR params jobDone
  liftIO $ schedule workers job

{-# INLINABLE mkJob #-}
mkJob :: Int
      -> Int
      -> Function
      -> Gamma aenv
      -> Val aenv
      -> ShapeR sh
      -> sh
      -> sh
      -> ParamsR Native params
      -> params
      -> Maybe Action
      -> Par Native Job
mkJob splits minsize fun gamma aenv shr from to paramsR params jobDone =
  mkJobUsing (divideWork shr splits minsize from to (,,)) fun gamma aenv shr paramsR params jobDone

{-# INLINABLE mkJobUsing #-}
mkJobUsing
      :: Seq (Int, sh, sh)
      -> Function
      -> Gamma aenv
      -> Val aenv
      -> ShapeR sh
      -> ParamsR Native params
      -> params
      -> Maybe Action
      -> Par Native Job
mkJobUsing ranges fun@(name,_) gamma aenv shr paramsR params jobDone = do
  jobTasks <- mkTasksUsing ranges fun gamma aenv shr paramsR params
  liftIO    $ timed name Job {..}

{-# INLINABLE mkJobUsingIndex #-}
mkJobUsingIndex
      :: Seq (Int, sh, sh)
      -> Function
      -> Gamma aenv
      -> Val aenv
      -> ShapeR sh
      -> ParamsR Native params
      -> params
      -> Maybe Action
      -> Par Native Job
mkJobUsingIndex ranges fun@(name,_) gamma aenv shr paramsR params jobDone = do
  jobTasks <- mkTasksUsingIndex ranges fun gamma aenv shr paramsR params
  liftIO    $ timed name Job {..}

{-# INLINABLE mkTasksUsing #-}
mkTasksUsing
      :: Seq (Int, sh, sh)
      -> Function
      -> Gamma aenv
      -> Val aenv
      -> ShapeR sh
      -> ParamsR Native params
      -> params
      -> Par Native (Seq Action)
mkTasksUsing ranges (name, f) gamma aenv shr paramsR params = do
  arg <- marshalParams' @Native (paramsR `TupRpair` TupRsingle (ParamRenv gamma)) (params, aenv)
  return $ flip fmap ranges $ \(_,u,v) -> do
    sched $ printf "%s (%s) -> (%s)" (S8.unpack name) (showShape shr u) (showShape shr v)
    let argU = marshalShape' @Native shr u
    let argV = marshalShape' @Native shr v
    callFFI f retVoid $ DL.toList $ argU `DL.append` argV `DL.append` arg

{-# INLINABLE mkTasksUsingIndex #-}
mkTasksUsingIndex
      :: Seq (Int, sh, sh)
      -> Function
      -> Gamma aenv
      -> Val aenv
      -> ShapeR sh
      -> ParamsR Native params
      -> params
      -> Par Native (Seq Action)
mkTasksUsingIndex ranges (name, f) gamma aenv shr paramsR params = do
  arg <- marshalParams' @Native (paramsR `TupRpair` TupRsingle (ParamRenv gamma)) (params, aenv)
  return $ flip fmap ranges $ \(i,u,v) -> do
    sched $ printf "%s (%s) -> (%s)" (S8.unpack name) (showShape shr u) (showShape shr v)
    let argU = marshalShape' @Native shr u
    let argV = marshalShape' @Native shr v
    let argI = DL.singleton $ marshalInt @Native i
    callFFI f retVoid $ DL.toList $ argU `DL.append` argV `DL.append` argI `DL.append` arg


-- Standard C functions
-- --------------------

memset :: Ptr Word8 -> Word8 -> Int -> IO ()
memset p w s = c_memset p (fromIntegral w) (fromIntegral s) >> return ()

foreign import ccall unsafe "string.h memset" c_memset
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)


-- Debugging
-- ---------

-- Since the (new) thread scheduler does not operate in block-synchronous mode,
-- it is a bit more difficult to track how long an individual operation took to
-- execute as we won't know when exactly it will begin. The following method
-- (where initial timing information is recorded as the first task) should give
-- reasonable results.
--
-- TLM: missing GC stats information (verbose mode) since we aren't using the
--      the default 'timed' helper.
--
timed :: ShortByteString -> Job -> IO Job
timed name job =
  case Debug.debuggingIsEnabled of
    False -> return job
    True  -> do
      yes <- if Debug.monitoringIsEnabled
               then return True
               else Debug.getFlag Debug.dump_exec
      --
      if yes
        then do
          ref1 <- newIORef 0
          ref2 <- newIORef 0
          let start = do !wall0 <- getMonotonicTime
                         !cpu0  <- getCPUTime
                         writeIORef ref1 wall0
                         writeIORef ref2 cpu0

              end   = do !cpu1  <- getCPUTime
                         !wall1 <- getMonotonicTime
                         !wall0 <- readIORef ref1
                         !cpu0  <- readIORef ref2
                         --
                         let wallTime = wall1 - wall0
                             cpuTime  = fromIntegral (cpu1 - cpu0) * 1E-12
                         --
                         Debug.addProcessorTime Debug.Native cpuTime
                         Debug.traceIO Debug.dump_exec $ printf "exec: %s %s" (S8.unpack name) (Debug.elapsedP wallTime cpuTime)
              --
          return $ Job { jobTasks = start Seq.<| jobTasks job
                       , jobDone  = case jobDone job of
                                      Nothing       -> Just end
                                      Just finished -> Just (finished >> end)
                       }
        else
          return job

-- accelerate/cbits/clock.c
foreign import ccall unsafe "clock_gettime_monotonic_seconds" getMonotonicTime :: IO Double


sched :: String -> IO ()
sched msg
  = Debug.when Debug.verbose
  $ Debug.when Debug.dump_sched
  $ do tid <- myThreadId
       Debug.putTraceMsg $ printf "sched: %s %s" (show tid) msg

