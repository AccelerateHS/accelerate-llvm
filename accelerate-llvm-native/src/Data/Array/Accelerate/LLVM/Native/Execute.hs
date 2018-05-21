{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute
-- Copyright   : [2014..2018] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute (

  executeAcc,
  executeOpenAcc

) where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.Analysis.Match
import Data.Array.Accelerate.LLVM.Execute
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

-- library
import Control.Concurrent                                           ( myThreadId )
import Control.Monad.State                                          ( gets )
import Control.Monad.Trans                                          ( liftIO )
import Data.ByteString.Short                                        ( ShortByteString )
import Data.IORef                                                   ( newIORef, readIORef, writeIORef )
import Data.List                                                    ( find )
import Data.Maybe                                                   ( fromMaybe )
import Data.Proxy                                                   ( Proxy(..) )
import Data.Sequence                                                ( Seq )
import Data.Word                                                    ( Word8 )
import System.CPUTime                                               ( getCPUTime )
import Text.Printf                                                  ( printf )
import qualified Data.ByteString.Short.Char8                        as S8
import qualified Data.Sequence                                      as Seq
import Prelude                                                      hiding ( map, sum, scanl, scanr, init )

import Foreign.C
import Foreign.LibFFI
import Foreign.Ptr


{-# SPECIALISE INLINE executeAcc     :: ExecAcc     Native      a ->             Par Native (Future a) #-}
{-# SPECIALISE INLINE executeOpenAcc :: ExecOpenAcc Native aenv a -> Val aenv -> Par Native (Future a) #-}

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
  {-# INLINE fold1       #-}
  {-# INLINE foldSeg     #-}
  {-# INLINE fold1Seg    #-}
  {-# INLINE scanl       #-}
  {-# INLINE scanl1      #-}
  {-# INLINE scanl'      #-}
  {-# INLINE scanr       #-}
  {-# INLINE scanr1      #-}
  {-# INLINE scanr'      #-}
  {-# INLINE permute     #-}
  {-# INLINE stencil1    #-}
  {-# INLINE stencil2    #-}
  {-# INLINE aforeign    #-}
  map           = mapOp
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
  -- stencil1      = stencil1Op
  stencil1 _    = simpleOp
  stencil2 _ _  = stencil2Op
  aforeign      = aforeignOp


-- Skeleton implementation
-- -----------------------

-- Simple kernels just needs to know the shape of the output array.
--
{-# INLINE simpleOp #-}
simpleOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Par Native (Future (Array sh e))
simpleOp NativeR{..} gamma aenv sh = do
  let fun = case functionTable (unsafeGetValue nativeExecutable) of
              f:_ -> f
              _   -> $internalError "simpleOp" "no functions found"
  --
  Native{..} <- gets llvmTarget
  future     <- new
  result     <- allocateRemote sh
  scheduleOp fun gamma aenv sh result
    `andThen` do putIO workers future result
                 touchLifetime nativeExecutable   -- XXX: must not unload the object code early
  return future

{--
simpleOpNestedLoops
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> LLVM Native (Array sh e)
simpleOpNestedLoops exe gamma aenv () sh = withExecutable exe $ \nativeExecutable -> do
  let fun = case functionTable nativeExecutable of
              f:_ -> f
              _   -> $internalError "simpleOpNestedLoops" "no functions found"
  --
  Native{..} <- gets llvmTarget
  liftIO $ do
    out <- allocateArray sh
    executeOpMultiDimensional defaultLargePPT fillP fun gamma aenv empty sh out
    return out
--}

{-# INLINE simpleNamed #-}
simpleNamed
    :: (Shape sh, Elt e)
    => ShortByteString
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Par Native (Future (Array sh e))
simpleNamed name NativeR{..} gamma aenv sh = do
  let fun = nativeExecutable !# name
  Native{..} <- gets llvmTarget
  future     <- new
  result     <- allocateRemote sh
  scheduleOp fun gamma aenv (Z :. size sh) result -- XXX: nested loops
    `andThen` do putIO workers future result
                 touchLifetime nativeExecutable   -- XXX: must not unload the object code early
  return future


{--
simpleNamedNestedLoops
    :: (Shape sh, Elt e)
    => ShortByteString
    -> ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> LLVM Native (Array sh e)
simpleNamedNestedLoops name exe gamma aenv () sh = withExecutable exe $ \nativeExecutable -> do
  Native{..} <- gets llvmTarget
  liftIO $ do
    out <- allocateArray sh
    executeOpMultiDimensional defaultLargePPT fillP (nativeExecutable !# name) gamma aenv empty sh out
    return out
--}

-- Map over an array can ignore the dimensionality of the array and treat it as
-- its underlying linear representation.
--
{-# INLINE mapOp #-}
mapOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Par Native (Future (Array sh e))
mapOp NativeR{..} gamma aenv sh = do
  let fun = nativeExecutable !# "map"
  Native{..} <- gets llvmTarget
  future     <- new
  result     <- allocateRemote sh
  scheduleOp fun gamma aenv (Z :. size sh) result
    `andThen` do putIO workers future result
                 touchLifetime nativeExecutable   -- XXX: must not unload the object code early
  return future


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
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> Par Native (Future (Array sh e))
fold1Op exe gamma aenv sh@(sx :. sz)
  = $boundsCheck "fold1" "empty array" (sz > 0)
  $ case size sh of
      0 -> newFull =<< allocateRemote sx    -- empty, but possibly with non-zero dimensions
      _ -> foldCore exe gamma aenv sh

{-# INLINE foldOp #-}
foldOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> Par Native (Future (Array sh e))
foldOp exe gamma aenv sh@(sx :. _) =
  case size sh of
    0 -> simpleNamed "generate" exe gamma aenv sx
    _ -> foldCore exe gamma aenv sh

{-# INLINE foldCore #-}
foldCore
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> Par Native (Future (Array sh e))
foldCore exe gamma aenv sh
  | Just Refl <- matchShapeType sh (undefined::DIM1)
  = foldAllOp exe gamma aenv sh
  --
  | otherwise
  = foldDimOp exe gamma aenv sh

{-# INLINE foldAllOp #-}
foldAllOp
    :: forall aenv e. Elt e
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> DIM1
    -> Par Native (Future (Scalar e))
foldAllOp NativeR{..} gamma aenv sh = do
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- allocateRemote Z
  let
      minsize = 4096
      splits  = numWorkers workers
      ranges  = divideWork splits minsize empty sh (,,)
      steps   = Seq.length ranges
  --
  if steps <= 1
    then
      scheduleOpUsing ranges (nativeExecutable !# "foldAllS") gamma aenv result
        `andThen` do putIO workers future result
                     touchLifetime nativeExecutable

    else do
      tmp   <- allocateRemote (Z :. steps) :: Par Native (Vector e)
      job2  <- mkJobUsing (Seq.singleton (0, empty, Z:.steps)) (nativeExecutable !# "foldAllP2") gamma aenv (tmp, result)
                 `andThen` do putIO workers future result
                              touchLifetime nativeExecutable

      job1  <- mkJobUsingIndex ranges (nativeExecutable !# "foldAllP1") gamma aenv tmp
                 `andThen` do schedule workers job2

      liftIO $ schedule workers job1
  --
  return future


{-# INLINE foldDimOp #-}
foldDimOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> Par Native (Future (Array sh e))
foldDimOp NativeR{..} gamma aenv (sh :. sz) = do
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- allocateRemote sh
  let
      fun     = nativeExecutable !# "fold"
      splits  = numWorkers workers
      minsize = 1
  --
  scheduleOpWith splits minsize fun gamma aenv (Z :. size sh) (sz, result)
    `andThen` do putIO workers future result
                 touchLifetime nativeExecutable
  return future


{-# INLINE foldSegOp #-}
foldSegOp
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> (Z  :. Int)
    -> Par Native (Future (Array (sh :. Int) e))
foldSegOp NativeR{..} gamma aenv (sh :. _) (Z :. ss) = do
  Native{..}  <- gets llvmTarget
  future      <- new
  --
  if segmentOffset
    then do
      -- We can execute in parallel. The segments array represents offset
      -- indices into the source array, rather than the length of each segment.
      --
      let
          n       = ss-1
          splits  = numWorkers workers
          minsize = case rank (undefined :: (sh:.Int)) of
                      1 -> 64
                      _ -> 16
      --
      result  <- allocateRemote (sh :. n)
      scheduleOpWith splits minsize (nativeExecutable !# "foldSegP") gamma aenv (Z :. size (sh:.n)) result
        `andThen` do putIO workers future result
                     touchLifetime nativeExecutable

    else do
      -- Execute on a single processor. The segments array contains the length
      -- of each segment.
      --
      result  <- allocateRemote (sh :. ss)
      scheduleOpUsing (Seq.singleton (0, empty, Z :. size (sh:.ss))) (nativeExecutable !# "foldSegS") gamma aenv result
        `andThen` do putIO workers future result
                     touchLifetime nativeExecutable
  --
  return future


{-# INLINE scanOp #-}
scanOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> Par Native (Future (Array (sh:.Int) e))
scanOp exe gamma aenv (sz :. n) =
  case n of
    0 -> simpleNamed "generate" exe gamma aenv (sz :. 1)
    _ -> scanCore exe gamma aenv sz n (n+1)

{-# INLINE scan1Op #-}
scan1Op
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> Par Native (Future (Array (sh:.Int) e))
scan1Op exe gamma aenv (sz :. n)
  = $boundsCheck "scan1" "empty array" (n > 0)
  $ scanCore exe gamma aenv sz n n

{-# INLINE scanCore #-}
scanCore
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh         -- outer dimension size
    -> Int        -- input size of innermost dimension
    -> Int        -- output size of innermost dimension
    -> Par Native (Future (Array (sh:.Int) e))
scanCore NativeR{..} gamma aenv sz n m = do
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- allocateRemote (sz :. m)
  --
  if rank sz > 0
    -- This is a multidimensional scan. Each partial scan result is evaluated
    -- individually by a thread, so no inter-thread communication is required.
    then
      let
          fun     = nativeExecutable !# "scanS"
          splits  = numWorkers workers
          minsize = 1
      in
      scheduleOpWith splits minsize fun gamma aenv (Z :. size sz) result
        `andThen` do putIO workers future result
                     touchLifetime nativeExecutable

    -- This is a one-dimensional scan. If the array is small just compute it
    -- sequentially using a single thread, otherwise we require multiple steps
    -- to execute it in parallel.
    else
      if n < 8192
        -- sequential execution
        then
          scheduleOpUsing (Seq.singleton (0, empty, Z:.1::DIM1)) (nativeExecutable !# "scanS") gamma aenv result
            `andThen` do putIO workers future result
                         touchLifetime nativeExecutable

        -- parallel execution
        else do
          let
              splits  = numWorkers workers
              minsize = 8192
              ranges  = divideWork splits minsize empty (Z:.n) (,,)
              steps   = Seq.length ranges
          --
          -- XXX: Should the sequential scan of the carry-in values just be
          -- executed immediately as part of the finalisation action?
          --
          tmp   <- allocateRemote (Z :. steps) :: Par Native (Vector e)
          job3  <- mkJobUsingIndex ranges (nativeExecutable !# "scanP3") gamma aenv (steps, result, tmp)
                     `andThen` do putIO workers future result
                                  touchLifetime nativeExecutable
          job2  <- mkJobUsing (Seq.singleton (0, empty, Z:.steps)) (nativeExecutable !# "scanP2") gamma aenv tmp
                     `andThen` schedule workers job3
          job1  <- mkJobUsingIndex ranges (nativeExecutable !# "scanP1") gamma aenv (steps, result, tmp)
                     `andThen` schedule workers job2

          liftIO $ schedule workers job1
  --
  return future


{-# INLINE scan'Op #-}
scan'Op
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> Par Native (Future (Array (sh:.Int) e, Array sh e))
scan'Op exe gamma aenv sh@(sz :. n) = do
  case n of
    0 -> do
      out     <- allocateRemote (sz :. 0)
      sum     <- simpleNamed "generate" exe gamma aenv sz
      future  <- new
      fork $ do sum' <- get sum
                put future (out, sum')
      return future
    --
    _ -> scan'Core exe gamma aenv sh

{-# INLINE scan'Core #-}
scan'Core
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> Par Native (Future (Array (sh:.Int) e, Array sh e))
scan'Core NativeR{..} gamma aenv sh@(sz :. n) = do
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- allocateRemote sh
  sums        <- allocateRemote sz
  --
  if rank sz > 0
    -- This is a multidimensional scan. Each partial scan result is evaluated
    -- individually by a thread, so no inter-thread communication is required.
    --
    then
      let fun     = nativeExecutable !# "scanS"
          splits  = numWorkers workers
          minsize = 1
      in
      scheduleOpWith splits minsize fun gamma aenv (Z :. size sz) (result, sums)
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
          scheduleOpUsing (Seq.singleton (0, empty, Z:.1::DIM1)) (nativeExecutable !# "scanS") gamma aenv (result, sums)
            `andThen` do putIO workers future (result, sums)
                         touchLifetime nativeExecutable

        -- parallel execution
        else do
          let
              splits  = numWorkers workers
              minsize = 8192
              ranges  = divideWork splits minsize empty (Z :. n) (,,)
              steps   = Seq.length ranges
          --
          tmp   <- allocateRemote (Z :. steps) :: Par Native (Vector e)
          job3  <- mkJobUsingIndex ranges (nativeExecutable !# "scanP3") gamma aenv (steps, result, tmp)
                     `andThen` do putIO workers future (result, sums)
                                  touchLifetime nativeExecutable
          job2  <- mkJobUsing (Seq.singleton (0, empty, Z:.steps)) (nativeExecutable !# "scanP2") gamma aenv (sums, tmp)
                     `andThen` schedule workers job3
          job1  <- mkJobUsingIndex ranges (nativeExecutable !# "scanP1") gamma aenv (steps, result, tmp)
                     `andThen` schedule workers job2

          liftIO $ schedule workers job1
  --
  return future


-- Forward permutation, specified by an indexing mapping into an array and a
-- combination function to combine elements.
--
{-# INLINE permuteOp #-}
permuteOp
    :: forall aenv sh sh' e. (Shape sh, Shape sh', Elt e)
    => Bool
    -> ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Array sh' e
    -> Par Native (Future (Array sh' e))
permuteOp inplace NativeR{..} gamma aenv shIn defaults@(shape -> shOut) = do
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- if inplace
                   then return defaults
                   else liftPar (cloneArray defaults)
  let
      splits  = numWorkers workers
      minsize = case rank (undefined::sh') of
                  1 -> 4096
                  2 -> 64
                  _ -> 16
      ranges  = divideWork splits minsize empty (Z :. size shIn) (,,) -- XXX: nested loops
      steps   = Seq.length ranges
  --
  if steps <= 1
    -- sequential execution does not require handling critical sections
    then
      scheduleOpUsing ranges (nativeExecutable !# "permuteS") gamma aenv result
        `andThen` do putIO workers future result
                     touchLifetime nativeExecutable

    -- parallel execution
    else
      case lookupFunction "permuteP_rmw" nativeExecutable of
        -- using atomic operations
        Just f ->
          scheduleOpUsing ranges f gamma aenv result
            `andThen` do putIO workers future result
                         touchLifetime nativeExecutable

        -- uses a temporary array of spin-locks to guard the critical section
        Nothing -> do
          let m = size shOut
          --
          barrier@(Array _ adb) <- allocateRemote (Z :. m) :: Par Native (Vector Word8)
          liftIO $ memset (ptrsOfArrayData adb) 0 m
          scheduleOpUsing ranges (nativeExecutable !# "permuteP_mutex") gamma aenv (result, barrier)
            `andThen` do putIO workers future result
                         touchLifetime nativeExecutable
  --
  return future


{--
permuteOpNested
    :: (Shape sh, Shape sh', Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Bool
    -> sh
    -> Array sh' e
    -> LLVM Native (Array sh' e)
permuteOpNested exe gamma aenv () inplace shIn dfs = withExecutable exe $ \nativeExecutable -> do
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
      executeOpMultiDimensional 1 fillS (nativeExecutable !# "permuteSNested") gamma aenv empty shIn out

    else liftIO $ do
      -- parallel permutation
      case lookupFunction "permuteP_rmwNested" nativeExecutable of
        Just f  -> executeOpMultiDimensional defaultLargePPT fillP f gamma aenv empty shIn out
        Nothing -> do
          barrier@(Array _ adb) <- allocateArray (Z :. m) :: IO (Vector Word8)
          memset (ptrsOfArrayData adb) 0 m
          executeOpMultiDimensional defaultLargePPT fillP (nativeExecutable !# "permuteP_mutexNested") gamma aenv empty shIn (out, barrier)

  return out


boundaryThickness
  :: StencilR sh a stencil
  -> sh
boundaryThickness = go
  where
    go :: StencilR sh a stencil -> sh
    go StencilRunit3 = Z :. 1
    go StencilRunit5 = Z :. 2
    go StencilRunit7 = Z :. 3
    go StencilRunit9 = Z :. 4
    --
    go (StencilRtup3 a b c            ) = foldl1 union [go a, go b, go c] :. 1
    go (StencilRtup5 a b c d e        ) = foldl1 union [go a, go b, go c, go d, go e] :. 2
    go (StencilRtup7 a b c d e f g    ) = foldl1 union [go a, go b, go c, go d, go e, go f, go g] :. 3
    go (StencilRtup9 a b c d e f g h i) = foldl1 union [go a, go b, go c, go d, go e, go f, go g, go h, go i] :. 4


stencil1Op
    :: forall aenv sh a b stencil. (Shape sh, Elt a, Elt b)
    => StencilR sh a stencil
    -> ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Array sh a
    -> LLVM Native (Array sh b)
stencil1Op s exe gamma aenv stream arr = withExecutable exe $ \nativeExecutable -> do
  Native{..} <- gets llvmTarget
  liftIO $ do
    out <- allocateArray (shape arr)
    let
      start' = boundaryThickness s
      end'   = listToShape (zipWith (-) (shapeToList $ shape arr) (shapeToList start'))
    executeOpMultiDimensional defaultLargePPT fillP (nativeExecutable !# "stencil1_inner") gamma aenv start' end' out
    executeOp 1 fillP (nativeExecutable !# "stencil1_boundary") gamma aenv (IE 0 (2 * rank (undefined::sh))) (reverse $ shapeToList start', out)

    return out


stencil2Op
    :: forall aenv sh a b c stencil1 stencil2. (Shape sh, Elt a, Elt b, Elt c)
    => StencilR sh a stencil1
    -> StencilR sh b stencil2
    -> ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Array sh a
    -> Array sh b
    -> LLVM Native (Array sh c)
stencil2Op s1 s2 exe gamma aenv stream arr brr = withExecutable exe $ \nativeExecutable -> do
  Native{..} <- gets llvmTarget
  liftIO $ do
    out <- allocateArray (shape arr `intersect` shape brr)
    let
      start' = boundaryThickness s1 `intersect` boundaryThickness s2
      end'   = rangeToShape (start', (shape arr))
    executeOpMultiDimensional defaultLargePPT fillP (nativeExecutable !# "stencil2_inner") gamma aenv start' end' out
    executeOp 1 fillS (nativeExecutable !# "stencil2_boundary") gamma aenv (IE 0 (2 * rank (undefined::sh))) (reverse $ shapeToList start', out)

    return out
--}


{-# INLINE stencil2Op #-}
stencil2Op
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> sh
    -> Par Native (Future (Array sh e))
stencil2Op exe gamma aenv sh1 sh2 =
  simpleOp exe gamma aenv (sh1 `intersect` sh2)


{-# INLINE aforeignOp #-}
aforeignOp
    :: (Arrays as, Arrays bs)
    => String
    -> (as -> Par Native (Future bs))
    -> as
    -> Par Native (Future bs)
aforeignOp name asm arr = do
  wallBegin <- liftIO getMonotonicTime
  result    <- Debug.timed Debug.dump_exec (\wall cpu -> printf "exec: %s %s" name (Debug.elapsedP wall cpu)) (asm arr)
  wallEnd   <- liftIO getMonotonicTime
  liftIO $ Debug.addProcessorTime Debug.Native (wallEnd - wallBegin)
  return result


-- Skeleton execution
-- ------------------

(!#) :: Lifetime FunctionTable -> ShortByteString -> Function
(!#) exe name
  = fromMaybe ($internalError "lookupFunction" ("function not found: " ++ S8.unpack name))
  $ lookupFunction name exe

lookupFunction :: ShortByteString -> Lifetime FunctionTable -> Maybe Function
lookupFunction name nativeExecutable = do
  find (\(n,_) -> n == name) (functionTable (unsafeGetValue nativeExecutable))

andThen :: (Maybe a -> t) -> a -> t
andThen f g = f (Just g)


{-# SPECIALISE scheduleOp :: Marshalable (Par Native) args => Function -> Gamma aenv -> Val aenv -> DIM0 -> args -> Maybe Action -> Par Native () #-}
{-# SPECIALISE scheduleOp :: Marshalable (Par Native) args => Function -> Gamma aenv -> Val aenv -> DIM1 -> args -> Maybe Action -> Par Native () #-}
scheduleOp
    :: forall sh aenv args. (Shape sh, Marshalable IO sh, Marshalable (Par Native) args)
    => Function
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> args
    -> Maybe Action
    -> Par Native ()
scheduleOp fun gamma aenv sz args done = do
  Native{..} <- gets llvmTarget
  let
      splits  = numWorkers workers
      minsize = case rank (undefined::sh) of
                  1 -> 4096
                  2 -> 64
                  _ -> 16
  --
  scheduleOpWith splits minsize fun gamma aenv sz args done

-- Schedule an operation over the entire iteration space, specifying the number
-- of partitions and minimum dimension size.
--
{-# SPECIALISE scheduleOpWith :: Marshalable (Par Native) args => Int -> Int -> Function -> Gamma aenv -> Val aenv -> DIM0 -> args -> Maybe Action -> Par Native () #-}
{-# SPECIALISE scheduleOpWith :: Marshalable (Par Native) args => Int -> Int -> Function -> Gamma aenv -> Val aenv -> DIM1 -> args -> Maybe Action -> Par Native () #-}
scheduleOpWith
    :: (Shape sh, Marshalable IO sh, Marshalable (Par Native) args)
    => Int            -- # subdivisions (hint)
    -> Int            -- minimum size of a dimension (must be a power of two)
    -> Function       -- function to execute
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> args
    -> Maybe Action   -- run after the last piece completes
    -> Par Native ()
scheduleOpWith splits minsize fun gamma aenv sz args done = do
  Native{..} <- gets llvmTarget
  job        <- mkJob splits minsize fun gamma aenv empty sz args done
  liftIO $ schedule workers job

{-# SPECIALISE scheduleOpUsing :: Marshalable (Par Native) args => Seq (Int, DIM0, DIM0) -> Function -> Gamma aenv -> Val aenv -> args -> Maybe Action -> Par Native () #-}
{-# SPECIALISE scheduleOpUsing :: Marshalable (Par Native) args => Seq (Int, DIM1, DIM1) -> Function -> Gamma aenv -> Val aenv -> args -> Maybe Action -> Par Native () #-}
scheduleOpUsing
    :: (Shape sh, Marshalable IO sh, Marshalable (Par Native) args)
    => Seq (Int, sh, sh)
    -> Function
    -> Gamma aenv
    -> Val aenv
    -> args
    -> Maybe Action
    -> Par Native ()
scheduleOpUsing ranges fun gamma aenv args jobDone = do
  Native{..} <- gets llvmTarget
  job        <- mkJobUsing ranges fun gamma aenv args jobDone
  liftIO $ schedule workers job

{-# SPECIALISE mkJob :: Marshalable (Par Native) args => Int -> Int -> Function -> Gamma aenv -> Val aenv -> DIM0 -> DIM0 -> args -> Maybe Action -> Par Native Job #-}
{-# SPECIALISE mkJob :: Marshalable (Par Native) args => Int -> Int -> Function -> Gamma aenv -> Val aenv -> DIM1 -> DIM1 -> args -> Maybe Action -> Par Native Job #-}
mkJob :: (Shape sh, Marshalable IO sh, Marshalable (Par Native) args)
      => Int
      -> Int
      -> Function
      -> Gamma aenv
      -> Val aenv
      -> sh
      -> sh
      -> args
      -> Maybe Action
      -> Par Native Job
mkJob splits minsize fun gamma aenv from to args jobDone =
  mkJobUsing (divideWork splits minsize from to (,,)) fun gamma aenv args jobDone

{-# SPECIALISE mkJobUsing :: Marshalable (Par Native) args => Seq (Int, DIM0, DIM0) -> Function -> Gamma aenv -> Val aenv -> args -> Maybe Action -> Par Native Job #-}
{-# SPECIALISE mkJobUsing :: Marshalable (Par Native) args => Seq (Int, DIM1, DIM1) -> Function -> Gamma aenv -> Val aenv -> args -> Maybe Action -> Par Native Job #-}
mkJobUsing
      :: (Shape sh, Marshalable IO sh, Marshalable (Par Native) args)
      => Seq (Int, sh, sh)
      -> Function
      -> Gamma aenv
      -> Val aenv
      -> args
      -> Maybe Action
      -> Par Native Job
mkJobUsing ranges (name, f) gamma aenv args jobDone = do
  argv <- marshal' (Proxy::Proxy Native) (args, (gamma, aenv))
  let
      jobTasks = flip fmap ranges $ \(_,u,v) -> do
                  sched $ printf "%s (%s) -> (%s)" (S8.unpack name) (showShape u) (showShape v)
                  callFFI f retVoid =<< marshal (Proxy::Proxy Native) (u, v, argv)
  --
  liftIO $ timed name Job {..}

{-# SPECIALISE mkJobUsingIndex :: Marshalable (Par Native) args => Seq (Int, DIM0, DIM0) -> Function -> Gamma aenv -> Val aenv -> args -> Maybe Action -> Par Native Job #-}
{-# SPECIALISE mkJobUsingIndex :: Marshalable (Par Native) args => Seq (Int, DIM1, DIM1) -> Function -> Gamma aenv -> Val aenv -> args -> Maybe Action -> Par Native Job #-}
mkJobUsingIndex
      :: (Shape sh, Marshalable IO sh, Marshalable (Par Native) args)
      => Seq (Int, sh, sh)
      -> Function
      -> Gamma aenv
      -> Val aenv
      -> args
      -> Maybe Action
      -> Par Native Job
mkJobUsingIndex ranges (name, f) gamma aenv args jobDone = do
  argv <- marshal' (Proxy::Proxy Native) (args, (gamma, aenv))
  let
      jobTasks = flip fmap ranges $ \(i,u,v) -> do
                  sched $ printf "%s (%s) -> (%s)" (S8.unpack name) (showShape u) (showShape v)
                  callFFI f retVoid =<< marshal (Proxy::Proxy Native) (u, v, i, argv)
  --
  liftIO $ timed name Job {..}


{--
executeOpMultiDimensional
  :: forall sh args aenv. (Marshalable args, Shape sh)
  => Int
  -> Executable
  -> Function
  -> Gamma aenv
  -> Aval aenv
  -> sh
  -> sh
  -> args
  -> IO ()
executeOpMultiDimensional ppt exe (name, f) gamma aenv start end args =
  case rank (undefined::sh) of
    0 -> do
      runExecutable exe name ppt (IE 0 1) $ \_s _e _tid -> do
        monitorProcTime $
          callFFI f retVoid =<< marshal (undefined::Native) () (args, (gamma, aenv))
    _ -> do
      let range = IE (innermost start) (innermost end)
      runExecutable exe name ppt range $ \s e _tid -> do
        let
          start' = changeInnermost start s
          end'   = changeInnermost end   e
        monitorProcTime $
          callFFI f retVoid =<< marshal (undefined::Native) ()
            (reverse $ shapeToList start', reverse $ shapeToList end', args, (gamma, aenv))

  where
    innermost :: Shape sh => sh -> Int
    innermost sh = go (eltType (undefined::sh)) (fromElt sh)
      where
        go :: TupleType t -> t -> Int
        go TypeRunit () = 0
        go (TypeRpair TypeRunit (TypeRscalar s)) ((), n)
          | Just Refl <- matchScalarType s (scalarType :: ScalarType Int) = n
        go (TypeRpair tt _) (dims, _) = go tt dims

    changeInnermost :: Shape sh => sh -> Int -> sh
    changeInnermost sh = toElt . go (eltType (undefined::sh)) (fromElt sh)
      where
        go :: TupleType t -> t -> Int -> t
        go TypeRunit () _ = ()
        go (TypeRpair tt (TypeRscalar s)) (dims, d) n
          | Just Refl <- matchScalarType s (scalarType :: ScalarType Int)
          = case tt of
            TypeRunit -> ((), n)
            _         -> (go tt dims n, d)
--}


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

