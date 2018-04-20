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
import Data.Word                                                    ( Word8 )
import System.CPUTime                                               ( getCPUTime )
import Text.Printf                                                  ( printf )
import qualified Data.ByteString.Short.Char8                        as S8
import qualified Data.Sequence                                      as Seq
import qualified Prelude                                            as P
import Prelude                                                      hiding ( map, sum, scanl, scanr, init )

import Foreign.C
import Foreign.LibFFI
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
  -- foldSeg       = foldSegOp
  -- fold1Seg      = foldSegOp
  -- scanl         = scanOp
  -- scanl1        = scan1Op
  -- scanl'        = scan'Op
  -- scanr         = scanOp
  -- scanr1        = scan1Op
  -- scanr'        = scan'Op
  -- permute       = permuteOp
  stencil1      = simpleOp
  stencil2      = stencil2Op
  aforeign      = aforeignOp


-- Skeleton implementation
-- -----------------------

-- Simple kernels just needs to know the shape of the output array.
--
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
  future <- new
  result <- allocateRemote sh
  scheduleOp fun gamma aenv (Z :. size sh) result -- XXX: nested loops
    `andThen` do putIO future result
                 touchLifetime nativeExecutable   -- XXX: must not unload the object code early
  return future


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
  future <- new
  result <- allocateRemote sh
  scheduleOp fun gamma aenv (Z :. size sh) result -- XXX: nested loops
    `andThen` do putIO future result
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
-- 'foldAllOp' in particular must execute specially whether the gang has
-- multiple worker threads which can process the array in parallel.
--

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

foldAllOp
    :: forall aenv e. Elt e
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> DIM1
    -> Par Native (Future (Scalar e))
foldAllOp NativeR{..} gamma aenv sh@(Z :. sz) = do
  Native{..}  <- gets llvmTarget
  future      <- new
  result      <- allocateRemote Z
  let
      minsize = 4096
      splits  = numWorkers workers
      ranges  = divideWork splits minsize empty sh (,)
      steps   = Seq.length ranges
  --
  if steps == 1
    then do
      let (_,f) = nativeExecutable !# "foldAllS"
      --
      argv <- marshal (Proxy::Proxy Native) (0::Int, sz, result, (gamma, aenv))
      job  <- liftIO . timed "foldAllS"
            $ Job { jobTasks = Seq.singleton $ do sched "foldAllS"
                                                  callFFI f retVoid argv
                  , jobDone  = Just $ do putIO future result
                                         touchLifetime nativeExecutable }
      liftIO $ schedule workers job

    else do
      let (_, f1) = nativeExecutable !# "foldAllP1"
          (_, f2) = nativeExecutable !# "foldAllP2"
      --
      tmp   <- allocateRemote (Z :. steps) :: Par Native (Vector e)
      aenv' <- marshal' (Proxy::Proxy Native) (gamma, aenv)
      argv1 <- marshal' (Proxy::Proxy Native) (tmp, aenv')
      argv2 <- marshal  (Proxy::Proxy Native) (0::Int, steps, tmp, result, aenv')

      job2  <- liftIO . timed "foldAllP2"
             $ Job { jobTasks = Seq.singleton $ do sched "foldAllP2"
                                                   callFFI f2 retVoid argv2
                   , jobDone  = Just $ do putIO future result
                                          touchLifetime nativeExecutable }

      job1  <- liftIO . timed "foldAllP1"
             $ Job { jobTasks = flip Seq.mapWithIndex ranges $ \i (u,v) -> do
                                  sched $ printf "foldAllP1 %d -> %d" (size u) (size v)
                                  callFFI f1 retVoid =<< marshal (Proxy::Proxy Native) (u, v, i, argv1)
                   , jobDone  = Just $ schedule workers job2 }

      liftIO $ schedule workers job1

  return future


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
    `andThen` do putIO future result
                 touchLifetime nativeExecutable
  return future


{--
foldSegOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> (Z  :. Int)
    -> LLVM Native (Array (sh :. Int) e)
foldSegOp = error "foldSegOp"
{--
foldSegOp exe gamma aenv () (sh :. _) (Z :. ss) = withExecutable exe $ \nativeExecutable -> do
  Native{..} <- gets llvmTarget
  let
      kernel | segmentOffset  = "foldSegP"
             | otherwise      = "foldSegS"
      n      | segmentOffset  = ss - 1            -- segments array has been 'scanl (+) 0'`ed
             | otherwise      = ss
      ppt    | rank sh == 0   = defaultLargePPT   -- work-steal over the single dimension
             | otherwise      = n                 -- a thread computes all segments along an index
  --
  liftIO $ do
    out <- allocateArray (sh :. n)
    executeOp ppt fillP (nativeExecutable !# kernel) gamma aenv (IE 0 (size (sh :. n))) out
    return out
--}


scanOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
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
    -> Val aenv
    -> sh :. Int
    -> LLVM Native (Array (sh:.Int) e)
scan1Op kernel gamma aenv stream (sz :. n)
  = $boundsCheck "scan1" "empty array" (n > 0)
  $ scanCore kernel gamma aenv stream sz n n

scanCore
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Int
    -> Int
    -> LLVM Native (Array (sh:.Int) e)
scanCore = error "scanCore"
{--
scanCore exe gamma aenv () sz n m = withExecutable exe $ \nativeExecutable -> do
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
      executeOp 1 fillP (nativeExecutable !# "scanS") gamma aenv (IE 0 (size sz)) out
      return out

    else liftIO $ do
      -- parallel one-dimensional scan
      out <- allocateArray (sz :. m)
      tmp <- allocateArray (Z  :. steps) :: IO (Vector e)
      executeOp 1 fillP (nativeExecutable !# "scanP1") gamma aenv (IE 0 steps) (stride, steps', out, tmp)
      executeOp 1 fillS (nativeExecutable !# "scanP2") gamma aenv (IE 0 steps) tmp
      executeOp 1 fillP (nativeExecutable !# "scanP3") gamma aenv (IE 0 steps') (stride, out, tmp)
      return out
--}


scan'Op
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> LLVM Native (Array (sh:.Int) e, Array sh e)
scan'Op = error "scan'Op"
{--
scan'Op native gamma aenv stream sh@(sz :. n) =
  case n of
    0 -> do
      out <- liftIO $ allocateArray (sz :. 0)
      sum <- simpleNamed "generate" native gamma aenv stream sz
      return (out, sum)
    --
    _ -> scan'Core native gamma aenv stream sh
--}

scan'Core
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> LLVM Native (Array (sh:.Int) e, Array sh e)
scan'Core = error "scan'Core"
{--
scan'Core exe gamma aenv () sh@(sz :. n) = withExecutable exe $ \nativeExecutable -> do
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
      executeOp 1 fillP (nativeExecutable !# "scanS") gamma aenv (IE 0 (size sz)) (out,sum)
      return (out,sum)

    else liftIO $ do
      tmp <- allocateArray (Z :. steps) :: IO (Vector e)
      out <- allocateArray sh
      sum <- allocateArray sz
      executeOp 1 fillP (nativeExecutable !# "scanP1") gamma aenv (IE 0 steps)  (stride, steps', out, tmp)
      executeOp 1 fillS (nativeExecutable !# "scanP2") gamma aenv (IE 0 steps)  (sum, tmp)
      executeOp 1 fillP (nativeExecutable !# "scanP3") gamma aenv (IE 0 steps') (stride, out, tmp)
      return (out,sum)
--}


-- Forward permutation, specified by an indexing mapping into an array and a
-- combination function to combine elements.
--
permuteOp
    :: (Shape sh, Shape sh', Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> Bool
    -> sh
    -> Array sh' e
    -> LLVM Native (Array sh' e)
permuteOp = error "permuteOp"
{--
permuteOp exe gamma aenv () inplace shIn dfs = withExecutable exe $ \nativeExecutable -> do
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
      executeOp 1 fillS (nativeExecutable !# "permuteS") gamma aenv (IE 0 n) out

    else liftIO $ do
      -- parallel permutation
      case lookupFunction "permuteP_rmw" nativeExecutable of
        Just f  -> executeOp defaultLargePPT fillP f gamma aenv (IE 0 n) out
        Nothing -> do
          barrier@(Array _ adb) <- allocateArray (Z :. m) :: IO (Vector Word8)
          memset (ptrsOfArrayData adb) 0 m
          executeOp defaultLargePPT fillP (nativeExecutable !# "permuteP_mutex") gamma aenv (IE 0 n) (out, barrier)

  return out
--}
--}


stencil2Op
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> sh
    -> Par Native (Future (Array sh e))
stencil2Op kernel gamma aenv sh1 sh2 =
  simpleOp kernel gamma aenv (sh1 `intersect` sh2)


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
mkJob splits minsize (name, f) gamma aenv from to args jobDone = do
  argv <- marshal' (Proxy::Proxy Native) (args, (gamma, aenv))
  let
      jobTasks = divideWork splits minsize from to $ \u v -> do
                  sched $ printf "%s (%s) -> (%s)" (S8.unpack name) (showShape u) (showShape v)
                  callFFI f retVoid =<< marshal (Proxy::Proxy Native) (u, v, argv)
  --
  liftIO $ timed name Job {..}


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

