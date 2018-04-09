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

  executeAcc, executeAfun,
  executeOpenAcc

) where

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Analysis.Match
import Data.Array.Accelerate.LLVM.Execute
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.Native.Array.Data
import Data.Array.Accelerate.LLVM.Native.Link
import Data.Array.Accelerate.LLVM.Native.Execute.Async
import Data.Array.Accelerate.LLVM.Native.Execute.Environment
import Data.Array.Accelerate.LLVM.Native.Execute.Marshal
import Data.Array.Accelerate.LLVM.Native.Target
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

-- Use work-stealing scheduler
import Data.Range                                                   ( Range(..) )
import Control.Parallel.Meta                                        ( Executable(..) )
import Data.Array.Accelerate.LLVM.Native.Execute.LBS

-- library
import Control.Monad.State                                          ( gets )
import Control.Monad.Trans                                          ( liftIO )
import Data.ByteString.Short                                        ( ShortByteString )
import Data.List                                                    ( find )
import Data.Maybe                                                   ( fromMaybe )
import Data.Time.Clock                                              ( getCurrentTime, diffUTCTime )
import Data.Word                                                    ( Word8 )
import Text.Printf                                                  ( printf )
import Prelude                                                      hiding ( map, sum, scanl, scanr, init )
import qualified Data.ByteString.Short.Char8                        as S8
import qualified Prelude                                            as P

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
  generate      = simpleNamedNestedLoops "generateNested"
  transform     = simpleNamedNestedLoops "generateNested"
  backpermute   = simpleNamedNestedLoops "generateNested"
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
  permute       = permuteOpNested
  stencil1      = stencil1Op
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
    -> Aval aenv
    -> Stream
    -> sh
    -> LLVM Native (Array sh e)
simpleOp exe gamma aenv () sh = withExecutable exe $ \nativeExecutable -> do
  let fun = case functionTable nativeExecutable of
              f:_ -> f
              _   -> $internalError "simpleOp" "no functions found"
  --
  Native{..} <- gets llvmTarget
  liftIO $ do
    out <- allocateArray sh
    executeOp defaultLargePPT fillP fun gamma aenv (IE 0 (size sh)) out
    return out

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

--
simpleNamed
    :: (Shape sh, Elt e)
    => ShortByteString
    -> ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> LLVM Native (Array sh e)
simpleNamed name exe gamma aenv () sh = withExecutable exe $ \nativeExecutable -> do
  Native{..} <- gets llvmTarget
  liftIO $ do
    out <- allocateArray sh
    executeOp defaultLargePPT fillP (nativeExecutable !# name) gamma aenv (IE 0 (size sh)) out
    return out

--
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
foldAllOp exe gamma aenv () (Z :. sz) = withExecutable exe $ \nativeExecutable -> do
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
      executeOp 1 fillS (nativeExecutable !# "foldAllS") gamma aenv (IE 0 sz) out
      return out

    else liftIO $ do
      -- Parallel reduction
      out <- allocateArray Z
      tmp <- allocateArray (Z :. steps) :: IO (Vector e)
      executeOp 1 fillP (nativeExecutable !# "foldAllP1") gamma aenv (IE 0 steps) (sz, stride, tmp)
      executeOp 1 fillS (nativeExecutable !# "foldAllP2") gamma aenv (IE 0 steps) (tmp, out)
      return out

foldDimOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM Native (Array sh e)
foldDimOp exe gamma aenv () (sh :. sz) = withExecutable exe $ \nativeExecutable -> do
  Native{..} <- gets llvmTarget
  let ppt = defaultSmallPPT `max` (defaultLargePPT `quot` (max 1 sz))
  liftIO $ do
    out <- allocateArray sh
    executeOp ppt fillP (nativeExecutable !# "fold") gamma aenv (IE 0 (size sh)) (sz, out)
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


aforeignOp
    :: (Arrays as, Arrays bs)
    => String
    -> (Stream -> as -> LLVM Native bs)
    -> Stream
    -> as
    -> LLVM Native bs
aforeignOp name asm stream arr = do
  wallBegin <- liftIO $ getCurrentTime
  result    <- Debug.timed Debug.dump_exec (\wall cpu -> printf "exec: %s %s" name (Debug.elapsedP wall cpu)) (asm stream arr)
  wallEnd   <- liftIO $ getCurrentTime
  liftIO $ Debug.addProcessorTime Debug.Native (realToFrac (diffUTCTime wallEnd wallBegin))
  return result


-- Skeleton execution
-- ------------------

(!#) :: FunctionTable -> ShortByteString -> Function
(!#) exe name
  = fromMaybe ($internalError "lookupFunction" ("function not found: " ++ S8.unpack name))
  $ lookupFunction name exe

lookupFunction :: ShortByteString -> FunctionTable -> Maybe Function
lookupFunction name nativeExecutable = do
  find (\(n,_) -> n == name) (functionTable nativeExecutable)

-- Execute the given function distributed over the available threads.
--
executeOp
    :: Marshalable args
    => Int
    -> Executable
    -> Function
    -> Gamma aenv
    -> Aval aenv
    -> Range
    -> args
    -> IO ()
executeOp ppt exe (name, f) gamma aenv r args = do
  args' <- marshal (undefined::Native) () (args, (gamma, aenv))
  --
  runExecutable exe name ppt r $ \start end _tid -> do
   monitorProcTime             $
    callFFI f retVoid (argInt start : argInt end : args')


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

