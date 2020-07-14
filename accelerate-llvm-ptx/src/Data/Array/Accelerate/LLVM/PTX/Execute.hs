{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute (

  executeAcc,
  executeOpenAcc,

) where

import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Execute

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch           ( multipleOf )
import Data.Array.Accelerate.LLVM.PTX.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Array.Prim                ( memsetArrayAsync )
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import Data.Array.Accelerate.LLVM.PTX.Execute.Environment
import Data.Array.Accelerate.LLVM.PTX.Execute.Marshal
import Data.Array.Accelerate.LLVM.PTX.Execute.Stream            ( Stream )
import Data.Array.Accelerate.LLVM.PTX.Link
import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug
import qualified Data.Array.Accelerate.LLVM.PTX.Execute.Event   as Event

import qualified Foreign.CUDA.Driver                            as CUDA

import Control.Monad                                            ( when, forM_ )
import Control.Monad.Reader                                     ( asks, local )
import Control.Monad.State                                      ( liftIO )
import Data.ByteString.Short.Char8                              ( ShortByteString, unpack )
import qualified Data.DList                                     as DL
import Data.List                                                ( find )
import Data.Maybe                                               ( fromMaybe )
import Text.Printf                                              ( printf )
import Prelude                                                  hiding ( exp, map, sum, scanl, scanr )


{-# SPECIALISE INLINE executeAcc     :: ExecAcc     PTX      a ->             Par PTX (FutureArraysR PTX a) #-}
{-# SPECIALISE INLINE executeOpenAcc :: ExecOpenAcc PTX aenv a -> Val aenv -> Par PTX (FutureArraysR PTX a) #-}

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

-- Simple kernels just need to know the shape of the output array
--
{-# INLINE simpleOp #-}
simpleOp
    :: HasCallStack
    => ShortByteString
    -> ArrayR (Array sh e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Par PTX (Future (Array sh e))
simpleOp name repr exe gamma aenv sh =
  withExecutable exe $ \ptxExecutable -> do
    future <- new
    result <- allocateRemote repr sh
    --
    let paramR = TupRsingle $ ParamRarray repr
    executeOp (ptxExecutable !# name) gamma aenv (arrayRshape repr) sh paramR result
    put future result
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
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Array sh a
    -> Par PTX (Future (Array sh b))
mapOp inplace repr tp exe gamma aenv input@(shape -> sh) =
  withExecutable exe $ \ptxExecutable -> do
    let reprOut = ArrayR (arrayRshape repr) tp
    future <- new
    result <- case inplace of
                Just Refl -> return input
                Nothing   -> allocateRemote reprOut sh
    --
    let paramsR = TupRsingle (ParamRarray reprOut) `TupRpair` TupRsingle (ParamRarray repr)
    executeOp (ptxExecutable !# "map") gamma aenv (arrayRshape repr) sh paramsR (result, input)
    put future result
    return future

{-# INLINE generateOp #-}
generateOp
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Par PTX (Future (Array sh e))
generateOp = simpleOp "generate"

{-# INLINE transformOp #-}
transformOp
    :: HasCallStack
    => ArrayR (Array sh a)
    -> ArrayR (Array sh' b)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh'
    -> Array sh a
    -> Par PTX (Future (Array sh' b))
transformOp repr repr' exe gamma aenv sh' input =
  withExecutable exe $ \ptxExecutable -> do
    future <- new
    result <- allocateRemote repr' sh'
    let paramsR = TupRsingle (ParamRarray repr') `TupRpair` TupRsingle (ParamRarray repr)
    executeOp (ptxExecutable !# "transform") gamma aenv (arrayRshape repr') sh' paramsR (result, input)
    put future result
    return future

{-# INLINE backpermuteOp #-}
backpermuteOp
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ShapeR sh'
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh'
    -> Array sh e
    -> Par PTX (Future (Array sh' e))
backpermuteOp (ArrayR shr tp) shr' = transformOp (ArrayR shr tp) (ArrayR shr' tp)

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
{-# INLINE fold1Op #-}
fold1Op
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array sh e))
fold1Op repr exe gamma aenv arr@(delayedShape -> sh@(sx, sz))
  = boundsCheck "empty array" (sz > 0)
  $ case size (ShapeRsnoc $ arrayRshape repr) sh of
      0 -> newFull =<< allocateRemote repr sx  -- empty, but possibly with one or more non-zero dimensions
      _ -> foldCore repr exe gamma aenv arr

{-# INLINE foldOp #-}
foldOp
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array sh e))
foldOp repr exe gamma aenv arr@(delayedShape -> sh@(sx, _))
  = case size (ShapeRsnoc $ arrayRshape repr) sh of
      0 -> generateOp repr exe gamma aenv sx
      _ -> foldCore repr exe gamma aenv arr

{-# INLINE foldCore #-}
foldCore
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array sh e))
foldCore repr exe gamma aenv arr
  | ArrayR ShapeRz tp <- repr
  = foldAllOp tp exe gamma aenv arr
  --
  | otherwise
  = foldDimOp repr exe gamma aenv arr

{-# INLINE foldAllOp #-}
foldAllOp
    :: forall aenv e. HasCallStack
    => TypeR e
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Vector e)
    -> Par PTX (Future (Scalar e))
foldAllOp tp exe gamma aenv input =
  withExecutable exe $ \ptxExecutable -> do
    future <- new
    let
        ks        = ptxExecutable !# "foldAllS"
        km1       = ptxExecutable !# "foldAllM1"
        km2       = ptxExecutable !# "foldAllM2"
        sh@((), n) = delayedShape input
        paramsRinput = TupRsingle $ ParamRmaybe $ ParamRarray $ ArrayR dim1 tp
        paramsRdim0  = TupRsingle $ ParamRarray $ ArrayR dim0 tp
        paramsRdim1  = TupRsingle $ ParamRarray $ ArrayR dim1 tp
    --
    if kernelThreadBlocks ks n == 1
      then do
        -- The array is small enough that we can compute it in a single step
        result <- allocateRemote (ArrayR dim0 tp) ()
        let paramsR = paramsRdim0 `TupRpair` paramsRinput
        executeOp ks gamma aenv dim1 sh paramsR (result, manifest input)
        put future result

      else do
        -- Multi-kernel reduction to a single element. The first kernel integrates
        -- any delayed elements, and the second is called recursively until
        -- reaching a single element.
        let
            rec :: Vector e -> Par PTX ()
            rec tmp@(Array ((),m) adata)
              | m <= 1    = put future (Array () adata)
              | otherwise = do
                  let sh' = ((), m `multipleOf` kernelThreadBlockSize km2)
                  out <- allocateRemote (ArrayR dim1 tp) sh'
                  let paramsR2 = paramsRdim1 `TupRpair` paramsRdim1
                  executeOp km2 gamma aenv dim1 sh' paramsR2 (tmp, out)
                  rec out
        --
        let sh' = ((), n `multipleOf` kernelThreadBlockSize km1)
        tmp <- allocateRemote (ArrayR dim1 tp) sh'
        let paramsR1 = paramsRdim1 `TupRpair` paramsRinput
        executeOp km1 gamma aenv dim1 sh' paramsR1 (tmp, manifest input)
        rec tmp
    --
    return future


{-# INLINE foldDimOp #-}
foldDimOp
    :: HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array sh e))
foldDimOp repr@(ArrayR shr tp) exe gamma aenv input@(delayedShape -> (sh, sz))
  | sz == 0   = generateOp repr exe gamma aenv sh
  | otherwise =
    withExecutable exe $ \ptxExecutable -> do
      future <- new
      result <- allocateRemote repr sh
      --
      let paramsR = TupRsingle (ParamRarray repr) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray $ ArrayR (ShapeRsnoc shr) tp)
      executeOp (ptxExecutable !# "fold") gamma aenv shr sh paramsR (result, manifest input)
      put future result
      return future


{-# INLINE foldSegOp #-}
foldSegOp
    :: HasCallStack
    => IntegralType i
    -> ArrayR (Array (sh, Int) e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Delayed (Segments i)
    -> Par PTX (Future (Array (sh, Int) e))
foldSegOp intTp repr exe gamma aenv input@(delayedShape -> (sh, sz)) segments@(delayedShape -> ((), ss)) =
  withExecutable exe $ \ptxExecutable -> do
    let
        ArrayR (ShapeRsnoc shr') _ = repr
        reprSeg = ArrayR dim1 $ TupRsingle $ SingleScalarType $ NumSingleType $ IntegralNumType intTp
        n       = ss - 1  -- segments array has been 'scanl (+) 0'`ed
        m       = size shr' sh * n
        foldseg = if (sz`quot`ss) < (2 * kernelThreadBlockSize foldseg_cta)
                    then foldseg_warp
                    else foldseg_cta
        --
        foldseg_cta   = ptxExecutable !# "foldSeg_block"
        foldseg_warp  = ptxExecutable !# "foldSeg_warp"
        -- qinit         = ptxExecutable !# "qinit"
    --
    future  <- new
    result  <- allocateRemote repr (sh, n)
    let paramsR = TupRsingle (ParamRarray repr) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray reprSeg)
    executeOp foldseg gamma aenv dim1 ((), m) paramsR ((result, manifest input), manifest segments)
    put future result
    return future


{-# INLINE scanOp #-}
scanOp
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array (sh, Int) e))
scanOp repr exe gamma aenv input@(delayedShape -> (sz, n)) =
  case n of
    0 -> generateOp repr exe gamma aenv (sz, 1)
    _ -> scanCore repr exe gamma aenv (n+1) input

{-# INLINE scan1Op #-}
scan1Op
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array (sh, Int) e))
scan1Op repr exe gamma aenv input@(delayedShape -> (_, n))
  = boundsCheck "empty array" (n > 0)
  $ scanCore repr exe gamma aenv n input

{-# INLINE scanCore #-}
scanCore
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Int                    -- output size of innermost dimension
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array (sh, Int) e))
scanCore repr exe gamma aenv m input
  | ArrayR (ShapeRsnoc ShapeRz) tp <- repr
  = scanAllOp tp exe gamma aenv m input
  --
  | otherwise
  = scanDimOp repr exe gamma aenv m input

{-# INLINE scanAllOp #-}
scanAllOp
    :: HasCallStack
    => TypeR e
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Int                    -- output size
    -> Delayed (Vector e)
    -> Par PTX (Future (Vector e))
scanAllOp tp exe gamma aenv m input@(delayedShape -> ((), n)) =
  withExecutable exe $ \ptxExecutable -> do
    let
        k1  = ptxExecutable !# "scanP1"
        k2  = ptxExecutable !# "scanP2"
        k3  = ptxExecutable !# "scanP3"
        --
        c   = kernelThreadBlockSize k1
        s   = n `multipleOf` c
        --
        repr = ArrayR dim1 tp
        paramR = TupRsingle $ ParamRarray repr
        paramsR1 = paramR `TupRpair` paramR `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
        paramsR3 = paramR `TupRpair` paramR `TupRpair` TupRsingle ParamRint
    --
    future  <- new
    result  <- allocateRemote repr ((), m)

    -- Step 1: Independent thread-block-wide scans of the input. Small arrays
    -- which can be computed by a single thread block will require no
    -- additional work.
    tmp     <- allocateRemote repr ((), s)
    executeOp k1 gamma aenv dim1 ((), s) paramsR1 ((tmp, result), manifest input)

    -- Step 2: Multi-block reductions need to compute the per-block prefix,
    -- then apply those values to the partial results.
    when (s > 1) $ do
      executeOp k2 gamma aenv dim1 ((), s)   paramR tmp
      executeOp k3 gamma aenv dim1 ((), s-1) paramsR3 ((tmp, result), c)

    put future result
    return future

{-# INLINE scanDimOp #-}
scanDimOp
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Int
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array (sh, Int) e))
scanDimOp repr exe gamma aenv m input@(delayedShape -> (sz, _)) =
  withExecutable exe $ \ptxExecutable -> do
    let ArrayR (ShapeRsnoc shr') _ = repr
    future  <- new
    result  <- allocateRemote repr (sz, m)
    let paramsR = TupRsingle (ParamRarray repr) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
    executeOp (ptxExecutable !# "scan") gamma aenv dim1 ((), size shr' sz) paramsR (result, manifest input)
    put future result
    return future


{-# INLINE scan'Op #-}
scan'Op
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array (sh, Int) e, Array sh e))
scan'Op repr exe gamma aenv input@(delayedShape -> (sz, n)) =
  case n of
    0 -> do
      future  <- new
      result  <- allocateRemote repr (sz, 0)
      sums    <- generateOp (reduceRank repr) exe gamma aenv sz
      fork $ do sums' <- get sums
                put future (result, sums')
      return future
    --
    _ -> scan'Core repr exe gamma aenv input

{-# INLINE scan'Core #-}
scan'Core
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array (sh, Int) e, Array sh e))
scan'Core repr exe gamma aenv input
  | ArrayR (ShapeRsnoc ShapeRz) tp <- repr
  = scan'AllOp tp exe gamma aenv input
  --
  | otherwise
  = scan'DimOp repr exe gamma aenv input

{-# INLINE scan'AllOp #-}
scan'AllOp
    :: HasCallStack
    => TypeR e
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Vector e)
    -> Par PTX (Future (Vector e, Scalar e))
scan'AllOp tp exe gamma aenv input@(delayedShape -> ((), n)) =
  withExecutable exe $ \ptxExecutable -> do
    let
        repr = ArrayR dim1 tp
        paramRdim0 = TupRsingle $ ParamRarray $ ArrayR dim0 tp
        paramRdim1 = TupRsingle $ ParamRarray repr
        k1  = ptxExecutable !# "scanP1"
        k2  = ptxExecutable !# "scanP2"
        k3  = ptxExecutable !# "scanP3"
        --
        c   = kernelThreadBlockSize k1
        s   = n `multipleOf` c
    --
    future  <- new
    result  <- allocateRemote repr ((), n)
    tmp     <- allocateRemote repr ((), s)

    -- Step 1: independent thread-block-wide scans. Each block stores its partial
    -- sum to a temporary array.
    let paramsR1 = paramRdim1 `TupRpair` paramRdim1 `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
    executeOp k1 gamma aenv dim1 ((), s) paramsR1 ((tmp, result), manifest input)

    -- If this was a small array that was processed by a single thread block then
    -- we are done, otherwise compute the per-block prefix and apply those values
    -- to the partial results.
    if s == 1
      then
        case tmp of
          Array _ ad -> put future (result, Array () ad)

      else do
        sums <- allocateRemote (ArrayR dim0 tp) ()
        let paramsR2 = paramRdim1 `TupRpair` paramRdim0
        let paramsR3 = paramRdim1 `TupRpair` paramRdim1 `TupRpair` TupRsingle ParamRint
        executeOp k2 gamma aenv dim1 ((), s)   paramsR2 (tmp, sums)
        executeOp k3 gamma aenv dim1 ((), s-1) paramsR3 ((tmp, result), c)
        put future (result, sums)
    --
    return future

{-# INLINE scan'DimOp #-}
scan'DimOp
    :: HasCallStack
    => ArrayR (Array (sh, Int) e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array (sh, Int) e)
    -> Par PTX (Future (Array (sh, Int) e, Array sh e))
scan'DimOp repr@(ArrayR (ShapeRsnoc shr') _) exe gamma aenv input@(delayedShape -> sh@(sz, _)) =
  withExecutable exe $ \ptxExecutable -> do
    future  <- new
    result  <- allocateRemote repr sh
    sums    <- allocateRemote (reduceRank repr) sz
    let paramsR = TupRsingle (ParamRarray repr) `TupRpair` TupRsingle (ParamRarray $ reduceRank repr) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray repr)
    executeOp (ptxExecutable !# "scan") gamma aenv dim1 ((), size shr' sz) paramsR ((result, sums), manifest input)
    put future (result, sums)
    return future


{-# INLINE permuteOp #-}
permuteOp
    :: HasCallStack
    => Bool
    -> ArrayR (Array sh e)
    -> ShapeR sh'
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Array sh' e
    -> Delayed (Array sh e)
    -> Par PTX (Future (Array sh' e))
permuteOp inplace repr@(ArrayR shr tp) shr' exe gamma aenv defaults@(shape -> shOut) input@(delayedShape -> shIn) =
  withExecutable exe $ \ptxExecutable -> do
    let
        n        = size shr  shIn
        m        = size shr' shOut
        repr'    = ArrayR shr' tp
        reprLock = ArrayR dim1 $ TupRsingle $ scalarTypeWord32
        paramR   = TupRsingle $ ParamRmaybe $ ParamRarray repr
        paramR'  = TupRsingle $ ParamRarray repr'
        kernel   = case functionTable ptxExecutable of
                      k:_ -> k
                      _   -> internalError "no kernels found"
    --
    future  <- new
    result  <- if inplace
                 then Debug.trace Debug.dump_exec "exec: permute/inplace" $ return defaults
                 else Debug.trace Debug.dump_exec "exec: permute/clone"   $ get =<< cloneArrayAsync repr' defaults
    --
    case kernelName kernel of
      -- execute directly using atomic operations
      "permute_rmw"   ->
        let paramsR = paramR' `TupRpair` paramR
        in  executeOp kernel gamma aenv dim1 ((), n) paramsR (result, manifest input)

      -- a temporary array is required for spin-locks around the critical section
      "permute_mutex" -> do
        barrier     <- new :: Par PTX (Future (Vector Word32))
        Array _ ad  <- allocateRemote reprLock ((), m)
        fork $ do fill <- memsetArrayAsync (NumSingleType $ IntegralNumType TypeWord32) m 0 ad
                  put barrier . Array ((), m) =<< get fill
        --
        let paramsR = paramR' `TupRpair` TupRsingle (ParamRfuture $ ParamRarray reprLock) `TupRpair` paramR
        executeOp kernel gamma aenv dim1 ((), n) paramsR ((result, barrier), manifest input)

      _               -> internalError "unexpected kernel image"
    --
    put future result
    return future


{-# INLINE stencil1Op #-}
stencil1Op
    :: HasCallStack
    => TypeR a
    -> ArrayR (Array sh b)
    -> sh
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array sh a)
    -> Par PTX (Future (Array sh b))
stencil1Op tp repr@(ArrayR shr _) halo exe gamma aenv input@(delayedShape -> sh) =
  stencilCore repr exe gamma aenv halo sh paramsR (manifest input)
  where paramsR = TupRsingle $ ParamRmaybe $ ParamRarray $ ArrayR shr tp

-- Using the defaulting instances for stencil operations (for now).
--
{-# INLINE stencil2Op #-}
stencil2Op
    :: HasCallStack
    => TypeR a
    -> TypeR b
    -> ArrayR (Array sh c)
    -> sh
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Delayed (Array sh a)
    -> Delayed (Array sh b)
    -> Par PTX (Future (Array sh c))
stencil2Op tpA tpB repr@(ArrayR shr _) halo exe gamma aenv input1@(delayedShape -> sh1) input2@(delayedShape -> sh2) =
  stencilCore repr exe gamma aenv halo (intersect (arrayRshape repr) sh1 sh2) paramsR (manifest input1, manifest input2)
  where paramsR = TupRsingle (ParamRmaybe $ ParamRarray $ ArrayR shr tpA) `TupRpair` TupRsingle (ParamRmaybe $ ParamRarray $ ArrayR shr tpB)

{-# INLINE stencilCore #-}
stencilCore
    :: forall aenv sh e params. HasCallStack
    => ArrayR (Array sh e)
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh                       -- border dimensions (i.e. index of first interior element)
    -> sh                       -- output array size
    -> ParamsR PTX params
    -> params
    -> Par PTX (Future (Array sh e))
stencilCore repr@(ArrayR shr _) exe gamma aenv halo shOut paramsR params =
  withExecutable exe $ \ptxExecutable -> do
    let
        inside  = ptxExecutable !# "stencil_inside"
        border  = ptxExecutable !# "stencil_border"

        shIn :: sh
        shIn = trav (\x y -> x - 2*y) shOut halo

        trav :: (Int -> Int -> Int) -> sh -> sh -> sh
        trav f a b = go (arrayRshape repr) a b
          where
            go :: ShapeR t -> t -> t -> t
            go ShapeRz           ()      ()      = ()
            go (ShapeRsnoc shr') (xa,xb) (ya,yb) = (go shr' xa ya, f xb yb)
    --
    future  <- new
    result  <- allocateRemote repr shOut
    parent  <- asks ptxStream

    -- interior (no bounds checking)
    let paramsRinside = TupRsingle (ParamRshape shr) `TupRpair` TupRsingle (ParamRarray repr) `TupRpair` paramsR
    executeOp inside gamma aenv shr shIn paramsRinside ((shIn, result), params)

    -- halo regions (bounds checking)
    -- executed in separate streams so that they might overlap the main stencil
    -- and each other, as individually they will not saturate the device
    forM_ (stencilBorders (arrayRshape repr) shOut halo) $ \(u, v) ->
      fork $ do
        -- launch in a separate stream
        let sh = trav (-) v u
        let paramsRborder = TupRsingle (ParamRshape shr) `TupRpair` TupRsingle (ParamRshape shr)
                              `TupRpair` TupRsingle (ParamRarray repr)
                              `TupRpair` paramsR
        executeOp border gamma aenv shr sh paramsRborder (((u, sh), result), params)

        -- synchronisation with main stream
        child <- asks ptxStream
        event <- liftPar (Event.waypoint child)
        ready <- liftIO  (Event.query event)
        if ready then return ()
                 else liftIO (Event.after event parent)

    put future result
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
    -> [(sh, sh)]
stencilBorders shr sh halo = [ face i | i <- [0 .. (2 * rank shr - 1)] ]
  where
    face :: Int -> (sh, sh)
    face n = go n shr sh halo

    go :: Int -> ShapeR t -> t -> t -> (t, t)
    go _ ShapeRz           ()         ()         = ((), ())
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


-- Foreign functions
--
{-# INLINE aforeignOp #-}
aforeignOp
    :: HasCallStack
    => String
    -> ArraysR as
    -> ArraysR bs
    -> (as -> Par PTX (Future bs))
    -> as
    -> Par PTX (Future bs)
aforeignOp name _ _ asm arr = do
  stream <- asks ptxStream
  Debug.monitorProcTime query msg (Just (unsafeGetValue stream)) (asm arr)
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

-- | Retrieve the named kernel
--
(!#) :: HasCallStack => FunctionTable -> ShortByteString -> Kernel
(!#) exe name
  = fromMaybe (internalError ("function not found: " ++ unpack name))
  $ lookupKernel name exe

lookupKernel :: ShortByteString -> FunctionTable -> Maybe Kernel
lookupKernel name ptxExecutable =
  find (\k -> kernelName k == name) (functionTable ptxExecutable)

delayedShape :: Delayed (Array sh e) -> sh
delayedShape (Delayed sh) = sh
delayedShape (Manifest a) = shape a

manifest :: Delayed (Array sh e) -> Maybe (Array sh e)
manifest (Manifest a) = Just a
manifest Delayed{}    = Nothing

-- | Execute some operation with the supplied executable functions
--
withExecutable :: HasCallStack => ExecutableR PTX -> (FunctionTable -> Par PTX b) -> Par PTX b
withExecutable PTXR{..} f =
  local (\(s,_) -> (s,Just ptxExecutable)) $ do
    r <- f (unsafeGetValue ptxExecutable)
    liftIO $ touchLifetime ptxExecutable
    return r


-- Execute the function implementing this kernel.
--
executeOp
    :: HasCallStack
    => Kernel
    -> Gamma aenv
    -> Val aenv
    -> ShapeR sh
    -> sh
    -> ParamsR PTX params
    -> params
    -> Par PTX ()
executeOp kernel gamma aenv shr sh paramsR params =
  let n = size shr sh
  in  when (n > 0) $ do
        stream <- asks ptxStream
        argv   <- marshalParams' @PTX (paramsR `TupRpair` TupRsingle (ParamRenv gamma)) (params, aenv)
        liftIO  $ launch kernel stream n $ DL.toList argv


-- Execute a device function with the given thread configuration and function
-- parameters.
--
launch :: HasCallStack => Kernel -> Stream -> Int -> [CUDA.FunParam] -> IO ()
launch Kernel{..} stream n args =
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

