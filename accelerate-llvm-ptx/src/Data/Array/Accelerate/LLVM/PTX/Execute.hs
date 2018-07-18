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
-- Copyright   : [2014..2018] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute (

  executeAcc,
  executeOpenAcc,

) where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
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

-- cuda
import qualified Foreign.CUDA.Driver                            as CUDA

-- library
import Control.Monad                                            ( when, forM_ )
import Control.Monad.Reader                                     ( asks, local )
import Control.Monad.State                                      ( liftIO )
import Data.ByteString.Short.Char8                              ( ShortByteString, unpack )
import Data.List                                                ( find )
import Data.Maybe                                               ( fromMaybe )
import Data.Proxy                                               ( Proxy(..) )
import Data.Word                                                ( Word32 )
import Text.Printf                                              ( printf )
import Prelude                                                  hiding ( exp, map, sum, scanl, scanr )


{-# SPECIALISE INLINE executeAcc     :: ExecAcc     PTX      a ->             Par PTX (Future a) #-}
{-# SPECIALISE INLINE executeOpenAcc :: ExecOpenAcc PTX aenv a -> Val aenv -> Par PTX (Future a) #-}

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
  aforeign      = aforeignOp


-- Skeleton implementation
-- -----------------------

-- Simple kernels just need to know the shape of the output array
--
{-# INLINE simpleOp #-}
simpleOp
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Par PTX (Future (Array sh e))
simpleOp exe gamma aenv sh = withExecutable exe $ \ptxExecutable -> do
  let kernel = case functionTable ptxExecutable of
                 k:_ -> k
                 _   -> $internalError "simpleOp" "no kernels found"
  --
  future <- new
  result <- allocateRemote sh
  --
  executeOp kernel gamma aenv sh result
  put future result
  return future

{-# INLINE simpleNamed #-}
simpleNamed
    :: (Shape sh, Elt e)
    => ShortByteString
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Par PTX (Future (Array sh e))
simpleNamed fun exe gamma aenv sh = withExecutable exe $ \ptxExecutable -> do
  future <- new
  result <- allocateRemote sh
  --
  executeOp (ptxExecutable !# fun) gamma aenv sh result
  put future result
  return future


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
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> Par PTX (Future (Array sh e))
fold1Op exe gamma aenv sh@(sx :. sz)
  = $boundsCheck "fold1" "empty array" (sz > 0)
  $ case size sh of
      0 -> newFull =<< allocateRemote sx  -- empty, but possibly with one or more non-zero dimensions
      _ -> foldCore exe gamma aenv sh

{-# INLINE foldOp #-}
foldOp
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> Par PTX (Future (Array sh e))
foldOp exe gamma aenv sh@(sx :. _)
  = case size sh of
      0 -> simpleNamed "generate" exe gamma aenv sx
      _ -> foldCore exe gamma aenv sh

{-# INLINE foldCore #-}
foldCore
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> Par PTX (Future (Array sh e))
foldCore exe gamma aenv sh
  | Just Refl <- matchShapeType @sh @Z
  = foldAllOp exe gamma aenv sh
  --
  | otherwise
  = foldDimOp exe gamma aenv sh

{-# INLINE foldAllOp #-}
foldAllOp
    :: forall aenv e. Elt e
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> DIM1
    -> Par PTX (Future (Scalar e))
foldAllOp exe gamma aenv sh@(Z :. n) = withExecutable exe $ \ptxExecutable -> do
  future <- new
  let
      ks    = ptxExecutable !# "foldAllS"
      km1   = ptxExecutable !# "foldAllM1"
      km2   = ptxExecutable !# "foldAllM2"
  --
  if kernelThreadBlocks ks n == 1
    then do
      -- The array is small enough that we can compute it in a single step
      result <- allocateRemote Z
      executeOp ks gamma aenv sh result
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
                let sh' = Z :. m `multipleOf` kernelThreadBlockSize km2
                out <- allocateRemote sh'
                executeOp km2 gamma aenv sh' (tmp, out)
                rec out
      --
      let sh' = Z :. n `multipleOf` kernelThreadBlockSize km1
      tmp <- allocateRemote sh'
      executeOp km1 gamma aenv sh' tmp
      rec tmp
  --
  return future


{-# INLINE foldDimOp #-}
foldDimOp
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> Par PTX (Future (Array sh e))
foldDimOp exe gamma aenv (sh :. sz)
  | sz > 0    = simpleNamed "fold"     exe gamma aenv sh
  | otherwise = simpleNamed "generate" exe gamma aenv sh


{-# INLINE foldSegOp #-}
foldSegOp
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> (sh :. Int)
    -> (Z  :. Int)
    -> Par PTX (Future (Array (sh :. Int) e))
foldSegOp exe gamma aenv (sh :. sz) (Z :. ss) = withExecutable exe $ \ptxExecutable -> do
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
  future  <- new
  result  <- allocateRemote (sh :. n)
  executeOp foldseg gamma aenv (Z :. m) result
  put future result
  return future


{-# INLINE scanOp #-}
scanOp
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> Par PTX (Future (Array (sh:.Int) e))
scanOp exe gamma aenv (sz :. n) =
  case n of
    0 -> simpleNamed "generate" exe gamma aenv (sz :. 1)
    _ -> scanCore exe gamma aenv sz n (n+1)

{-# INLINE scan1Op #-}
scan1Op
    :: (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> Par PTX (Future (Array (sh:.Int) e))
scan1Op exe gamma aenv (sz :. n)
  = $boundsCheck "scan1" "empty array" (n > 0)
  $ scanCore exe gamma aenv sz n n

{-# INLINE scanCore #-}
scanCore
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Int                    -- input size
    -> Int                    -- output size
    -> Par PTX (Future (Array (sh:.Int) e))
scanCore exe gamma aenv sz n m
  | Just Refl <- matchShapeType @sh @Z
  = scanAllOp exe gamma aenv n m
  --
  | otherwise
  = scanDimOp exe gamma aenv sz m

{-# INLINE scanAllOp #-}
scanAllOp
    :: forall aenv e. Elt e
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> Int                    -- input size
    -> Int                    -- output size
    -> Par PTX (Future (Vector e))
scanAllOp exe gamma aenv n m = withExecutable exe $ \ptxExecutable -> do
  let
      k1  = ptxExecutable !# "scanP1"
      k2  = ptxExecutable !# "scanP2"
      k3  = ptxExecutable !# "scanP3"
      --
      c   = kernelThreadBlockSize k1
      s   = n `multipleOf` c
  --
  future  <- new
  result  <- allocateRemote (Z :. m)

  -- Step 1: Independent thread-block-wide scans of the input. Small arrays
  -- which can be computed by a single thread block will require no
  -- additional work.
  tmp     <- allocateRemote (Z :. s) :: Par PTX (Vector e)
  executeOp k1 gamma aenv (Z :. s) (tmp, result)

  -- Step 2: Multi-block reductions need to compute the per-block prefix,
  -- then apply those values to the partial results.
  when (s > 1) $ do
    executeOp k2 gamma aenv (Z :. s)   tmp
    executeOp k3 gamma aenv (Z :. s-1) (tmp, result, c)

  put future result
  return future

{-# INLINE scanDimOp #-}
scanDimOp
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Int
    -> Par PTX (Future (Array (sh:.Int) e))
scanDimOp exe gamma aenv sz m = withExecutable exe $ \ptxExecutable -> do
  future  <- new
  result  <- allocateRemote (sz :. m)
  executeOp (ptxExecutable !# "scan") gamma aenv (Z :. size sz) result
  put future result
  return future


{-# INLINE scan'Op #-}
scan'Op
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> Par PTX (Future (Array (sh:.Int) e, Array sh e))
scan'Op exe gamma aenv sh@(sz :. n) =
  case n of
    0 -> do
      future  <- new
      result  <- allocateRemote (sz :. 0)
      sums    <- simpleNamed "generate" exe gamma aenv sz
      fork $ do sums' <- get sums
                put future (result, sums')
      return future
    --
    _ -> scan'Core exe gamma aenv sh

{-# INLINE scan'Core #-}
scan'Core
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> Par PTX (Future (Array (sh:.Int) e, Array sh e))
scan'Core exe gamma aenv sh
  | Just Refl <- matchShapeType @sh @Z
  = scan'AllOp exe gamma aenv sh
  --
  | otherwise
  = scan'DimOp exe gamma aenv sh

{-# INLINE scan'AllOp #-}
scan'AllOp
    :: forall aenv e. Elt e
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> DIM1
    -> Par PTX (Future (Vector e, Scalar e))
scan'AllOp exe gamma aenv (Z :. n) = withExecutable exe $ \ptxExecutable -> do
  let
      k1  = ptxExecutable !# "scanP1"
      k2  = ptxExecutable !# "scanP2"
      k3  = ptxExecutable !# "scanP3"
      --
      c   = kernelThreadBlockSize k1
      s   = n `multipleOf` c
  --
  future  <- new
  result  <- allocateRemote (Z :. n)
  tmp     <- allocateRemote (Z :. s)  :: Par PTX (Vector e)

  -- Step 1: independent thread-block-wide scans. Each block stores its partial
  -- sum to a temporary array.
  executeOp k1 gamma aenv (Z :. s) (tmp, result)

  -- If this was a small array that was processed by a single thread block then
  -- we are done, otherwise compute the per-block prefix and apply those values
  -- to the partial results.
  if s == 1
    then
      case tmp of
        Array _ ad -> put future (result, Array () ad)

    else do
      sums <- allocateRemote Z
      executeOp k2 gamma aenv (Z :. s)   (tmp, sums)
      executeOp k3 gamma aenv (Z :. s-1) (tmp, result, c)
      put future (result, sums)
  --
  return future

{-# INLINE scan'DimOp #-}
scan'DimOp
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh :. Int
    -> Par PTX (Future (Array (sh:.Int) e, Array sh e))
scan'DimOp exe gamma aenv sh@(sz :. _) = withExecutable exe $ \ptxExecutable -> do
  future  <- new
  result  <- allocateRemote sh
  sums    <- allocateRemote sz
  executeOp (ptxExecutable !# "scan") gamma aenv (Z :. size sz) (result, sums)
  put future (result, sums)
  return future


{-# INLINE permuteOp #-}
permuteOp
    :: (Shape sh, Shape sh', Elt e)
    => Bool
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Array sh' e
    -> Par PTX (Future (Array sh' e))
permuteOp inplace exe gamma aenv shIn dfs@(shape -> shOut) = withExecutable exe $ \ptxExecutable -> do
  let
      n       = size shIn
      m       = size shOut
      kernel  = case functionTable ptxExecutable of
                  k:_ -> k
                  _   -> $internalError "permute" "no kernels found"
  --
  future  <- new
  result  <- if inplace
               then Debug.trace Debug.dump_exec "exec: permute/inplace" $ return dfs
               else Debug.trace Debug.dump_exec "exec: permute/clone"   $ get =<< cloneArrayAsync dfs
  --
  case kernelName kernel of
    -- execute directly using atomic operations
    "permute_rmw"   -> executeOp kernel gamma aenv (Z :. n) result

    -- a temporary array is required for spin-locks around the critical section
    "permute_mutex" -> do
      barrier     <- new                     :: Par PTX (Future (Vector Word32))
      Array _ ad  <- allocateRemote (Z :. m) :: Par PTX (Vector Word32)
      fork $ do fill <- memsetArrayAsync m 0 ad
                put barrier . Array ((), m) =<< get fill
      --
      executeOp kernel gamma aenv (Z :. n) (result, barrier)

    _               -> $internalError "permute" "unexpected kernel image"
  --
  put future result
  return future


{-# INLINE stencil1Op #-}
stencil1Op
    :: (Shape sh, Elt e)
    => sh
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> Par PTX (Future (Array sh e))
stencil1Op halo exe gamma aenv sh =
  stencilCore exe gamma aenv halo sh

-- Using the defaulting instances for stencil operations (for now).
--
{-# INLINE stencil2Op #-}
stencil2Op
    :: (Shape sh, Elt e)
    => sh
    -> ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> sh
    -> Par PTX (Future (Array sh e))
stencil2Op halo exe gamma aenv sh1 sh2 =
  stencilCore exe gamma aenv halo (sh1 `intersect` sh2)

{-# INLINE stencilCore #-}
stencilCore
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR PTX
    -> Gamma aenv
    -> Val aenv
    -> sh                       -- border dimensions (i.e. index of first interior element)
    -> sh                       -- output array size
    -> Par PTX (Future (Array sh e))
stencilCore exe gamma aenv halo shOut =  withExecutable exe $ \ptxExecutable -> do
  let
      inside  = ptxExecutable !# "stencil_inside"
      border  = ptxExecutable !# "stencil_border"

      shIn :: sh
      shIn = trav (\x y -> x - 2*y) shOut halo

      trav :: (Int -> Int -> Int) -> sh -> sh -> sh
      trav f a b = toElt $ go (eltType @sh) (fromElt a) (fromElt b)
        where
          go :: TupleType t -> t -> t -> t
          go TypeRunit         ()      ()      = ()
          go (TypeRpair ta tb) (xa,xb) (ya,yb) = (go ta xa ya, go tb xb yb)
          go (TypeRscalar t)   x       y
            | SingleScalarType (NumSingleType (IntegralNumType TypeInt{})) <- t = f x y
            | otherwise                                                         = $internalError "stencilCore" "expected Int dimensions"
  --
  future  <- new
  result  <- allocateRemote shOut
  parent  <- asks ptxStream

  -- interior (no bounds checking)
  executeOp inside gamma aenv shIn (shIn, result)

  -- halo regions (bounds checking)
  -- executed in separate streams so that they might overlap the main stencil
  -- and each other, as individually they will not saturate the device
  forM_ (stencilBorders shOut halo) $ \(u, v) ->
    fork $ do
      -- launch in a separate stream
      let sh = trav (-) v u
      executeOp border gamma aenv sh (u, sh, result)

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
    :: forall sh. Shape sh
    => sh
    -> sh
    -> [(sh, sh)]
stencilBorders sh halo = [ face i | i <- [0 .. (2 * rank @sh - 1)] ]
  where
    face :: Int -> (sh, sh)
    face n = let (u,v) = go n (eltType @sh) (fromElt sh) (fromElt halo)
             in  (toElt u, toElt v)

    go :: Int -> TupleType t -> t -> t -> (t, t)
    go _ TypeRunit           ()         ()         = ((), ())
    go n (TypeRpair tsh tsz) (sha, sza) (shb, szb)
      | TypeRscalar (SingleScalarType (NumSingleType (IntegralNumType TypeInt{}))) <- tsz
      = let
            (sha', shb')  = go (n-2) tsh sha shb
            (sza', szb')
              | n <  0    = (0,       sza)
              | n == 0    = (0,       szb)
              | n == 1    = (sza-szb, sza)
              | otherwise = (szb,     sza-szb)
        in
        ((sha', sza'), (shb', szb'))
    go _ _ _ _
      = $internalError "stencilBorders" "expected Int dimensions"


-- Foreign functions
--
{-# INLINE aforeignOp #-}
aforeignOp
    :: (Arrays as, Arrays bs)
    => String
    -> (as -> Par PTX (Future bs))
    -> as
    -> Par PTX (Future bs)
aforeignOp name asm arr = do
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
(!#) :: FunctionTable -> ShortByteString -> Kernel
(!#) exe name
  = fromMaybe ($internalError "lookupFunction" ("function not found: " ++ unpack name))
  $ lookupKernel name exe

lookupKernel :: ShortByteString -> FunctionTable -> Maybe Kernel
lookupKernel name ptxExecutable =
  find (\k -> kernelName k == name) (functionTable ptxExecutable)

-- | Execute some operation with the supplied executable functions
--
withExecutable :: ExecutableR PTX -> (FunctionTable -> Par PTX b) -> Par PTX b
withExecutable PTXR{..} f =
  local (\(s,_) -> (s,Just ptxExecutable)) $ do
    r <- f (unsafeGetValue ptxExecutable)
    liftIO $ touchLifetime ptxExecutable
    return r


-- Execute the function implementing this kernel.
--
executeOp
    :: (Shape sh, Marshalable (Par PTX) args)
    => Kernel
    -> Gamma aenv
    -> Val aenv
    -> sh
    -> args
    -> Par PTX ()
executeOp kernel gamma aenv sh args =
  let n = size sh
  in  when (n > 0) $ do
        stream <- asks ptxStream
        argv   <- marshal (Proxy::Proxy PTX) (args, (gamma, aenv))
        liftIO  $ launch kernel stream n argv


-- Execute a device function with the given thread configuration and function
-- parameters.
--
launch :: Kernel -> Stream -> Int -> [CUDA.FunParam] -> IO ()
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

