{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Scan
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Scan (

  mkScan, mkScan',

) where

import Data.Array.Accelerate.AST                                    ( Direction(..) )
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Loop
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
import Data.Array.Accelerate.LLVM.PTX.Target

import LLVM.AST.Type.Representation

import qualified Foreign.CUDA.Analysis                              as CUDA

import Control.Applicative
import Control.Monad                                                ( (>=>), void )
import Control.Monad.State                                          ( gets )
import Data.String                                                  ( fromString )
import Data.Coerce                                                  as Safe
import Data.Bits                                                    as P
import Prelude                                                      as P hiding ( last )



-- 'Data.List.scanl' or 'Data.List.scanl1' style exclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation.
--
-- > scanl (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 11) [10,10,11,13,16,20,25,31,38,46,55]
--
mkScan
    :: forall aenv sh e.
       Gamma            aenv
    -> ArrayR (Array (sh, Int) e)
    -> Direction
    -> IRFun2       PTX aenv (e -> e -> e)
    -> Maybe (IRExp PTX aenv e)
    -> MIRDelayed   PTX aenv (Array (sh, Int) e)
    -> CodeGen      PTX      (IROpenAcc PTX aenv (Array (sh, Int) e))
mkScan aenv repr dir combine seed arr
  = foldr1 (+++) <$> sequence (codeScan ++ codeFill)

  where
    codeScan = case repr of
      ArrayR (ShapeRsnoc ShapeRz) tp -> [ mkScanAllP1 dir aenv tp   combine seed arr
                                        , mkScanAllP2 dir aenv tp   combine
                                        , mkScanAllP3 dir aenv tp   combine seed
                                        ]
      _                              -> [ mkScanDim   dir aenv repr combine seed arr
                                        ]
    codeFill = case seed of
      Just s -> [ mkScanFill aenv repr s ]
      Nothing -> []

-- Variant of 'scanl' where the final result is returned in a separate array.
--
-- > scanr' (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> ( Array (Z :. 10) [10,10,11,13,16,20,25,31,38,46]
--       , Array Z [55]
--       )
--
mkScan'
    :: forall aenv sh e.
       Gamma          aenv
    -> ArrayR (Array (sh, Int) e)
    -> Direction
    -> IRFun2     PTX aenv (e -> e -> e)
    -> IRExp      PTX aenv e
    -> MIRDelayed PTX aenv (Array (sh, Int) e)
    -> CodeGen    PTX      (IROpenAcc PTX aenv (Array (sh, Int) e, Array sh e))
mkScan' aenv repr dir combine seed arr
  | ArrayR (ShapeRsnoc ShapeRz) tp <- repr
  = foldr1 (+++) <$> sequence [ mkScan'AllP1 dir aenv tp combine seed arr
                              , mkScan'AllP2 dir aenv tp combine
                              , mkScan'AllP3 dir aenv tp combine
                              , mkScan'Fill aenv repr seed
                              ]
  --
  | otherwise
  = (+++) <$> mkScan'Dim dir aenv repr combine seed arr
          <*> mkScan'Fill  aenv repr seed


-- Device wide scans
-- -----------------
--
-- This is a classic two-pass algorithm which proceeds in two phases and
-- requires ~4n data movement to global memory. In future we would like to
-- replace this with a single pass algorithm.
--

-- Parallel scan, step 1.
--
-- Threads scan a stripe of the input into a temporary array, incorporating the
-- initial element and any fused functions on the way. The final reduction
-- result of this chunk is written to a separate array.
--
mkScanAllP1
    :: forall aenv e.
       Direction
    -> Gamma          aenv                      -- ^ array environment
    -> TypeR e
    -> IRFun2     PTX aenv (e -> e -> e)        -- ^ combination function
    -> MIRExp     PTX aenv e                    -- ^ seed element, if this is an exclusive scan
    -> MIRDelayed PTX aenv (Vector e)           -- ^ input data
    -> CodeGen    PTX (IROpenAcc PTX aenv (Vector e))
mkScanAllP1 dir aenv tp combine mseed marr = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  --
  let
      (arrOut, paramOut)  = mutableArray (ArrayR dim1 tp) "out"
      (arrTmp, paramTmp)  = mutableArray (ArrayR dim1 tp) "tmp"
      (arrIn,  paramIn)   = delayedArray "in" marr
      end                 = indexHead (irArrayShape arrTmp)
      paramEnv            = envParam aenv
      --
      config              = launchConfig dev (CUDA.incWarp dev) smem const [|| const ||]
      smem n              = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `P.quot` ws
          per_warp  = ws + ws `P.quot` 2
          bytes     = bytesElt tp
  --
  makeOpenAccWith config "scanP1" (paramTmp ++ paramOut ++ paramIn ++ paramEnv) $ do

    -- Size of the input array
    sz  <- indexHead <$> delayedExtent arrIn

    -- A thread block scans a non-empty stripe of the input, storing the final
    -- block-wide aggregate into a separate array
    --
    -- For exclusive scans, thread 0 of segment 0 must incorporate the initial
    -- element into the input and output. Threads shuffle their indices
    -- appropriately.
    --
    bid <- blockIdx
    gd  <- gridDim
    gd' <- int gd
    s0  <- int bid

    -- iterating over thread-block-wide segments
    imapFromStepTo s0 gd' end $ \chunk -> do

      bd    <- blockDim
      bd'   <- int bd
      inf   <- A.mul numType chunk bd'

      -- index i* is the index that this thread will read data from. Recall that
      -- the supremum index is exclusive
      tid   <- threadIdx
      tid'  <- int tid
      i0    <- case dir of
                 LeftToRight -> A.add numType inf tid'
                 RightToLeft -> do x <- A.sub numType sz inf
                                   y <- A.sub numType x tid'
                                   z <- A.sub numType y (liftInt 1)
                                   return z

      -- index j* is the index that we write to. Recall that for exclusive scans
      -- the output array is one larger than the input; the initial element will
      -- be written into this spot by thread 0 of the first thread block.
      j0    <- case mseed of
                 Nothing -> return i0
                 Just _  -> case dir of
                              LeftToRight -> A.add numType i0 (liftInt 1)
                              RightToLeft -> return i0

      -- If this thread has input, read data and participate in thread-block scan
      let valid i = case dir of
                      LeftToRight -> A.lt  singleType i sz
                      RightToLeft -> A.gte singleType i (liftInt 0)

      when (valid i0) $ do
        x0 <- app1 (delayedLinearIndex arrIn) i0
        x1 <- case mseed of
                Nothing   -> return x0
                Just seed ->
                  if (tp, A.eq singleType tid (liftInt32 0) `A.land'` A.eq singleType chunk (liftInt 0))
                    then do
                      z <- seed
                      case dir of
                        LeftToRight -> writeArray TypeInt32 arrOut (liftInt32 0) z >> app2 combine z x0
                        RightToLeft -> writeArray TypeInt   arrOut sz            z >> app2 combine x0 z
                    else
                      return x0

        n  <- A.sub numType sz inf
        n' <- i32 n
        x2 <- if (tp, A.gte singleType n bd')
                then scanBlockSMem dir dev tp combine Nothing   x1
                else scanBlockSMem dir dev tp combine (Just n') x1

        -- Write this thread's scan result to memory
        writeArray TypeInt arrOut j0 x2

        -- The last thread also writes its result---the aggregate for this
        -- thread block---to the temporary partial sums array. This is only
        -- necessary for full blocks in a multi-block scan; the final
        -- partially-full tile does not have a successor block.
        last <- A.sub numType bd (liftInt32 1)
        when (A.gt singleType gd (liftInt32 1) `land'` A.eq singleType tid last) $
          case dir of
            LeftToRight -> writeArray TypeInt arrTmp chunk x2
            RightToLeft -> do u <- A.sub numType end chunk
                              v <- A.sub numType u (liftInt 1)
                              writeArray TypeInt arrTmp v x2

    return_


-- Parallel scan, step 2
--
-- A single thread block performs a scan of the per-block aggregates computed in
-- step 1. This gives the per-block prefix which must be added to each element
-- in step 3.
--
mkScanAllP2
    :: forall aenv e.
       Direction
    -> Gamma       aenv                         -- ^ array environment
    -> TypeR e
    -> IRFun2  PTX aenv (e -> e -> e)           -- ^ combination function
    -> CodeGen PTX      (IROpenAcc PTX aenv (Vector e))
mkScanAllP2 dir aenv tp combine = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  --
  let
      (arrTmp, paramTmp)  = mutableArray (ArrayR dim1 tp) "tmp"
      paramEnv            = envParam aenv
      start               = liftInt 0
      end                 = indexHead (irArrayShape arrTmp)
      --
      config              = launchConfig dev (CUDA.incWarp dev) smem grid gridQ
      grid _ _            = 1
      gridQ               = [|| \_ _ -> 1 ||]
      smem n              = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `P.quot` ws
          per_warp  = ws + ws `P.quot` 2
          bytes     = bytesElt tp
  --
  makeOpenAccWith config "scanP2" (paramTmp ++ paramEnv) $ do

    -- The first and last threads of the block need to communicate the
    -- block-wide aggregate as a carry-in value across iterations.
    --
    -- TODO: We could optimise this a bit if we can get access to the shared
    -- memory area used by 'scanBlockSMem', and from there directly read the
    -- value computed by the last thread.
    carry <- staticSharedMem tp 1

    bd    <- blockDim
    bd'   <- int bd

    imapFromStepTo start bd' end $ \offset -> do

      -- Index of the partial sums array that this thread will process.
      tid   <- threadIdx
      tid'  <- int tid
      i0    <- case dir of
                 LeftToRight -> A.add numType offset tid'
                 RightToLeft -> do x <- A.sub numType end offset
                                   y <- A.sub numType x tid'
                                   z <- A.sub numType y (liftInt 1)
                                   return z

      let valid i = case dir of
                      LeftToRight -> A.lt  singleType i end
                      RightToLeft -> A.gte singleType i start

      when (valid i0) $ do

        -- wait for the carry-in value to be updated
        __syncthreads

        x0 <- readArray TypeInt arrTmp i0
        x1 <- if (tp, A.gt singleType offset (liftInt 0) `land'` A.eq singleType tid (liftInt32 0))
                then do
                  c <- readArray TypeInt32 carry (liftInt32 0)
                  case dir of
                    LeftToRight -> app2 combine c x0
                    RightToLeft -> app2 combine x0 c
                else do
                  return x0

        n  <- A.sub numType end offset
        n' <- i32 n
        x2 <- if (tp, A.gte singleType n bd')
                then scanBlockSMem dir dev tp combine Nothing   x1
                else scanBlockSMem dir dev tp combine (Just n') x1

        -- Update the temporary array with this thread's result
        writeArray TypeInt arrTmp i0 x2

        -- The last thread writes the carry-out value. If the last thread is not
        -- active, then this must be the last stripe anyway.
        last <- A.sub numType bd (liftInt32 1)
        when (A.eq singleType tid last) $
          writeArray TypeInt32 carry (liftInt32 0) x2

    return_


-- Parallel scan, step 3.
--
-- Threads combine every element of the partial block results with the carry-in
-- value computed in step 2.
--
mkScanAllP3
    :: forall aenv e.
       Direction
    -> Gamma       aenv                         -- ^ array environment
    -> TypeR e
    -> IRFun2  PTX aenv (e -> e -> e)           -- ^ combination function
    -> MIRExp  PTX aenv e                       -- ^ seed element, if this is an exclusive scan
    -> CodeGen PTX      (IROpenAcc PTX aenv (Vector e))
mkScanAllP3 dir aenv tp combine mseed = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  --
  let
      (arrOut, paramOut)  = mutableArray (ArrayR dim1 tp) "out"
      (arrTmp, paramTmp)  = mutableArray (ArrayR dim1 tp) "tmp"
      paramEnv            = envParam aenv
      --
      stride              = local     (TupRsingle scalarTypeInt) "ix.stride"
      paramStride         = parameter (TupRsingle scalarTypeInt) "ix.stride"
      --
      config              = launchConfig dev (CUDA.incWarp dev) (const 0) const [|| const ||]
  --
  makeOpenAccWith config "scanP3" (paramTmp ++ paramOut ++ paramStride ++ paramEnv) $ do

    sz  <- return $ indexHead (irArrayShape arrOut)
    tid <- int =<< threadIdx

    -- Threads that will never contribute can just exit immediately. The size of
    -- each chunk is set by the block dimension of the step 1 kernel, which may
    -- be different from the block size of this kernel.
    when (A.lt singleType tid stride) $ do

      -- Iterate over the segments computed in phase 1. Note that we have one
      -- fewer chunk to process because the first has no carry-in.
      bid <- int =<< blockIdx
      gd  <- int =<< gridDim
      end <- A.sub numType (indexHead (irArrayShape arrTmp)) (liftInt 1)

      imapFromStepTo bid gd end $ \chunk -> do

        -- Determine the start and end indicies of this chunk to which we will
        -- carry-in the value. Returned for left-to-right traversal.
        (inf,sup) <- case dir of
                       LeftToRight -> do
                         a <- A.add numType chunk (liftInt 1)
                         b <- A.mul numType stride a
                         case mseed of
                           Just{}  -> do
                             c <- A.add numType    b (liftInt 1)
                             d <- A.add numType    c stride
                             e <- A.min singleType d sz
                             return (c,e)
                           Nothing -> do
                             c <- A.add numType    b stride
                             d <- A.min singleType c sz
                             return (b,d)
                       RightToLeft -> do
                         a <- A.sub numType end chunk
                         b <- A.mul numType stride a
                         c <- A.sub numType sz b
                         case mseed of
                           Just{}  -> do
                             d <- A.sub numType    c (liftInt 1)
                             e <- A.sub numType    d stride
                             f <- A.max singleType e (liftInt 0)
                             return (f,d)
                           Nothing -> do
                             d <- A.sub numType    c stride
                             e <- A.max singleType d (liftInt 0)
                             return (e,c)

        -- Read the carry-in value
        carry     <- case dir of
                       LeftToRight -> readArray TypeInt arrTmp chunk
                       RightToLeft -> do
                         a <- A.add numType chunk (liftInt 1)
                         b <- readArray TypeInt arrTmp a
                         return b

        -- Apply the carry-in value to each element in the chunk
        bd        <- int =<< blockDim
        i0        <- A.add numType inf tid
        imapFromStepTo i0 bd sup $ \i -> do
          v <- readArray TypeInt arrOut i
          u <- case dir of
                 LeftToRight -> app2 combine carry v
                 RightToLeft -> app2 combine v carry
          writeArray TypeInt arrOut i u

    return_


-- Parallel scan', step 1.
--
-- Similar to mkScanAllP1. Threads scan a stripe of the input into a temporary
-- array, incorporating the initial element and any fused functions on the way.
-- The final reduction result of this chunk is written to a separate array.
--
mkScan'AllP1
    :: forall aenv e.
       Direction
    -> Gamma          aenv
    -> TypeR e
    -> IRFun2     PTX aenv (e -> e -> e)
    -> IRExp      PTX aenv e
    -> MIRDelayed PTX aenv (Vector e)
    -> CodeGen    PTX      (IROpenAcc PTX aenv (Vector e, Scalar e))
mkScan'AllP1 dir aenv tp combine seed marr = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  --
  let
      (arrOut, paramOut)  = mutableArray (ArrayR dim1 tp) "out"
      (arrTmp, paramTmp)  = mutableArray (ArrayR dim1 tp) "tmp"
      (arrIn,  paramIn)   = delayedArray "in" marr
      end                 = indexHead (irArrayShape arrTmp)
      paramEnv            = envParam aenv
      --
      config              = launchConfig dev (CUDA.incWarp dev) smem const [|| const ||]
      smem n              = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `P.quot` ws
          per_warp  = ws + ws `P.quot` 2
          bytes     = bytesElt tp
  --
  makeOpenAccWith config "scanP1" (paramTmp ++ paramOut ++ paramIn ++ paramEnv) $ do

    -- Size of the input array
    sz  <- indexHead <$> delayedExtent arrIn

    -- A thread block scans a non-empty stripe of the input, storing the partial
    -- result and the final block-wide aggregate
    bid <- int =<< blockIdx
    gd  <- int =<< gridDim

    -- iterate over thread-block wide segments
    imapFromStepTo bid gd end $ \seg -> do

      bd  <- int =<< blockDim
      inf <- A.mul numType seg bd

      -- i* is the index that this thread will read data from
      tid <- int =<< threadIdx
      i0  <- case dir of
               LeftToRight -> A.add numType inf tid
               RightToLeft -> do x <- A.sub numType sz inf
                                 y <- A.sub numType x tid
                                 z <- A.sub numType y (liftInt 1)
                                 return z

      -- j* is the index this thread will write to. This is just shifted by one
      -- to make room for the initial element
      j0  <- case dir of
               LeftToRight -> A.add numType i0 (liftInt 1)
               RightToLeft -> A.sub numType i0 (liftInt 1)

      -- If this thread has input it participates in the scan
      let valid i = case dir of
                      LeftToRight -> A.lt  singleType i sz
                      RightToLeft -> A.gte singleType i (liftInt 0)

      when (valid i0) $ do
        x0 <- app1 (delayedLinearIndex arrIn) i0

        -- Thread 0 of the first segment must also evaluate and store the
        -- initial element
        ti <- threadIdx
        x1 <- if (tp, A.eq singleType ti (liftInt32 0) `A.land'` A.eq singleType seg (liftInt 0))
                then do
                  z <- seed
                  writeArray TypeInt arrOut i0 z
                  case dir of
                    LeftToRight -> app2 combine z x0
                    RightToLeft -> app2 combine x0 z
                else
                  return x0

        -- Block-wide scan
        n  <- A.sub numType sz inf
        n' <- i32 n
        x2 <- if (tp, A.gte singleType n bd)
                then scanBlockSMem dir dev tp combine Nothing   x1
                else scanBlockSMem dir dev tp combine (Just n') x1

        -- Write this thread's scan result to memory. Recall that we had to make
        -- space for the initial element, so the very last thread does not store
        -- its result here.
        case dir of
          LeftToRight -> when (A.lt  singleType j0 sz)          $ writeArray TypeInt arrOut j0 x2
          RightToLeft -> when (A.gte singleType j0 (liftInt 0)) $ writeArray TypeInt arrOut j0 x2

        -- Last active thread writes its result to the partial sums array. These
        -- will be used to compute the carry-in value in step 2.
        m  <- do x <- A.min singleType n bd
                 y <- A.sub numType x (liftInt 1)
                 return y
        when (A.eq singleType tid m) $
          case dir of
            LeftToRight -> writeArray TypeInt arrTmp seg x2
            RightToLeft -> do x <- A.sub numType end seg
                              y <- A.sub numType x (liftInt 1)
                              writeArray TypeInt arrTmp y x2

    return_


-- Parallel scan', step 2
--
-- A single thread block performs an inclusive scan of the partial sums array to
-- compute the per-block carry-in values, as well as the final reduction result.
--
mkScan'AllP2
    :: forall aenv e.
       Direction
    -> Gamma aenv
    -> TypeR e
    -> IRFun2 PTX aenv (e -> e -> e)
    -> CodeGen PTX (IROpenAcc PTX aenv (Vector e, Scalar e))
mkScan'AllP2 dir aenv tp combine = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  --
  let
      (arrTmp, paramTmp)  = mutableArray (ArrayR dim1 tp) "tmp"
      (arrSum, paramSum)  = mutableArray (ArrayR dim0 tp) "sum"
      paramEnv            = envParam aenv
      start               = liftInt 0
      end                 = indexHead (irArrayShape arrTmp)
      --
      config              = launchConfig dev (CUDA.incWarp dev) smem grid gridQ
      grid _ _            = 1
      gridQ               = [|| \_ _ -> 1 ||]
      smem n              = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `P.quot` ws
          per_warp  = ws + ws `P.quot` 2
          bytes     = bytesElt tp
  --
  makeOpenAccWith config "scanP2" (paramTmp ++ paramSum ++ paramEnv) $ do

    -- The first and last threads of the block need to communicate the
    -- block-wide aggregate as a carry-in value across iterations.
    carry <- staticSharedMem tp 1

    -- A single thread block iterates over the per-block partial results from
    -- step 1
    tid   <- threadIdx
    tid'  <- int tid
    bd    <- int =<< blockDim

    imapFromStepTo start bd end $ \offset -> do

      i0  <- case dir of
               LeftToRight -> A.add numType offset tid'
               RightToLeft -> do x <- A.sub numType end offset
                                 y <- A.sub numType x tid'
                                 z <- A.sub numType y (liftInt 1)
                                 return z

      let valid i = case dir of
                      LeftToRight -> A.lt  singleType i end
                      RightToLeft -> A.gte singleType i start

      -- wait for the carry-in value to be updated
      __syncthreads

      x0 <- if (tp, valid i0)
              then readArray TypeInt arrTmp i0
              else
                let go :: TypeR a -> Operands a
                    go TupRunit       = OP_Unit
                    go (TupRpair a b) = OP_Pair (go a) (go b)
                    go (TupRsingle t) = ir t (undef t)
                in
                return $ go tp

      x1 <- if (tp, A.gt singleType offset (liftInt 0) `A.land'` A.eq singleType tid (liftInt32 0))
              then do
                c <- readArray TypeInt32 carry (liftInt32 0)
                case dir of
                  LeftToRight -> app2 combine c x0
                  RightToLeft -> app2 combine x0 c
              else
                return x0

      n  <- A.sub numType end offset
      n' <- i32 n
      x2 <- if (tp, A.gte singleType n bd)
              then scanBlockSMem dir dev tp combine Nothing   x1
              else scanBlockSMem dir dev tp combine (Just n') x1

      -- Update the partial results array
      when (valid i0) $
        writeArray TypeInt arrTmp i0 x2

      -- The last active thread saves its result as the carry-out value.
      m  <- do x <- A.min singleType bd n
               y <- A.sub numType x (liftInt 1)
               z <- i32 y
               return z
      when (A.eq singleType tid m) $
        writeArray TypeInt32 carry (liftInt32 0) x2

    -- First thread stores the final carry-out values at the final reduction
    -- result for the entire array
    __syncthreads

    when (A.eq singleType tid (liftInt32 0)) $
      writeArray TypeInt32 arrSum (liftInt32 0) =<< readArray TypeInt32 carry (liftInt32 0)

    return_


-- Parallel scan', step 3.
--
-- Threads combine every element of the partial block results with the carry-in
-- value computed in step 2.
--
mkScan'AllP3
    :: forall aenv e.
       Direction
    -> Gamma aenv                                   -- ^ array environment
    -> TypeR e
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> CodeGen PTX (IROpenAcc PTX aenv (Vector e, Scalar e))
mkScan'AllP3 dir aenv tp combine = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  --
  let
      (arrOut, paramOut)  = mutableArray (ArrayR dim1 tp) "out"
      (arrTmp, paramTmp)  = mutableArray (ArrayR dim1 tp) "tmp"
      paramEnv            = envParam aenv
      --
      stride              = local     (TupRsingle scalarTypeInt) "ix.stride"
      paramStride         = parameter (TupRsingle scalarTypeInt) "ix.stride"
      --
      config              = launchConfig dev (CUDA.incWarp dev) (const 0) const [|| const ||]
  --
  makeOpenAccWith config "scanP3" (paramTmp ++ paramOut ++ paramStride ++ paramEnv) $ do

    sz  <- return $ indexHead (irArrayShape arrOut)
    tid <- int =<< threadIdx

    when (A.lt singleType tid stride) $ do

      bid <- int =<< blockIdx
      gd  <- int =<< gridDim
      end <- A.sub numType (indexHead (irArrayShape arrTmp)) (liftInt 1)

      imapFromStepTo bid gd end $ \chunk -> do

        (inf,sup) <- case dir of
                       LeftToRight -> do
                         a <- A.add numType    chunk  (liftInt 1)
                         b <- A.mul numType    stride a
                         c <- A.add numType    b      (liftInt 1)
                         d <- A.add numType    c      stride
                         e <- A.min singleType d      sz
                         return (c,e)
                       RightToLeft -> do
                         a <- A.sub numType    end    chunk
                         b <- A.mul numType    stride a
                         c <- A.sub numType    sz     b
                         d <- A.sub numType    c      (liftInt 1)
                         e <- A.sub numType    d      stride
                         f <- A.max singleType e      (liftInt 0)
                         return (f,d)

        carry     <- case dir of
                       LeftToRight -> readArray TypeInt arrTmp chunk
                       RightToLeft -> do
                         a <- A.add numType chunk (liftInt 1)
                         b <- readArray TypeInt arrTmp a
                         return b

        -- Apply the carry-in value to each element in the chunk
        bd        <- int =<< blockDim
        i0        <- A.add numType inf tid
        imapFromStepTo i0 bd sup $ \i -> do
          v <- readArray TypeInt arrOut i
          u <- case dir of
                 LeftToRight -> app2 combine carry v
                 RightToLeft -> app2 combine v carry
          writeArray TypeInt arrOut i u

    return_


-- Multidimensional scans
-- ----------------------

-- Multidimensional scan along the innermost dimension
--
-- A thread block individually computes along each innermost dimension. This is
-- a single-pass operation.
--
--  * We can assume that the array is non-empty; exclusive scans with empty
--    innermost dimension will be instead filled with the seed element via
--    'mkScanFill'.
--
--  * Small but non-empty innermost dimension arrays (size << thread
--    block size) will have many threads which do no work.
--
mkScanDim
    :: forall aenv sh e.
       Direction
    -> Gamma          aenv                          -- ^ array environment
    -> ArrayR (Array (sh, Int) e)
    -> IRFun2     PTX aenv (e -> e -> e)            -- ^ combination function
    -> MIRExp     PTX aenv e                        -- ^ seed element, if this is an exclusive scan
    -> MIRDelayed PTX aenv (Array (sh, Int) e)      -- ^ input data
    -> CodeGen    PTX (IROpenAcc PTX aenv (Array (sh, Int) e))
mkScanDim dir aenv repr@(ArrayR (ShapeRsnoc shr) tp) combine mseed marr = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  --
  let
      (arrOut, paramOut)  = mutableArray repr "out"
      (arrIn,  paramIn)   = delayedArray "in" marr
      paramEnv            = envParam aenv
      --
      config              = launchConfig dev (CUDA.incWarp dev) smem const [|| const ||]
      smem n              = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `P.quot` ws
          per_warp  = ws + ws `P.quot` 2
          bytes     = bytesElt tp
  --
  makeOpenAccWith config "scan" (paramOut ++ paramIn ++ paramEnv) $ do

    -- The first and last threads of the block need to communicate the
    -- block-wide aggregate as a carry-in value across iterations.
    --
    -- TODO: we could optimise this a bit if we can get access to the shared
    -- memory area used by 'scanBlockSMem', and from there directly read the
    -- value computed by the last thread.
    carry <- staticSharedMem tp 1

    -- Size of the input array
    sz  <- indexHead <$> delayedExtent arrIn

    -- Thread blocks iterate over the outer dimensions. Threads in a block
    -- cooperatively scan along one dimension, but thread blocks do not
    -- communicate with each other.
    --
    bid <- int =<< blockIdx
    gd  <- int =<< gridDim
    end <- shapeSize shr (indexTail (irArrayShape arrOut))

    imapFromStepTo bid gd end $ \seg -> do

      -- Index this thread reads from
      tid   <- threadIdx
      tid'  <- int tid
      i0    <- case dir of
                 LeftToRight -> do x <- A.mul numType seg sz
                                   y <- A.add numType x tid'
                                   return y

                 RightToLeft -> do x <- A.add numType seg (liftInt 1)
                                   y <- A.mul numType x sz
                                   z <- A.sub numType y tid'
                                   w <- A.sub numType z (liftInt 1)
                                   return w

      -- Index this thread writes to
      j0  <- case mseed of
               Nothing -> return i0
               Just{}  -> do szp1 <- return $ indexHead (irArrayShape arrOut)
                             case dir of
                               LeftToRight -> do x <- A.mul numType seg szp1
                                                 y <- A.add numType x tid'
                                                 return y

                               RightToLeft -> do x <- A.add numType seg (liftInt 1)
                                                 y <- A.mul numType x szp1
                                                 z <- A.sub numType y tid'
                                                 w <- A.sub numType z (liftInt 1)
                                                 return w

      -- Stride indices by block dimension
      bd  <- blockDim
      bd' <- int bd
      let next ix = case dir of
                      LeftToRight -> A.add numType ix bd'
                      RightToLeft -> A.sub numType ix bd'

      -- Initialise this scan segment
      --
      -- If this is an exclusive scan then the first thread just evaluates the
      -- seed element and stores this value into the carry-in slot. All threads
      -- shift their write-to index (j) by one, to make space for this element.
      --
      -- If this is an inclusive scan then do a block-wide scan. The last thread
      -- in the block writes the carry-in value.
      --
      r <-
        case mseed of
          Just seed -> do
            when (A.eq singleType tid (liftInt32 0)) $ do
              z <- seed
              writeArray TypeInt   arrOut j0 z
              writeArray TypeInt32 carry (liftInt32 0) z
            j1 <- case dir of
                   LeftToRight -> A.add numType j0 (liftInt 1)
                   RightToLeft -> A.sub numType j0 (liftInt 1)
            return $ A.trip sz i0 j1

          Nothing -> do
            when (A.lt singleType tid' sz) $ do
              n' <- i32 sz
              x0 <- app1 (delayedLinearIndex arrIn) i0
              r0 <- if (tp, A.gte singleType sz bd')
                      then scanBlockSMem dir dev tp combine Nothing   x0
                      else scanBlockSMem dir dev tp combine (Just n') x0
              writeArray TypeInt arrOut j0 r0

              ll <- A.sub numType bd (liftInt32 1)
              when (A.eq singleType tid ll) $
                writeArray TypeInt32 carry (liftInt32 0) r0

            n1 <- A.sub numType sz bd'
            i1 <- next i0
            j1 <- next j0
            return $ A.trip n1 i1 j1

      -- Iterate over the remaining elements in this segment
      void $ while
        (TupRunit `TupRpair` TupRsingle scalarTypeInt `TupRpair` TupRsingle scalarTypeInt `TupRpair` TupRsingle scalarTypeInt)
        (\(A.fst3   -> n)       -> A.gt singleType n (liftInt 0))
        (\(A.untrip -> (n,i,j)) -> do

          -- Wait for the carry-in value from the previous iteration to be updated
          __syncthreads

          -- Compute and store the next element of the scan
          --
          -- NOTE: As with 'foldSeg' we require all threads to participate in
          -- every iteration of the loop otherwise they will die prematurely.
          -- Out-of-bounds threads return 'undef' at this point, which is really
          -- unfortunate ):
          --
          x <- if (tp, A.lt singleType tid' n)
                 then app1 (delayedLinearIndex arrIn) i
                 else let
                          go :: TypeR a -> Operands a
                          go TupRunit       = OP_Unit
                          go (TupRpair a b) = OP_Pair (go a) (go b)
                          go (TupRsingle t) = ir t (undef t)
                      in
                      return $ go tp

          -- Thread zero incorporates the carry-in element
          y <- if (tp, A.eq singleType tid (liftInt32 0))
                 then do
                   c <- readArray TypeInt32 carry (liftInt32 0)
                   case dir of
                     LeftToRight -> app2 combine c x
                     RightToLeft -> app2 combine x c
                  else
                    return x

          -- Perform the scan and write the result to memory
          m <- i32 n
          z <- if (tp, A.gte singleType n bd')
                 then scanBlockSMem dir dev tp combine Nothing  y
                 else scanBlockSMem dir dev tp combine (Just m) y

          when (A.lt singleType tid' n) $ do
            writeArray TypeInt arrOut j z

            -- The last thread of the block writes its result as the carry-out
            -- value. If this thread is not active then we are on the last
            -- iteration of the loop and it will not be needed.
            w <- A.sub numType bd (liftInt32 1)
            when (A.eq singleType tid w) $
              writeArray TypeInt32 carry (liftInt32 0) z

          -- Update indices for the next iteration
          n' <- A.sub numType n bd'
          i' <- next i
          j' <- next j
          return $ A.trip n' i' j')
        r

    return_


-- Multidimensional scan' along the innermost dimension
--
-- A thread block individually computes along each innermost dimension. This is
-- a single-pass operation.
--
--  * We can assume that the array is non-empty; exclusive scans with empty
--    innermost dimension will be instead filled with the seed element via
--    'mkScan'Fill'.
--
--  * Small but non-empty innermost dimension arrays (size << thread
--    block size) will have many threads which do no work.
--
mkScan'Dim
    :: forall aenv sh e.
       Direction
    -> Gamma          aenv                          -- ^ array environment
    -> ArrayR (Array (sh, Int) e)
    -> IRFun2     PTX aenv (e -> e -> e)            -- ^ combination function
    -> IRExp      PTX aenv e                        -- ^ seed element
    -> MIRDelayed PTX aenv (Array (sh, Int) e)      -- ^ input data
    -> CodeGen    PTX      (IROpenAcc PTX aenv (Array (sh, Int) e, Array sh e))
mkScan'Dim dir aenv repr@(ArrayR (ShapeRsnoc shr) tp) combine seed marr = do
  dev <- liftCodeGen $ gets ptxDeviceProperties
  --
  let
      (arrSum, paramSum)  = mutableArray (reduceRank repr) "sum"
      (arrOut, paramOut)  = mutableArray repr "out"
      (arrIn,  paramIn)   = delayedArray "in" marr
      paramEnv            = envParam aenv
      --
      config              = launchConfig dev (CUDA.incWarp dev) smem const [|| const ||]
      smem n              = warps * (1 + per_warp) * bytes
        where
          ws        = CUDA.warpSize dev
          warps     = n `P.quot` ws
          per_warp  = ws + ws `P.quot` 2
          bytes     = bytesElt tp
  --
  makeOpenAccWith config "scan" (paramOut ++ paramSum ++ paramIn ++ paramEnv) $ do

    -- The first and last threads of the block need to communicate the
    -- block-wide aggregate as a carry-in value across iterations.
    --
    -- TODO: we could optimise this a bit if we can get access to the shared
    -- memory area used by 'scanBlockSMem', and from there directly read the
    -- value computed by the last thread.
    carry <- staticSharedMem tp 1

    -- Size of the input array
    sz    <- indexHead <$> delayedExtent arrIn

    -- If the innermost dimension is smaller than the number of threads in the
    -- block, those threads will never contribute to the output.
    tid   <- threadIdx
    tid'  <- int tid
    when (A.lte singleType tid' sz) $ do

      -- Thread blocks iterate over the outer dimensions, each thread block
      -- cooperatively scanning along each outermost index.
      bid <- int =<< blockIdx
      gd  <- int =<< gridDim
      end <- shapeSize shr (irArrayShape arrSum)

      imapFromStepTo bid gd end $ \seg -> do

        -- Not necessary to wait for threads to catch up before starting this segment
        -- __syncthreads

        -- Linear index bounds for this segment
        inf <- A.mul numType seg sz
        sup <- A.add numType inf sz

        -- Index that this thread will read from. Recall that the supremum index
        -- is exclusive.
        i0  <- case dir of
                 LeftToRight -> A.add numType inf tid'
                 RightToLeft -> do x <- A.sub numType sup tid'
                                   y <- A.sub numType x (liftInt 1)
                                   return y

        -- The index that this thread will write to. This is just shifted along
        -- by one to make room for the initial element.
        j0  <- case dir of
                 LeftToRight -> A.add numType i0 (liftInt 1)
                 RightToLeft -> A.sub numType i0 (liftInt 1)

        -- Evaluate the initial element. Store it into the carry-in slot as well
        -- as to the array as the first element. This is always valid because if
        -- the input array is empty then we will be evaluating via mkScan'Fill.
        when (A.eq singleType tid (liftInt32 0)) $ do
          z <- seed
          writeArray TypeInt   arrOut i0            z
          writeArray TypeInt32 carry  (liftInt32 0) z

        bd  <- blockDim
        bd' <- int bd
        let next ix = case dir of
                        LeftToRight -> A.add numType ix bd'
                        RightToLeft -> A.sub numType ix bd'

        -- Now, threads iterate over the elements along the innermost dimension.
        -- At each iteration the first thread incorporates the carry-in value
        -- from the previous step.
        --
        -- The index tracks how many elements remain for the thread block, since
        -- indices i* and j* are local to each thread
        n0  <- A.sub numType sup inf
        void $ while
          (TupRunit `TupRpair` TupRsingle scalarTypeInt `TupRpair` TupRsingle scalarTypeInt `TupRpair` TupRsingle scalarTypeInt)
          (\(A.fst3   -> n)       -> A.gt singleType n (liftInt 0))
          (\(A.untrip -> (n,i,j)) -> do

            -- Wait for threads to catch up to ensure the carry-in value from
            -- the last iteration has been updated
            __syncthreads

            -- If all threads in the block will participate this round we can
            -- avoid (almost) all bounds checks.
            _ <- if (TupRunit, A.gte singleType n bd')
                    -- All threads participate. No bounds checks required but
                    -- the last thread needs to update the carry-in value.
                    then do
                      x <- app1 (delayedLinearIndex arrIn) i
                      y <- if (tp, A.eq singleType tid (liftInt32 0))
                              then do
                                c <- readArray TypeInt32 carry (liftInt32 0)
                                case dir of
                                  LeftToRight -> app2 combine c x
                                  RightToLeft -> app2 combine x c
                              else
                                return x
                      z <- scanBlockSMem dir dev tp combine Nothing y

                      -- Write results to the output array. Note that if we
                      -- align directly on the boundary of the array this is not
                      -- valid for the last thread.
                      case dir of
                        LeftToRight -> when (A.lt  singleType j sup) $ writeArray TypeInt arrOut j z
                        RightToLeft -> when (A.gte singleType j inf) $ writeArray TypeInt arrOut j z

                      -- Last thread of the block also saves its result as the
                      -- carry-in value
                      bd1 <- A.sub numType bd (liftInt32 1)
                      when (A.eq singleType tid bd1) $
                        writeArray TypeInt32 carry (liftInt32 0) z

                      return (lift TupRunit ())

                    -- Only threads that are in bounds can participate. This is
                    -- the last iteration of the loop. The last active thread
                    -- still needs to store its value into the carry-in slot.
                    --
                    -- Note that all threads must call the block-wide scan.
                    -- SEE: [Synchronisation problems with SM_70 and greater]
                    else do
                      x <- if (tp, A.lt singleType tid' n)
                              then do
                                x <- app1 (delayedLinearIndex arrIn) i
                                y <- if (tp, A.eq singleType tid (liftInt32 0))
                                        then do
                                          c <- readArray TypeInt32 carry (liftInt32 0)
                                          case dir of
                                            LeftToRight -> app2 combine c x
                                            RightToLeft -> app2 combine x c
                                        else
                                          return x
                                return y
                              else
                                let
                                    go :: TypeR a -> Operands a
                                    go TupRunit       = OP_Unit
                                    go (TupRpair a b) = OP_Pair (go a) (go b)
                                    go (TupRsingle t) = ir t (undef t)
                                in
                                return $ go tp

                      l <- i32 n
                      y <- scanBlockSMem dir dev tp combine (Just l) x

                      m <- A.sub numType l (liftInt32 1)
                      when (A.lt singleType tid m) $ writeArray TypeInt   arrOut j            y
                      when (A.eq singleType tid m) $ writeArray TypeInt32 carry (liftInt32 0) y

                      return (lift TupRunit ())

            A.trip <$> A.sub numType n bd' <*> next i <*> next j)
          (A.trip n0 i0 j0)

        -- Wait for the carry-in value to be updated
        __syncthreads

        -- Store the carry-in value to the separate final results array
        when (A.eq singleType tid (liftInt32 0)) $
          writeArray TypeInt arrSum seg =<< readArray TypeInt32 carry (liftInt32 0)

    return_



-- Parallel scan, auxiliary
--
-- If this is an exclusive scan of an empty array, we just fill the result with
-- the seed element.
--
mkScanFill
    :: Gamma aenv
    -> ArrayR (Array sh e)
    -> IRExp PTX aenv e
    -> CodeGen PTX (IROpenAcc PTX aenv (Array sh e))
mkScanFill aenv repr seed =
  mkGenerate aenv repr (IRFun1 (const seed))

mkScan'Fill
    :: Gamma aenv
    -> ArrayR (Array (sh, Int) e)
    -> IRExp PTX aenv e
    -> CodeGen PTX (IROpenAcc PTX aenv (Array (sh, Int) e, Array sh e))
mkScan'Fill aenv repr seed =
  Safe.coerce <$> mkGenerate aenv (reduceRank repr) (IRFun1 (const seed))


-- Block wide scan
-- ---------------

-- Efficient block-wide (inclusive) scan using the specified operator.
--
-- Each block requires (#warps * (1 + 1.5*warp size)) elements of dynamically
-- allocated shared memory.
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.4/cub/block/specializations/block_scan_warp_scans.cuh
--
-- NOTE: [Synchronisation problems with SM_70 and greater]
--
-- This operation uses thread synchronisation. When calling this operation, it
-- is important that all active (that is, non-exited) threads of the thread
-- block participate. It seems that sm_70+ (devices with independent thread
-- scheduling) are stricter about the requirement that all non-existed threads
-- participate in every barrier.
--
-- See: https://github.com/AccelerateHS/accelerate/issues/436
--
scanBlockSMem
    :: forall aenv e.
       Direction
    -> DeviceProperties                             -- ^ properties of the target device
    -> TypeR e
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> Maybe (Operands Int32)                       -- ^ number of valid elements (may be less than block size)
    -> Operands e                                   -- ^ calling thread's input element
    -> CodeGen PTX (Operands e)
scanBlockSMem dir dev tp combine nelem = warpScan >=> warpPrefix
  where
    int32 :: Integral a => a -> Operands Int32
    int32 = liftInt32 . P.fromIntegral

    -- Temporary storage required for each warp
    warp_smem_elems = CUDA.warpSize dev + (CUDA.warpSize dev `P.quot` 2)
    warp_smem_bytes = warp_smem_elems  * bytesElt tp

    -- Step 1: Scan in every warp
    warpScan :: Operands e -> CodeGen PTX (Operands e)
    warpScan input = do
      -- Allocate (1.5 * warpSize) elements of shared memory for each warp
      -- (individually addressable by each warp)
      wid   <- warpId
      skip  <- A.mul numType wid (int32 warp_smem_bytes)
      smem  <- dynamicSharedMem tp TypeInt32 (int32 warp_smem_elems) skip
      scanWarpSMem dir dev tp combine smem input

    -- Step 2: Collect the aggregate results of each warp to compute the prefix
    -- values for each warp and combine with the partial result to compute each
    -- thread's final value.
    warpPrefix :: Operands e -> CodeGen PTX (Operands e)
    warpPrefix input = do
      -- Allocate #warps elements of shared memory
      bd    <- blockDim
      warps <- A.quot integralType bd (int32 (CUDA.warpSize dev))
      skip  <- A.mul numType warps (int32 warp_smem_bytes)
      smem  <- dynamicSharedMem tp TypeInt32 warps skip

      -- Share warp aggregates
      wid   <- warpId
      lane  <- laneId
      when (A.eq singleType lane (int32 (CUDA.warpSize dev - 1))) $ do
        writeArray TypeInt32 smem wid input

      -- Wait for each warp to finish its local scan and share the aggregate
      __syncthreads

      -- Compute the prefix value for this warp and add to the partial result.
      -- This step is not required for the first warp, which has no carry-in.
      if (tp, A.eq singleType wid (liftInt32 0))
        then return input
        else do
          -- Every thread sequentially scans the warp aggregates to compute
          -- their prefix value. We do this sequentially, but could also have
          -- warp 0 do it cooperatively if we limit thread block sizes to
          -- (warp size ^ 2).
          steps  <- case nelem of
                      Nothing -> return wid
                      Just n  -> A.min singleType wid =<< A.quot integralType n (int32 (CUDA.warpSize dev))

          p0     <- readArray TypeInt32 smem (liftInt32 0)
          prefix <- iterFromStepTo tp (liftInt32 1) (liftInt32 1) steps p0 $ \step x -> do
                      y <- readArray TypeInt32 smem step
                      case dir of
                        LeftToRight -> app2 combine x y
                        RightToLeft -> app2 combine y x

          case dir of
            LeftToRight -> app2 combine prefix input
            RightToLeft -> app2 combine input prefix


-- Warp-wide scan
-- --------------

-- Efficient warp-wide (inclusive) scan using the specified operator.
--
-- Each warp requires 48 (1.5 x warp size) elements of shared memory. The
-- routine assumes that it is allocated individually per-warp (i.e. can be
-- indexed in the range [0, warp size)).
--
-- Example: https://github.com/NVlabs/cub/blob/1.5.4/cub/warp/specializations/warp_scan_smem.cuh
--
scanWarpSMem
    :: forall aenv e.
       Direction
    -> DeviceProperties                             -- ^ properties of the target device
    -> TypeR e
    -> IRFun2 PTX aenv (e -> e -> e)                -- ^ combination function
    -> IRArray (Vector e)                           -- ^ temporary storage array in shared memory (1.5 x warp size elements)
    -> Operands e                                   -- ^ calling thread's input element
    -> CodeGen PTX (Operands e)
scanWarpSMem dir dev tp combine smem = scan 0
  where
    log2 :: Double -> Double
    log2 = P.logBase 2

    -- Number of steps required to scan warp
    steps     = P.floor (log2 (P.fromIntegral (CUDA.warpSize dev)))
    halfWarp  = P.fromIntegral (CUDA.warpSize dev `P.quot` 2)

    -- Unfold the scan as a recursive code generation function
    scan :: Int -> Operands e -> CodeGen PTX (Operands e)
    scan step x
      | step >= steps = return x
      | otherwise     = do
          let offset = liftInt32 (1 `P.shiftL` step)

          -- share partial result through shared memory buffer
          lane <- laneId
          i    <- A.add numType lane (liftInt32 halfWarp)
          writeArray TypeInt32 smem i x

          __syncwarp

          -- update partial result if in range
          x'   <- if (tp, A.gte singleType lane offset)
                    then do
                      i' <- A.sub numType i offset    -- lane + HALF_WARP - offset
                      x' <- readArray TypeInt32 smem i'
                      case dir of
                        LeftToRight -> app2 combine x' x
                        RightToLeft -> app2 combine x x'

                    else
                      return x

          __syncwarp

          scan (step+1) x'


-- Utilities
-- ---------

i32 :: Operands Int -> CodeGen PTX (Operands Int32)
i32 = A.fromIntegral integralType numType

int :: Operands Int32 -> CodeGen PTX (Operands Int)
int = A.fromIntegral integralType numType

