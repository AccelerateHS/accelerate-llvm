{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.FoldSeg
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.FoldSeg
  where

import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Fold
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )

import Control.Monad
import Prelude                                                      as P

{--
-- Segmented reduction where a single processor reduces the entire array. The
-- segments array contains the length of each segment.
--
mkFoldSegS
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    => UID
    -> Gamma             aenv
    -> IRFun2     Native aenv (e -> e -> e)
    -> MIRExp     Native aenv e
    -> MIRDelayed Native aenv (Array (sh :. Int) e)
    -> MIRDelayed Native aenv (Segments i)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array (sh :. Int) e))
mkFoldSegS uid aenv combine mseed marr mseg =
  let
      (start, end, paramGang) = gangParam @DIM1
      (arrOut, paramOut)      = mutableArray @(sh:.Int) "out"
      (arrIn,  paramIn)       = delayedArray @(sh:.Int) "in"  marr
      (arrSeg, paramSeg)      = delayedArray @DIM1      "seg" mseg
      paramEnv                = envParam aenv
  in
  makeOpenAcc uid "foldSegS" (paramGang ++ paramOut ++ paramIn ++ paramSeg ++ paramEnv) $ do

    -- Number of segments, useful only if reducing DIM2 and higher
    ss <- indexHead <$> delayedExtent arrSeg

    let test si = A.lt singleType (A.fst si) (indexHead end)
        initial = A.pair (indexHead start) (lift 0)

        body :: IR (Int,Int) -> CodeGen Native (IR (Int,Int))
        body (A.unpair -> (s,inf)) = do
          -- We can avoid an extra division if this is a DIM1 array. Higher
          -- dimensional reductions need to wrap around the segment array at
          -- each new lower-dimensional index.
          s'  <- case rank @sh of
                   0 -> return s
                   _ -> A.rem integralType s ss

          len <- A.fromIntegral integralType numType =<< app1 (delayedLinearIndex arrSeg) s'
          sup <- A.add numType inf len

          r   <- case mseed of
                   Just seed -> do z <- seed
                                   reduceFromTo  inf sup (app2 combine) z (app1 (delayedLinearIndex arrIn))
                   Nothing   ->    reduce1FromTo inf sup (app2 combine)   (app1 (delayedLinearIndex arrIn))
          writeArray arrOut s r

          t <- A.add numType s (lift 1)
          return $ A.pair t sup

    void $ while test body initial
    return_
--}


-- Segmented reduction along the innermost dimension of an array. Performs one
-- reduction per segment of the source array. When no seed is given, assumes
-- that /all/ segments are non-empty.
--
-- This implementation assumes that the segments array represents the offset
-- indices to the source array, rather than the lengths of each segment. The
-- segment-offset approach is required for parallel implementations.
--
mkFoldSeg
    :: UID
    -> Gamma             aenv
    -> ArrayR (Array (sh, Int) e)
    -> IntegralType i
    -> IRFun2     Native aenv (e -> e -> e)
    -> MIRExp     Native aenv e
    -> MIRDelayed Native aenv (Array (sh, Int) e)
    -> MIRDelayed Native aenv (Segments i)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array (sh, Int) e))
mkFoldSeg uid aenv aR@(ArrayR shR eR) int combine mseed marr mseg =
  let
      (start, end, paramGang) = gangParam shR
      (arrOut, paramOut)      = mutableArray aR "out"
      (arrIn,  paramIn)       = delayedArray    "in"  marr
      (arrSeg, paramSeg)      = delayedArray    "seg" mseg
      paramEnv                = envParam aenv
  in
  makeOpenAcc uid "foldSegP" (paramGang ++ paramOut ++ paramIn ++ paramSeg ++ paramEnv) $ do

    imapNestFromTo shR start end (irArrayShape arrOut) $ \ix ii -> do

      -- Determine the start and end indices of the innermost portion of
      -- the array to reduce. This is a segment-offset array computed by
      -- 'scanl (+) 0' of the segment length array.
      --
      let iz = indexTail ix
          i  = indexHead ix
      --
      j <- A.add numType i (liftInt 1)
      u <- A.fromIntegral int numType =<< app1 (delayedLinearIndex arrSeg) i
      v <- A.fromIntegral int numType =<< app1 (delayedLinearIndex arrSeg) j

      r <- case mseed of
             Just seed -> do z <- seed
                             reduceFromTo  eR u v (app2 combine) z (app1 (delayedIndex arrIn) . indexCons iz)
             Nothing   ->    reduce1FromTo eR u v (app2 combine)   (app1 (delayedIndex arrIn) . indexCons iz)

      writeArray TypeInt arrOut ii r

    return_

