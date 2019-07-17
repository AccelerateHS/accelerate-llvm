{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.FoldSeg
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.FoldSeg
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp                       ( indexHead )
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Fold
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )

import Control.Applicative
import Control.Monad
import Prelude                                                      as P


-- Segmented reduction along the innermost dimension of an array. Performs one
-- reduction per segment of the source array.
--
mkFoldSeg
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    => UID
    -> Gamma             aenv
    -> IRFun2     Native aenv (e -> e -> e)
    -> IRExp      Native aenv e
    -> MIRDelayed Native aenv (Array (sh :. Int) e)
    -> MIRDelayed Native aenv (Segments i)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array (sh :. Int) e))
mkFoldSeg uid aenv combine seed arr seg =
  mkFoldSegP uid aenv combine (Just seed) arr seg


-- Segmented reduction along the innermost dimension of an array, where /all/
-- segments are non-empty.
--
mkFold1Seg
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    => UID
    -> Gamma             aenv
    -> IRFun2     Native aenv (e -> e -> e)
    -> MIRDelayed Native aenv (Array (sh :. Int) e)
    -> MIRDelayed Native aenv (Segments i)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array (sh :. Int) e))
mkFold1Seg uid aenv combine arr seg =
  mkFoldSegP uid aenv combine Nothing arr seg


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


-- This implementation assumes that the segments array represents the offset
-- indices to the source array, rather than the lengths of each segment. The
-- segment-offset approach is required for parallel implementations.
--
mkFoldSegP
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    => UID
    -> Gamma             aenv
    -> IRFun2     Native aenv (e -> e -> e)
    -> MIRExp     Native aenv e
    -> MIRDelayed Native aenv (Array (sh :. Int) e)
    -> MIRDelayed Native aenv (Segments i)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array (sh :. Int) e))
mkFoldSegP uid aenv combine mseed marr mseg =
  let
      (start, end, paramGang) = gangParam @DIM1
      (arrOut, paramOut)      = mutableArray @(sh:.Int) "out"
      (arrIn,  paramIn)       = delayedArray @(sh:.Int) "in"  marr
      (arrSeg, paramSeg)      = delayedArray @DIM1      "seg" mseg
      paramEnv                = envParam aenv
  in
  makeOpenAcc uid "foldSegP" (paramGang ++ paramOut ++ paramIn ++ paramSeg ++ paramEnv) $ do

    -- Number of segments and size of the innermost dimension. These are
    -- required if we are reducing a DIM2 or higher array, to properly compute
    -- the start and end indices of the portion of the array to reduce. Note
    -- that this is a segment-offset array computed by 'scanl (+) 0' of the
    -- segment length array, so its size has increased by one.
    sz <- indexHead <$> delayedExtent arrIn
    ss <- do n <- indexHead <$> delayedExtent arrSeg
             A.sub numType n (lift 1)

    imapFromTo (indexHead start) (indexHead end) $ \s -> do

      i   <- case rank @sh of
               0 -> return s
               _ -> A.rem integralType s ss
      j   <- A.add numType i (lift 1)
      u   <- A.fromIntegral integralType numType =<< app1 (delayedLinearIndex arrSeg) i
      v   <- A.fromIntegral integralType numType =<< app1 (delayedLinearIndex arrSeg) j

      (inf,sup) <- A.unpair <$> case rank @sh of
                     0 -> return (A.pair u v)
                     _ -> do q <- A.quot integralType s ss
                             a <- A.mul numType q sz
                             A.pair <$> A.add numType u a <*> A.add numType v a

      r   <- case mseed of
               Just seed -> do z <- seed
                               reduceFromTo  inf sup (app2 combine) z (app1 (delayedLinearIndex arrIn))
               Nothing   ->    reduce1FromTo inf sup (app2 combine)   (app1 (delayedLinearIndex arrIn))

      writeArray arrOut s r

    return_

