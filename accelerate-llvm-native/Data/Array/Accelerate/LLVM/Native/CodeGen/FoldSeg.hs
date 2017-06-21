{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.FoldSeg
-- Copyright   : [2014..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Exp                       ( indexHead )
import Data.Array.Accelerate.LLVM.CodeGen.Loop
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

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
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRExp     Native aenv e
    -> IRDelayed Native aenv (Array (sh :. Int) e)
    -> IRDelayed Native aenv (Segments i)
    -> CodeGen (IROpenAcc Native aenv (Array (sh :. Int) e))
mkFoldSeg aenv combine seed arr seg =
  (+++) <$> mkFoldSegS aenv combine (Just seed) arr seg
        <*> mkFoldSegP aenv combine (Just seed) arr seg


-- Segmented reduction along the innermost dimension of an array, where /all/
-- segments are non-empty.
--
mkFold1Seg
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRDelayed Native aenv (Array (sh :. Int) e)
    -> IRDelayed Native aenv (Segments i)
    -> CodeGen (IROpenAcc Native aenv (Array (sh :. Int) e))
mkFold1Seg aenv combine arr seg =
  (+++) <$> mkFoldSegS aenv combine Nothing arr seg
        <*> mkFoldSegP aenv combine Nothing arr seg


-- Segmented reduction where a single processor reduces the entire array. The
-- segments array contains the length of each segment.
--
mkFoldSegS
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    =>          Gamma            aenv
    ->          IRFun2    Native aenv (e -> e -> e)
    -> Maybe   (IRExp     Native aenv e)
    ->          IRDelayed Native aenv (Array (sh :. Int) e)
    ->          IRDelayed Native aenv (Segments i)
    -> CodeGen (IROpenAcc Native aenv (Array (sh :. Int) e))
mkFoldSegS aenv combine mseed arr seg =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array (sh :. Int) e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "foldSegS" (paramGang ++ paramOut ++ paramEnv) $ do

    -- Number of segments, useful only if reducing DIM2 and higher
    ss <- indexHead <$> delayedExtent seg

    let test si = A.lt scalarType (A.fst si) end
        initial = A.pair start (lift 0)

        body :: IR (Int,Int) -> CodeGen (IR (Int,Int))
        body (A.unpair -> (s,inf)) = do
          -- We can avoid an extra division if this is a DIM1 array. Higher
          -- dimensional reductions need to wrap around the segment array at
          -- each new lower-dimensional index.
          s'  <- case rank (undefined::sh) of
                   0 -> return s
                   _ -> A.rem integralType s ss

          len <- A.fromIntegral integralType numType =<< app1 (delayedLinearIndex seg) s'
          sup <- A.add numType inf len

          r   <- case mseed of
                   Just seed -> do z <- seed
                                   reduceFromTo  inf sup (app2 combine) z (app1 (delayedLinearIndex arr))
                   Nothing   ->    reduce1FromTo inf sup (app2 combine)   (app1 (delayedLinearIndex arr))
          writeArray arrOut s r

          t <- A.add numType s (lift 1)
          return $ A.pair t sup

    void $ while test body initial
    return_


-- This implementation assumes that the segments array represents the offset
-- indices to the source array, rather than the lengths of each segment. The
-- segment-offset approach is required for parallel implementations.
--
mkFoldSegP
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    =>          Gamma            aenv
    ->          IRFun2    Native aenv (e -> e -> e)
    -> Maybe   (IRExp     Native aenv e)
    ->          IRDelayed Native aenv (Array (sh :. Int) e)
    ->          IRDelayed Native aenv (Segments i)
    -> CodeGen (IROpenAcc Native aenv (Array (sh :. Int) e))
mkFoldSegP aenv combine mseed arr seg =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array (sh :. Int) e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "foldSegP" (paramGang ++ paramOut ++ paramEnv) $ do

    -- Number of segments and size of the innermost dimension. These are
    -- required if we are reducing a DIM2 or higher array, to properly compute
    -- the start and end indices of the portion of the array to reduce. Note
    -- that this is a segment-offset array computed by 'scanl (+) 0' of the
    -- segment length array, so its size has increased by one.
    sz <- indexHead <$> delayedExtent arr
    ss <- do n <- indexHead <$> delayedExtent seg
             A.sub numType n (lift 1)

    imapFromTo start end $ \s -> do

      i   <- case rank (undefined::sh) of
               0 -> return s
               _ -> A.rem integralType s ss
      j   <- A.add numType i (lift 1)
      u   <- A.fromIntegral integralType numType =<< app1 (delayedLinearIndex seg) i
      v   <- A.fromIntegral integralType numType =<< app1 (delayedLinearIndex seg) j

      (inf,sup) <- A.unpair <$> case rank (undefined::sh) of
                     0 -> return (A.pair u v)
                     _ -> do q <- A.quot integralType s ss
                             a <- A.mul numType q sz
                             A.pair <$> A.add numType u a <*> A.add numType v a

      r   <- case mseed of
               Just seed -> do z <- seed
                               reduceFromTo  inf sup (app2 combine) z (app1 (delayedLinearIndex arr))
               Nothing   ->    reduce1FromTo inf sup (app2 combine)   (app1 (delayedLinearIndex arr))

      writeArray arrOut s r

    return_

