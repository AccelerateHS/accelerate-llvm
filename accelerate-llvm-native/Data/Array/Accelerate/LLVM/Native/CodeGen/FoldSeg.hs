{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.FoldSeg
-- Copyright   : [2014..2015] Trevor L. McDonell
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
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Fold
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )


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
  mkFoldSegP aenv combine (Just seed) arr seg


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
  mkFoldSegP aenv combine Nothing arr seg


-- This implementation assumes that the segments array represents the offset
-- indices to the source array, rather than the lengths of each segment. The
-- segment-offset approach is required for parallel implementations.
--
mkFoldSegP
    :: forall aenv sh i e. (Shape sh, IsIntegral i, Elt i, Elt e)
    =>        Gamma            aenv
    ->        IRFun2    Native aenv (e -> e -> e)
    -> Maybe (IRExp     Native aenv e)
    ->        IRDelayed Native aenv (Array (sh :. Int) e)
    ->        IRDelayed Native aenv (Segments i)
    -> CodeGen (IROpenAcc Native aenv (Array (sh :. Int) e))
mkFoldSegP aenv combine mseed arr seg =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "foldSegP" (paramGang ++ paramOut ++ paramEnv) $ do

    imapFromTo start end $ \i -> do
      j   <- A.add numType i (lift 1)
      inf <- A.fromIntegral integralType numType =<< app1 (delayedLinearIndex seg) i
      sup <- A.fromIntegral integralType numType =<< app1 (delayedLinearIndex seg) j

      r   <- case mseed of
               Just seed -> do z <- seed
                               reduceFromTo  inf sup (app2 combine) z (app1 (delayedLinearIndex arr))
               Nothing   ->    reduce1FromTo inf sup (app2 combine)   (app1 (delayedLinearIndex arr))
      writeArray arrOut i r

    return_

