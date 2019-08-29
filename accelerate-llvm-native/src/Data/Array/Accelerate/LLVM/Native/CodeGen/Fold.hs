{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Fold
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Fold
  where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Generate
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )

import Control.Applicative
import Prelude                                                      as P hiding ( length )


-- Reduce a (possibly empty) array along the innermost dimension. The reduction
-- function must be associative to allow for an efficient parallel
-- implementation. The initial element does not need to be a neutral element of
-- the operator.
--
mkFold
    :: forall aenv sh e. (Shape sh, Elt e)
    => UID
    -> Gamma             aenv
    -> IRFun2     Native aenv (e -> e -> e)
    -> IRExp      Native aenv e
    -> MIRDelayed Native aenv (Array (sh :. Int) e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array sh e))
mkFold uid aenv f z arr
  | Just Refl <- matchShapeType @sh @Z
  = (+++) <$> mkFoldAll  uid aenv f (Just z) arr
          <*> mkFoldFill uid aenv z

  | otherwise
  = (+++) <$> mkFoldDim  uid aenv f (Just z) arr
          <*> mkFoldFill uid aenv z


-- Reduce a non-empty array along the innermost dimension. The reduction
-- function must be associative to allow for efficient parallel implementation.
--
mkFold1
    :: forall aenv sh e. (Shape sh, Elt e)
    => UID
    -> Gamma             aenv
    -> IRFun2     Native aenv (e -> e -> e)
    -> MIRDelayed Native aenv (Array (sh :. Int) e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array sh e))
mkFold1 uid aenv f arr
  | Just Refl <- matchShapeType @sh @Z
  = mkFoldAll uid aenv f Nothing arr

  | otherwise
  = mkFoldDim uid aenv f Nothing arr


-- Reduce a multidimensional (>1) array along the innermost dimension.
--
-- For simplicity, each element of the output (reduction along the entire length
-- of an innermost-dimension index) is computed by a single thread.
--
mkFoldDim
  :: forall aenv sh e. (Shape sh, Elt e)
  => UID
  -> Gamma aenv
  -> IRFun2     Native aenv (e -> e -> e)
  -> MIRExp     Native aenv e
  -> MIRDelayed Native aenv (Array (sh :. Int) e)
  -> CodeGen    Native      (IROpenAcc Native aenv (Array sh e))
mkFoldDim uid aenv combine mseed mdelayed =
  let
      (start, end, paramGang) = gangParam    @DIM1
      (arrOut, paramOut)      = mutableArray @sh "out"
      (arrIn,  paramIn)       = delayedArray     "in"  mdelayed
      paramEnv                = envParam aenv
  in
  makeOpenAcc uid "fold" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do

    sz <- indexHead <$> delayedExtent arrIn

    imapFromTo (indexHead start) (indexHead end) $ \seg -> do
      from <- mul numType seg  sz
      to   <- add numType from sz
      --
      r    <- case mseed of
                Just seed -> do z <- seed
                                reduceFromTo  from to (app2 combine) z (app1 (delayedLinearIndex arrIn))
                Nothing   ->    reduce1FromTo from to (app2 combine)   (app1 (delayedLinearIndex arrIn))
      writeArray arrOut seg r
    return_


-- Reduce an array to single element.
--
-- Since reductions consume arrays that have been fused into them,
-- a parallel fold requires two passes. At an example, take vector dot
-- product:
--
-- > dotp xs ys = fold (+) 0 (zipWith (*) xs ys)
--
--   1. The first pass reads in the fused array data, in this case corresponding
--   to the function (\i -> (xs!i) * (ys!i)).
--
--   2. The second pass reads in the manifest array data from the first step and
--   directly reduces the array. This second step should be small and so is
--   usually just done by a single core.
--
-- Note that the first step is split into two kernels, the second of which
-- reads a carry-in value of that thread's partial reduction, so that
-- threads can still participate in work-stealing. These kernels must not
-- be invoked over empty ranges.
--
-- The final step is sequential reduction of the partial results. If this
-- is an exclusive reduction, the seed element is included at this point.
--
mkFoldAll
    :: forall aenv e. Elt e
    => UID
    -> Gamma aenv                                   -- ^ array environment
    -> IRFun2     Native aenv (e -> e -> e)         -- ^ combination function
    -> MIRExp     Native aenv e                     -- ^ seed element, if this is an exclusive reduction
    -> MIRDelayed Native aenv (Vector e)            -- ^ input data
    -> CodeGen    Native      (IROpenAcc Native aenv (Scalar e))
mkFoldAll uid aenv combine mseed mdelayed =
  foldr1 (+++) <$> sequence [ mkFoldAllS  uid aenv combine mseed mdelayed
                            , mkFoldAllP1 uid aenv combine       mdelayed
                            , mkFoldAllP2 uid aenv combine mseed
                            ]


-- Sequential reduction of an entire array to a single element
--
mkFoldAllS
    :: forall aenv e. Elt e
    => UID
    -> Gamma aenv                                   -- ^ array environment
    -> IRFun2     Native aenv (e -> e -> e)         -- ^ combination function
    -> MIRExp     Native aenv e                     -- ^ seed element, if this is an exclusive reduction
    -> MIRDelayed Native aenv (Vector e)            -- ^ input data
    -> CodeGen    Native      (IROpenAcc Native aenv (Scalar e))
mkFoldAllS uid aenv combine mseed mdelayed  =
  let
      (start, end, paramGang) = gangParam    @DIM1
      (arrOut, paramOut)      = mutableArray @DIM0 "out"
      (arrIn,  paramIn)       = delayedArray @DIM1 "in" mdelayed
      paramEnv                = envParam aenv
      zero                    = lift 0 :: IR Int
  in
  makeOpenAcc uid "foldAllS" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do
    r <- case mseed of
           Just seed -> do z <- seed
                           reduceFromTo  (indexHead start) (indexHead end) (app2 combine) z (app1 (delayedLinearIndex arrIn))
           Nothing   ->    reduce1FromTo (indexHead start) (indexHead end) (app2 combine)   (app1 (delayedLinearIndex arrIn))
    writeArray arrOut zero r
    return_

-- Parallel reduction of an entire array to a single element, step 1.
--
-- Threads reduce each stripe of the input into a temporary array, incorporating
-- any fused functions on the way.
--
mkFoldAllP1
    :: forall aenv e. Elt e
    => UID
    -> Gamma            aenv                        -- ^ array environment
    -> IRFun2     Native aenv (e -> e -> e)         -- ^ combination function
    -> MIRDelayed Native aenv (Vector e)            -- ^ input data
    -> CodeGen    Native      (IROpenAcc Native aenv (Scalar e))
mkFoldAllP1 uid aenv combine mdelayed =
  let
      (start, end, paramGang) = gangParam    @DIM1
      (arrTmp, paramTmp)      = mutableArray @DIM1 "tmp"
      (arrIn,  paramIn)       = delayedArray @DIM1 "in" mdelayed
      piece                   = local     @Int "ix.piece"
      paramPiece              = parameter @Int "ix.piece"
      paramEnv                = envParam aenv
  in
  makeOpenAcc uid "foldAllP1" (paramGang ++ paramPiece ++ paramTmp ++ paramIn ++ paramEnv) $ do

    -- A thread reduces a sequential (non-empty) stripe of the input and stores
    -- that value into a temporary array at a specific index. This method thus
    -- supports non-commutative operators because the order of operations
    -- remains left-to-right.
    --
    r <- reduce1FromTo (indexHead start) (indexHead end) (app2 combine) (app1 (delayedLinearIndex arrIn))
    writeArray arrTmp piece r

    return_

-- Parallel reduction of an entire array to a single element, step 2.
--
-- A single thread reduces the temporary array to a single element.
--
-- During execution, we choose a stripe size in phase 1 so that the temporary is
-- small-ish and thus suitable for sequential reduction. An alternative would be
-- to keep the stripe size constant and, for if the partial reductions array is
-- large, continuing reducing it in parallel.
--
mkFoldAllP2
    :: forall aenv e. Elt e
    => UID
    -> Gamma          aenv                          -- ^ array environment
    -> IRFun2  Native aenv (e -> e -> e)            -- ^ combination function
    -> MIRExp  Native aenv e                        -- ^ seed element, if this is an exclusive reduction
    -> CodeGen Native      (IROpenAcc Native aenv (Scalar e))
mkFoldAllP2 uid aenv combine mseed =
  let
      (start, end, paramGang) = gangParam    @DIM1
      (arrTmp, paramTmp)      = mutableArray @DIM1 "tmp"
      (arrOut, paramOut)      = mutableArray @DIM0 "out"
      paramEnv                = envParam aenv
      zero                    = lift @Int 0
  in
  makeOpenAcc uid "foldAllP2" (paramGang ++ paramTmp ++ paramOut ++ paramEnv) $ do
    r <- case mseed of
           Just seed -> do z <- seed
                           reduceFromTo  (indexHead start) (indexHead end) (app2 combine) z (readArray arrTmp)
           Nothing   ->    reduce1FromTo (indexHead start) (indexHead end) (app2 combine)   (readArray arrTmp)
    writeArray arrOut zero r
    return_


-- Exclusive reductions over empty arrays (of any dimension) fill the lower
-- dimensions with the initial element
--
mkFoldFill
    :: (Shape sh, Elt e)
    => UID
    -> Gamma aenv
    -> IRExp   Native aenv e
    -> CodeGen Native      (IROpenAcc Native aenv (Array sh e))
mkFoldFill uid aenv seed =
  mkGenerate uid aenv (IRFun1 (const seed))

-- Reduction loops
-- ---------------

-- Reduction of a (possibly empty) index space.
--
reduceFromTo
    :: Elt a
    => IR Int                                       -- ^ starting index
    -> IR Int                                       -- ^ final index (exclusive)
    -> (IR a -> IR a -> CodeGen Native (IR a))      -- ^ combination function
    -> IR a                                         -- ^ initial value
    -> (IR Int -> CodeGen Native (IR a))            -- ^ function to retrieve element at index
    -> CodeGen Native (IR a)
reduceFromTo m n f z get =
  iterFromTo m n z $ \i acc -> do
    x <- get i
    y <- f acc x
    return y

-- Reduction of an array over a _non-empty_ index space. The array must
-- contain at least one element.
--
reduce1FromTo
    :: Elt a
    => IR Int                                       -- ^ starting index
    -> IR Int                                       -- ^ final index
    -> (IR a -> IR a -> CodeGen Native (IR a))      -- ^ combination function
    -> (IR Int -> CodeGen Native (IR a))            -- ^ function to retrieve element at index
    -> CodeGen Native (IR a)
reduce1FromTo m n f get = do
  z  <- get m
  m1 <- add numType m (ir numType (num numType 1))
  reduceFromTo m1 n f z get

