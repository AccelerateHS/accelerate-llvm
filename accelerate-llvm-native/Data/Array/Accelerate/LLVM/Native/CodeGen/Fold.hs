{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Fold
-- Copyright   : [2014..2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Fold
  where

import Data.Typeable

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.Native.Target                         ( Native )
import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop


-- Reduce an array along the innermost dimension.
--
mkFold
    :: forall aenv sh e. (Shape sh, Elt e)
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRExp     Native aenv e
    -> IRDelayed Native aenv (Array (sh :. Int) e)
    -> CodeGen (IROpenAcc Native aenv (Array sh e))
mkFold aenv f z acc
  | Just REFL <- matchShapeType (undefined::sh) (undefined::Z)
  = mkFoldAll aenv f (Just z) acc

  | otherwise
  = mkFold' aenv f (Just z) acc


mkFold1
    :: forall aenv sh e. (Shape sh, Elt e)
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRDelayed Native aenv (Array (sh :. Int) e)
    -> CodeGen (IROpenAcc Native aenv (Array sh e))
mkFold1 aenv f acc
  | Just REFL <- matchShapeType (undefined::sh) (undefined::Z)
  = mkFoldAll aenv f Nothing acc

  | otherwise
  = mkFold' aenv f Nothing acc


mkFold'
  :: forall aenv sh e. (Shape sh, Elt e)
  =>          Gamma            aenv
  ->          IRFun2    Native aenv (e -> e -> e)
  -> Maybe   (IRExp     Native aenv e)
  ->          IRDelayed Native aenv (Array (sh :. Int) e)
  -> CodeGen (IROpenAcc Native aenv (Array sh e))
mkFold' aenv combine mseed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh e))
      paramEnv                  = envParam aenv
      --
      name                      = "ix.stride" :: Name Int
      paramStride               = scalarParameter scalarType name
      stride                    = local           scalarType name
  in
  makeOpenAcc "fold" (paramGang ++ paramStride : paramOut ++ paramEnv) $ do

    imapFromTo start end $ \seg -> do
      from <- mul numType seg  stride
      to   <- add numType from stride
      --
      r    <- case mseed of
                Just seed -> do z <- seed
                                reduceFromTo  from to (app2 combine) z (app1 delayedLinearIndex)
                Nothing   ->    reduce1FromTo from to (app2 combine)   (app1 delayedLinearIndex)
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
--   1. The first pass reads in the fused array data, in this case
--   corresponding to the function (\i -> (xs!i) * (ys!i)).
--
--   2. The second pass reads in the manifest array data from the first
--   step and directly reduces the array. This second step should be small
--   (one element per thread) and so is just done by a single core.
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
    =>          Gamma            aenv                           -- ^ array environment
    ->          IRFun2    Native aenv (e -> e -> e)             -- ^ combination function
    -> Maybe   (IRExp     Native aenv e)                        -- ^ seed element, if this is an exclusive reduction
    ->          IRDelayed Native aenv (Vector e)                -- ^ input data
    -> CodeGen (IROpenAcc Native aenv (Scalar e))
mkFoldAll aenv combine mseed IRDelayed{..} = do
  let
      (start, end, paramGang)   = gangParam
      (tid, paramId)            = gangId
      (arrOut,  paramOut)       = mutableArray ("out"  :: Name (Scalar e))
      (arrTmp,  paramTmp)       = mutableArray ("tmp"  :: Name (Vector e))
      (arrFlag, paramFlag)      = mutableArray ("flag" :: Name (Vector Word8))
      paramEnv                  = envParam aenv
      --
      zero                      = ir numType (num numType 0)
      one                       = ir numType (num numType 1)

  -- Sequential reduction
  -- --------------------
  s1 <- makeKernel "foldAllS" (paramGang ++ paramOut ++ paramEnv) $ do
          r <- case mseed of
                 Just seed -> do z <- seed
                                 reduceFromTo  zero end (app2 combine) z (app1 delayedLinearIndex)
                 Nothing   ->    reduce1FromTo zero end (app2 combine)   (app1 delayedLinearIndex)
          writeArray arrOut zero r
          return_

  -- Parallel reduction
  -- ------------------
  --
  -- Step 1: Threads participate in a parallel reduction of the input array
  --
  p1 <- makeKernel "foldAllP1" (paramGang ++ paramId ++ paramTmp ++ paramEnv) $ do
          r <- reduce1FromTo start end (app2 combine) (app1 delayedLinearIndex)
          writeArray arrTmp tid r
          return_

  -- Step 2: Threads participate in parallel reduction of the input array,
  --         but additionally carry in a temporary value from step1. This
  --         enables work-stealing over the main reduction loop.
  --
  p2 <- makeKernel "foldAllP2" (paramGang ++ paramId ++ paramTmp ++ paramEnv) $ do
          c <- readArray arrTmp tid
          r <- reduceFromTo start end (app2 combine) c (app1 delayedLinearIndex)
          writeArray arrTmp tid r
          return_

  -- Step 3: A single thread combines the partial results from all the
  -- threads. If this is an exclusive reduction, this is the time to
  -- include the seed element.
  --
  -- We also need to be careful to only include those local sums that were
  -- actually initialised, as marked by the flag parameter. This is an awkward
  -- race condition in the scheduler, where a thread might be assigned some work
  -- to do, but that is stolen before it gets a chance to execute it. If this
  -- becomes a problem with other parallel operations, we will need to build
  -- a better solution.
  --
  p3 <- makeKernel "foldAllP3" (paramGang ++ paramTmp ++ paramFlag ++ paramOut ++ paramEnv) $ do
          let
              -- Add any initialised elements to the given initial value
              reduceFromToIf m n seed = do
                z <- seed
                r <- iterFromTo m n z $ \i acc ->
                       ifThenElse
                         (eq scalarType one =<< readArray arrFlag i)
                         (app2 combine acc  =<< readArray arrTmp i)
                         (return acc)

                -- store result
                writeArray arrOut zero r
                return_
          --
          case mseed of
            Just seed -> reduceFromToIf start end seed
            Nothing   -> do
              loop   <- newBlock "search.top"
              mid    <- newBlock "search.mid"
              exit   <- newBlock "search.exit"
              _      <- newBlock "search.entry"

              p      <- lt scalarType start end
              top    <- cbr p loop exit
              prev   <- fresh

              -- loop over the temporary array, searching for a valid entry, and
              -- break out as soon as we find one.
              setBlock loop
              ok     <- readArray arrFlag prev
              p'     <- eq scalarType ok one
              found  <- cbr p' exit mid

              setBlock mid
              next   <- add numType prev (ir numType (num numType 1))
              p''    <- lt scalarType next end
              bot    <- cbr p'' loop exit
              _      <- phi' loop prev [(start,top), (next, bot)]

              setBlock exit
              start' <- phi [(start,top), (prev,found), (end,bot)]
              next'  <- add numType start' (ir numType (num numType 1))

              -- Assume at least one element was valid
              reduceFromToIf next' end (readArray arrTmp start')

  -- NOTE: The sequential kernel must appear first in the list, so it will
  --       be treated as the default function to execute.
  return $ IROpenAcc [s1,p1,p2,p3]


-- Reduction loops
-- ---------------

-- Reduction of a (possibly empty) index space.
--
reduceFromTo
    :: Elt a
    => IR Int                                   -- ^ starting index
    -> IR Int                                   -- ^ final index (exclusive)
    -> (IR a -> IR a -> CodeGen (IR a))         -- ^ combination function
    -> IR a                                     -- ^ initial value
    -> (IR Int -> CodeGen (IR a))               -- ^ function to retrieve element at index
    -> CodeGen (IR a)
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
    => IR Int                                   -- ^ starting index
    -> IR Int                                   -- ^ final index
    -> (IR a -> IR a -> CodeGen (IR a))         -- ^ combination function
    -> (IR Int -> CodeGen (IR a))               -- ^ function to retrieve element at index
    -> CodeGen (IR a)
reduce1FromTo m n f get = do
  z  <- get m
  m1 <- add numType m (ir numType (num numType 1))
  reduceFromTo m1 n f z get


-- Utilities
-- ---------

-- Match reified shape types
--
matchShapeType
    :: forall sh sh'. (Shape sh, Shape sh')
    => sh
    -> sh'
    -> Maybe (sh :=: sh')
matchShapeType _ _
  | Just REFL <- matchTupleType (eltType (undefined::sh)) (eltType (undefined::sh'))
  = gcast REFL

matchShapeType _ _
  = Nothing

