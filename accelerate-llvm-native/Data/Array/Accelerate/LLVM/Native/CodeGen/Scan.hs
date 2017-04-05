{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Scan
-- Copyright   : [2014..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Scan
  where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Analysis.Match
import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR                        ( IR )
import Data.Array.Accelerate.LLVM.CodeGen.Loop
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Generate
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )

import Control.Applicative
import Control.Monad
import Data.String                                                  ( fromString )
import Data.Coerce                                                  as Safe
import Prelude                                                      as P


data Direction = L | R

-- 'Data.List.scanl' style left-to-right exclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation.
--
-- > scanl (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 11) [10,10,11,13,16,20,25,31,38,46,55]
--
mkScanl
    :: forall aenv sh e. (Shape sh, Elt e)
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRExp     Native aenv e
    -> IRDelayed Native aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc Native aenv (Array (sh:.Int) e))
mkScanl aenv combine seed arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = foldr1 (+++) <$> sequence [ mkScanS L aenv combine (Just seed) arr
                              , mkScanP L aenv combine (Just seed) arr
                              , mkScanFill aenv seed
                              ]
  --
  | otherwise
  = (+++) <$> mkScanS L aenv combine (Just seed) arr
          <*> mkScanFill aenv seed


-- 'Data.List.scanl1' style left-to-right inclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation. The array must not be empty.
--
-- > scanl1 (+) (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 10) [0,1,3,6,10,15,21,28,36,45]
--
mkScanl1
    :: forall aenv sh e. (Shape sh, Elt e)
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRDelayed Native aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc Native aenv (Array (sh:.Int) e))
mkScanl1 aenv combine arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = (+++) <$> mkScanS L aenv combine Nothing arr
          <*> mkScanP L aenv combine Nothing arr
  --
  | otherwise
  = mkScanS L aenv combine Nothing arr


-- Variant of 'scanl' where the final result is returned in a separate array.
--
-- > scanr' (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> ( Array (Z :. 10) [10,10,11,13,16,20,25,31,38,46]
--       , Array Z [55]
--       )
--
mkScanl'
    :: forall aenv sh e. (Shape sh, Elt e)
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRExp     Native aenv e
    -> IRDelayed Native aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc Native aenv (Array (sh:.Int) e, Array sh e))
mkScanl' aenv combine seed arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = foldr1 (+++) <$> sequence [ mkScan'S L aenv combine seed arr
                              , mkScan'P L aenv combine seed arr
                              , mkScan'Fill aenv seed
                              ]
  --
  | otherwise
  = (+++) <$> mkScan'S L aenv combine seed arr
          <*> mkScan'Fill aenv seed


-- 'Data.List.scanr' style right-to-left exclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation.
--
-- > scanr (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 11) [55,55,54,52,49,45,40,34,27,19,10]
--
mkScanr
    :: forall aenv sh e. (Shape sh, Elt e)
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRExp     Native aenv e
    -> IRDelayed Native aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc Native aenv (Array (sh:.Int) e))
mkScanr aenv combine seed arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = foldr1 (+++) <$> sequence [ mkScanS R aenv combine (Just seed) arr
                              , mkScanP R aenv combine (Just seed) arr
                              , mkScanFill aenv seed
                              ]
  --
  | otherwise
  = (+++) <$> mkScanS R aenv combine (Just seed) arr
          <*> mkScanFill aenv seed


-- 'Data.List.scanr1' style right-to-left inclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation. The array must not be empty.
--
-- > scanr (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 10) [45,45,44,42,39,35,30,24,17,9]
--
mkScanr1
    :: forall aenv sh e. (Shape sh, Elt e)
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRDelayed Native aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc Native aenv (Array (sh:.Int) e))
mkScanr1 aenv combine arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = (+++) <$> mkScanS R aenv combine Nothing arr
          <*> mkScanP R aenv combine Nothing arr
  --
  | otherwise
  = mkScanS R aenv combine Nothing arr


-- Variant of 'scanr' where the final result is returned in a separate array.
--
-- > scanr' (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> ( Array (Z :. 10) [55,54,52,49,45,40,34,27,19,10]
--       , Array Z [55]
--       )
--
mkScanr'
    :: forall aenv sh e. (Shape sh, Elt e)
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRExp     Native aenv e
    -> IRDelayed Native aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc Native aenv (Array (sh:.Int) e, Array sh e))
mkScanr' aenv combine seed arr
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = foldr1 (+++) <$> sequence [ mkScan'S R aenv combine seed arr
                              , mkScan'P R aenv combine seed arr
                              , mkScan'Fill aenv seed
                              ]
  --
  | otherwise
  = (+++) <$> mkScan'S R aenv combine seed arr
          <*> mkScan'Fill aenv seed


-- If the innermost dimension of an exclusive scan is empty, then we just fill
-- the result with the seed element.
--
mkScanFill
    :: (Shape sh, Elt e)
    => Gamma aenv
    -> IRExp Native aenv e
    -> CodeGen (IROpenAcc Native aenv (Array sh e))
mkScanFill aenv seed =
  mkGenerate aenv (IRFun1 (const seed))

mkScan'Fill
    :: forall aenv sh e. (Shape sh, Elt e)
    => Gamma aenv
    -> IRExp Native aenv e
    -> CodeGen (IROpenAcc Native aenv (Array (sh:.Int) e, Array sh e))
mkScan'Fill aenv seed =
  Safe.coerce <$> (mkScanFill aenv seed :: CodeGen (IROpenAcc Native aenv (Array sh e)))


-- A single thread sequentially scans along an entire innermost dimension. For
-- inclusive scans we can assume that the innermost-dimension is at least one
-- element.
--
-- Note that we can use this both when there is a single thread, or in parallel
-- where threads are scheduled over the outer dimensions (segments).
--
mkScanS
    :: forall aenv sh e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> Maybe (IRExp Native aenv e)
    -> IRDelayed Native aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc Native aenv (Array (sh:.Int) e))
mkScanS dir aenv combine mseed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array (sh:.Int) e))
      paramEnv                  = envParam aenv
      --
      next i                    = case dir of
                                    L -> A.add numType i (lift 1)
                                    R -> A.sub numType i (lift 1)
  in
  makeOpenAcc "scanS" (paramGang ++ paramOut ++ paramEnv) $ do

    sz    <- indexHead <$> delayedExtent
    szp1  <- A.add numType sz (lift 1)
    szm1  <- A.sub numType sz (lift 1)

    -- loop over each lower-dimensional index (segment)
    imapFromTo start end $ \seg -> do

      -- index i* is the index that we will read data from. Recall that the
      -- supremum index is exclusive
      i0 <- case dir of
              L -> A.mul numType sz seg
              R -> do x <- A.mul numType sz seg
                      y <- A.add numType szm1 x
                      return y

      -- index j* is the index that we write to. Recall that for exclusive scans
      -- the output array inner dimension is one larger than the input.
      j0 <- case mseed of
              Nothing -> return i0        -- merge 'i' and 'j' indices whenever we can
              Just{}  -> case dir of
                           L -> A.mul numType szp1 seg
                           R -> do x <- A.mul numType szp1 seg
                                   y <- A.add numType x sz
                                   return y

      -- Evaluate or read the initial element. Update the read-from index
      -- appropriately.
      (v0,i1) <- case mseed of
                   Just seed -> (,) <$> seed                       <*> pure i0
                   Nothing   -> (,) <$> app1 delayedLinearIndex i0 <*> next i0

      -- Write first element, then continue looping through the rest
      writeArray arrOut j0 v0
      j1 <- next j0

      iz <- case dir of
              L -> A.add numType i0 sz
              R -> A.sub numType i0 sz

      let cont i = case dir of
                     L -> A.lt scalarType i iz
                     R -> A.gt scalarType i iz

      void $ while (cont . A.fst3)
                   (\(A.untrip -> (i,j,v)) -> do
                       u  <- app1 delayedLinearIndex i
                       v' <- case dir of
                               L -> app2 combine v u
                               R -> app2 combine u v
                       writeArray arrOut j v'
                       A.trip <$> next i <*> next j <*> pure v')
                   (A.trip i1 j1 v0)

    return_


mkScan'S
    :: forall aenv sh e. (Shape sh, Elt e)
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> IRExp Native aenv e
    -> IRDelayed Native aenv (Array (sh:.Int) e)
    -> CodeGen (IROpenAcc Native aenv (Array (sh:.Int) e, Array sh e))
mkScan'S dir aenv combine seed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array (sh:.Int) e))
      (arrSum, paramSum)        = mutableArray ("sum" :: Name (Array sh e))
      paramEnv                  = envParam aenv
      --
      next i                    = case dir of
                                    L -> A.add numType i (lift 1)
                                    R -> A.sub numType i (lift 1)
  in
  makeOpenAcc "scanS" (paramGang ++ paramOut ++ paramSum ++ paramEnv) $ do

    sz    <- indexHead <$> delayedExtent
    szm1  <- A.sub numType sz (lift 1)

    -- iterate over each lower-dimensional index (segment)
    imapFromTo start end $ \seg -> do

      -- index to read data from
      i0 <- case dir of
              L -> A.mul numType seg sz
              R -> do x <- A.mul numType sz seg
                      y <- A.add numType x szm1
                      return y

      -- initial element
      v0 <- seed

      iz <- case dir of
              L -> A.add numType i0 sz
              R -> A.sub numType i0 sz

      let cont i  = case dir of
                      L -> A.lt scalarType i iz
                      R -> A.gt scalarType i iz

      -- Loop through the input. Only at the top of the loop to we write the
      -- carry-in value (i.e. value from the last loop iteration) to the output
      -- array. This ensures correct behaviour if the input array was empty.
      r  <- while (cont . A.fst)
                  (\(A.unpair -> (i,v)) -> do
                      writeArray arrOut i v

                      u  <- app1 delayedLinearIndex i
                      v' <- case dir of
                              L -> app2 combine v u
                              R -> app2 combine u v
                      i' <- next i
                      return $ A.pair i' v')
                  (A.pair i0 v0)

      -- write final reduction result
      writeArray arrSum seg (A.snd r)

    return_


mkScanP
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> Maybe (IRExp Native aenv e)
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e))
mkScanP dir aenv combine mseed arr =
  foldr1 (+++) <$> sequence [ mkScanP1 dir aenv combine mseed arr
                            , mkScanP2 dir aenv combine
                            , mkScanP3 dir aenv combine mseed
                            ]

-- Parallel scan, step 1.
--
-- Threads scan a stripe of the input into a temporary array, incorporating the
-- initial element and any fused functions on the way. The final reduction
-- result of this chunk is written to a separate array.
--
mkScanP1
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> Maybe (IRExp Native aenv e)
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e))
mkScanP1 dir aenv combine mseed IRDelayed{..} =
  let
      (chunk, _, paramGang)     = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Vector e))
      paramEnv                  = envParam aenv
      --
      steps                     = local           scalarType ("ix.steps"  :: Name Int)
      paramSteps                = scalarParameter scalarType ("ix.steps"  :: Name Int)
      stride                    = local           scalarType ("ix.stride" :: Name Int)
      paramStride               = scalarParameter scalarType ("ix.stride" :: Name Int)
      --
      next i                    = case dir of
                                    L -> A.add numType i (lift 1)
                                    R -> A.sub numType i (lift 1)
      firstChunk                = case dir of
                                    L -> lift 0
                                    R -> steps
  in
  makeOpenAcc "scanP1" (paramGang ++ paramStride : paramSteps : paramOut ++ paramTmp ++ paramEnv) $ do

    len <- indexHead <$> delayedExtent

    -- A thread scans a non-empty stripe of the input, storing the final
    -- reduction result into a separate array.
    --
    -- For exclusive scans the first chunk must incorporate the initial element
    -- into the input and output, while all other chunks increment their output
    -- index by one.
    inf <- A.mul numType chunk stride
    a   <- A.add numType inf   stride
    sup <- A.min scalarType a  len

    -- index i* is the index that we read data from. Recall that the supremum
    -- index is exclusive
    i0  <- case dir of
             L -> return inf
             R -> next sup

    -- index j* is the index that we write to. Recall that for exclusive scan
    -- the output array is one larger than the input; the first chunk uses
    -- this spot to write the initial element, all other chunks shift by one.
    j0  <- case mseed of
             Nothing -> return i0
             Just _  -> case dir of
                          L -> if A.eq scalarType chunk firstChunk
                                 then return i0
                                 else next i0
                          R -> if A.eq scalarType chunk firstChunk
                                 then return sup
                                 else return i0

    -- Evaluate/read the initial element for this chunk. Update the read-from
    -- index appropriately
    (v0,i1) <- A.unpair <$> case mseed of
                 Just seed -> if A.eq scalarType chunk firstChunk
                                then A.pair <$> seed                       <*> pure i0
                                else A.pair <$> app1 delayedLinearIndex i0 <*> next i0
                 Nothing   ->        A.pair <$> app1 delayedLinearIndex i0 <*> next i0

    -- Write first element
    writeArray arrOut j0 v0
    j1  <- next j0

    -- Continue looping through the rest of the input
    let cont i =
           case dir of
             L -> A.lt  scalarType i sup
             R -> A.gte scalarType i inf

    r   <- while (cont . A.fst3)
                 (\(A.untrip -> (i,j,v)) -> do
                     u  <- app1 delayedLinearIndex i
                     v' <- case dir of
                             L -> app2 combine v u
                             R -> app2 combine u v
                     writeArray arrOut j v'
                     A.trip <$> next i <*> next j <*> pure v')
                 (A.trip i1 j1 v0)

    -- Final reduction result of this chunk
    writeArray arrTmp chunk (A.thd3 r)

    return_


-- Parallel scan, step 2.
--
-- A single thread performs an in-place inclusive scan of the partial block
-- sums. This forms the carry-in value which are added to the stripe partial
-- results in the final step.
--
mkScanP2
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> CodeGen (IROpenAcc Native aenv (Vector e))
mkScanP2 dir aenv combine =
  let
      (start, end, paramGang)   = gangParam
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Vector e))
      paramEnv                  = envParam aenv
      --
      cont i                    = case dir of
                                    L -> A.lt  scalarType i end
                                    R -> A.gte scalarType i start

      next i                    = case dir of
                                    L -> A.add numType i (lift 1)
                                    R -> A.sub numType i (lift 1)
  in
  makeOpenAcc "scanP2" (paramGang ++ paramTmp ++ paramEnv) $ do

    i0 <- case dir of
            L -> return start
            R -> next end

    v0 <- readArray arrTmp i0
    i1 <- next i0

    void $ while (cont . A.fst)
                 (\(A.unpair -> (i,v)) -> do
                    u  <- readArray arrTmp i
                    i' <- next i
                    v' <- case dir of
                            L -> app2 combine v u
                            R -> app2 combine u v
                    writeArray arrTmp i v'
                    return $ A.pair i' v')
                 (A.pair i1 v0)

    return_


-- Parallel scan, step 3.
--
-- Threads combine every element of the partial block results with the carry-in
-- value computed from step 2.
--
-- Note that we launch (chunks-1) threads, because the first chunk does not need
-- extra processing (has no carry-in value).
--
mkScanP3
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> Maybe (IRExp Native aenv e)
    -> CodeGen (IROpenAcc Native aenv (Vector e))
mkScanP3 dir aenv combine mseed =
  let
      (chunk, _, paramGang)     = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Vector e))
      paramEnv                  = envParam aenv
      --
      stride                    = local           scalarType ("ix.stride" :: Name Int)
      paramStride               = scalarParameter scalarType ("ix.stride" :: Name Int)
      --
      next i                    = case dir of
                                    L -> A.add numType i (lift 1)
                                    R -> A.sub numType i (lift 1)
      prev i                    = case dir of
                                    L -> A.sub numType i (lift 1)
                                    R -> A.add numType i (lift 1)
  in
  makeOpenAcc "scanP3" (paramGang ++ paramStride : paramOut ++ paramTmp ++ paramEnv) $ do

    -- Determine which chunk will be carrying in values for. Compute appropriate
    -- start and end indices.
    a     <- case dir of
               L -> next chunk
               R -> pure chunk

    b     <- A.mul numType a stride
    c     <- A.add numType b stride
    d     <- A.min scalarType c (indexHead (irArrayShape arrOut))

    (inf,sup) <- case (dir,mseed) of
                   (L,Just _) -> (,) <$> next b <*> next d
                   _          -> (,) <$> pure b <*> pure d

    -- Carry in value from the previous chunk
    e     <- case dir of
               L -> pure chunk
               R -> prev chunk
    carry <- readArray arrTmp e

    imapFromTo inf sup $ \i -> do
      x <- readArray arrOut i
      y <- case dir of
             L -> app2 combine carry x
             R -> app2 combine x carry
      writeArray arrOut i y

    return_


mkScan'P
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> IRExp Native aenv e
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e, Scalar e))
mkScan'P dir aenv combine seed arr =
  foldr1 (+++) <$> sequence [ mkScan'P1 dir aenv combine seed arr
                            , mkScan'P2 dir aenv combine
                            , mkScan'P3 dir aenv combine
                            ]

-- Parallel scan', step 1
--
-- Threads scan a stripe of the input into a temporary array. Similar to
-- exclusive scan, but since the size of the output array is the same as the
-- input, input and output indices are shifted by one.
--
mkScan'P1
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> IRExp Native aenv e
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e, Scalar e))
mkScan'P1 dir aenv combine seed IRDelayed{..} =
  let
      (chunk, _, paramGang)     = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Vector e))
      paramEnv                  = envParam aenv
      --
      steps                     = local           scalarType ("ix.steps"  :: Name Int)
      paramSteps                = scalarParameter scalarType ("ix.steps"  :: Name Int)
      stride                    = local           scalarType ("ix.stride" :: Name Int)
      paramStride               = scalarParameter scalarType ("ix.stride" :: Name Int)
      --
      next i                    = case dir of
                                    L -> A.add numType i (lift 1)
                                    R -> A.sub numType i (lift 1)

      firstChunk                = case dir of
                                    L -> lift 0
                                    R -> steps
  in
  makeOpenAcc "scanP1" (paramGang ++ paramStride : paramSteps : paramOut ++ paramTmp ++ paramEnv) $ do

    -- Compute the start and end indices for this non-empty chunk of the input.
    --
    len <- indexHead <$> delayedExtent
    inf <- A.mul numType chunk stride
    a   <- A.add numType inf   stride
    sup <- A.min scalarType a  len

    -- index i* is the index that we pull data from.
    i0 <- case dir of
            L -> return inf
            R -> next sup

    -- index j* is the index that we write results to. The first chunk needs to
    -- include the initial element, and all other chunks shift their results
    -- across by one to make space.
    j0      <- if A.eq scalarType chunk firstChunk
                 then pure i0
                 else next i0

    -- Evaluate/read the initial element. Update the read-from index
    -- appropriately.
    (v0,i1) <- A.unpair <$> if A.eq scalarType chunk firstChunk
                              then A.pair <$> seed                       <*> pure i0
                              else A.pair <$> app1 delayedLinearIndex i0 <*> pure j0

    -- Write the first element
    writeArray arrOut j0 v0
    j1 <- next j0

    -- Continue looping through the rest of the input
    let cont i =
           case dir of
             L -> A.lt  scalarType i sup
             R -> A.gte scalarType i inf

    r  <- while (cont . A.fst3)
                (\(A.untrip-> (i,j,v)) -> do
                    u  <- app1 delayedLinearIndex i
                    v' <- case dir of
                            L -> app2 combine v u
                            R -> app2 combine u v
                    writeArray arrOut j v'
                    A.trip <$> next i <*> next j <*> pure v')
                (A.trip i1 j1 v0)

    -- Write the final reduction result of this chunk
    writeArray arrTmp chunk (A.thd3 r)

    return_


-- Parallel scan', step 2
--
-- Identical to mkScanP2, except we store the total scan result into a separate
-- array (rather than discard it).
--
mkScan'P2
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> CodeGen (IROpenAcc Native aenv (Vector e, Scalar e))
mkScan'P2 dir aenv combine =
  let
      (start, end, paramGang)   = gangParam
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Vector e))
      (arrSum, paramSum)        = mutableArray ("sum" :: Name (Scalar e))
      paramEnv                  = envParam aenv
      --
      cont i                    = case dir of
                                    L -> A.lt  scalarType i end
                                    R -> A.gte scalarType i start

      next i                    = case dir of
                                    L -> A.add numType i (lift 1)
                                    R -> A.sub numType i (lift 1)
  in
  makeOpenAcc "scanP2" (paramGang ++ paramSum ++ paramTmp ++ paramEnv) $ do

    i0 <- case dir of
            L -> return start
            R -> next end

    v0 <- readArray arrTmp i0
    i1 <- next i0

    r  <- while (cont . A.fst)
                (\(A.unpair -> (i,v)) -> do
                   u  <- readArray arrTmp i
                   i' <- next i
                   v' <- case dir of
                           L -> app2 combine v u
                           R -> app2 combine u v
                   writeArray arrTmp i v'
                   return $ A.pair i' v')
                (A.pair i1 v0)

    writeArray arrSum (lift 0 :: IR Int) (A.snd r)

    return_


-- Parallel scan', step 3
--
-- Similar to mkScanP3, except that indices are shifted by one since the output
-- array is the same size as the input (despite being an exclusive scan).
--
-- Launch (chunks-1) threads, because the first chunk does not need extra
-- processing.
--
mkScan'P3
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> CodeGen (IROpenAcc Native aenv (Vector e, Scalar e))
mkScan'P3 dir aenv combine =
  let
      (chunk, _, paramGang)     = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
      (arrTmp, paramTmp)        = mutableArray ("tmp" :: Name (Vector e))
      paramEnv                  = envParam aenv
      --
      stride                    = local           scalarType ("ix.stride" :: Name Int)
      paramStride               = scalarParameter scalarType ("ix.stride" :: Name Int)
      --
      next i                    = case dir of
                                    L -> A.add numType i (lift 1)
                                    R -> A.sub numType i (lift 1)
      prev i                    = case dir of
                                    L -> A.sub numType i (lift 1)
                                    R -> A.add numType i (lift 1)
  in
  makeOpenAcc "scanP3" (paramGang ++ paramStride : paramOut ++ paramTmp ++ paramEnv) $ do

    -- Determine which chunk we will be carrying in the values of, and compute
    -- the appropriate start and end indices
    a     <- case dir of
               L -> next chunk
               R -> pure chunk

    b     <- A.mul numType a stride
    c     <- A.add numType b stride
    d     <- A.min scalarType c (indexHead (irArrayShape arrOut))

    inf   <- next b
    sup   <- next d

    -- Carry-value from the previous chunk
    e     <- case dir of
               L -> pure chunk
               R -> prev chunk

    carry <- readArray arrTmp e

    imapFromTo inf sup $ \i -> do
      x <- readArray arrOut i
      y <- case dir of
             L -> app2 combine carry x
             R -> app2 combine x carry
      writeArray arrOut i y

    return_

