{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Scan
-- Copyright   : [2014..2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Scan
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Loop
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )

import Control.Applicative
import Control.Monad
import Prelude                                                      as P


-- 'Data.List.scanl' style left-to-right exclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation.
--
-- > scanl (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 11) [10,10,11,13,16,20,25,31,38,46,55]
--
mkScanl
    :: forall aenv e. Elt e
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRExp     Native aenv e
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e))
mkScanl aenv combine seed arr =
  (+++) <$> mkScanS L aenv combine (Just seed) arr
        <*> mkScanP L aenv combine (Just seed) arr

-- 'Data.List.scanl1' style left-to-right inclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation. The array must not be empty.
--
-- > scanl1 (+) (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 10) [0,1,3,6,10,15,21,28,36,45]
--
mkScanl1
    :: forall aenv e. Elt e
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e))
mkScanl1 aenv combine arr =
  (+++) <$> mkScanS L aenv combine Nothing arr
        <*> mkScanP L aenv combine Nothing arr

-- Variant of 'scanl' where the final result is returned in a separate array.
--
-- > scanr' (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> ( Array (Z :. 10) [10,10,11,13,16,20,25,31,38,46]
--       , Array Z [55]
--       )
--
mkScanl'
    :: forall aenv e. Elt e
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRExp     Native aenv e
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e, Scalar e))
mkScanl' aenv combine seed arr =
  (+++) <$> mkScan'S L aenv combine seed arr
        <*> mkScan'P L aenv combine seed arr


-- 'Data.List.scanr' style right-to-left exclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation.
--
-- > scanr (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 11) [55,55,54,52,49,45,40,34,27,19,10]
--
mkScanr
    :: forall aenv e. Elt e
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRExp     Native aenv e
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e))
mkScanr aenv combine seed arr =
  (+++) <$> mkScanS R aenv combine (Just seed) arr
        <*> mkScanP R aenv combine (Just seed) arr

-- 'Data.List.scanr1' style right-to-left inclusive scan, but with the
-- restriction that the combination function must be associative to enable
-- efficient parallel implementation. The array must not be empty.
--
-- > scanr (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 10) [45,45,44,42,39,35,30,24,17,9]
--
mkScanr1
    :: forall aenv e. Elt e
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e))
mkScanr1 aenv combine arr =
  (+++) <$> mkScanS R aenv combine Nothing arr
        <*> mkScanP R aenv combine Nothing arr

-- Variant of 'scanr' where the final result is returned in a separate array.
--
-- > scanr' (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> ( Array (Z :. 10) [55,54,52,49,45,40,34,27,19,10]
--       , Array Z [55]
--       )
--
mkScanr'
    :: forall aenv e. Elt e
    => Gamma            aenv
    -> IRFun2    Native aenv (e -> e -> e)
    -> IRExp     Native aenv e
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e, Scalar e))
mkScanr' aenv combine seed arr =
  (+++) <$> mkScan'S R aenv combine seed arr
        <*> mkScan'P R aenv combine seed arr


-- Sequentially scan an input array. For inclusive scans we can assume that
-- there is at least one element to the input.
--
mkScanS
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> Maybe (IRExp Native aenv e)
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e))
mkScanS dir aenv combine mseed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
      paramEnv                  = envParam aenv
      --
      cont i                    = case dir of
                                    L -> A.lt  scalarType i end
                                    R -> A.gte scalarType i start

      next i                    = case dir of
                                    L -> A.add numType i (lift 1)
                                    R -> A.sub numType i (lift 1)
  in
  makeOpenAcc "scanS" (paramGang ++ paramOut ++ paramEnv) $ do

    -- index i* is the index that we will read data from. Recall that the
    -- supremum index is exclusive.
    i0 <- case dir of
            L -> return start
            R -> next end

    -- index j* is the index that we write to. Recall that for an exclusive scan
    -- the output array is one larger than the input.
    j0 <- case (dir, mseed) of
            (R,Just _) -> return end
            _          -> return i0   -- merge 'i' and 'j' whenever we can

    -- Evaluate or read the initial element. Update the read-from index
    -- appropriately.
    (v0,i1) <- case mseed of
                 Just seed -> (,) <$> seed <*> return i0
                 Nothing   -> (,) <$> app1 delayedLinearIndex i0
                                  <*> next i0

    -- Write first element, then continue looping through the rest
    writeArray arrOut j0 v0
    j1 <- next j0

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
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> IRExp Native aenv e
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e, Scalar e))
mkScan'S dir aenv combine seed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
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
  makeOpenAcc "scanS" (paramGang ++ paramOut ++ paramSum ++ paramEnv) $ do

    -- index to read data from
    i0 <- case dir of
            L -> return start
            R -> next end

    -- initial element
    v0 <- seed

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
    writeArray arrSum (lift 0 :: IR Int) (A.snd r)

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
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "scanP" (paramGang ++ paramOut ++ paramEnv) $ do
    return_

mkScan'P
    :: forall aenv e. Elt e
    => Direction
    -> Gamma aenv
    -> IRFun2 Native aenv (e -> e -> e)
    -> IRExp Native aenv e
    -> IRDelayed Native aenv (Vector e)
    -> CodeGen (IROpenAcc Native aenv (Vector e, Scalar e))
mkScan'P dir aenv combine seed IRDelayed{..} =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Vector e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc "scanP" (paramGang ++ paramOut ++ paramEnv) $ do
    return_


-- Utilities
-- ---------

data Direction = L | R
  deriving (Eq, Show)

