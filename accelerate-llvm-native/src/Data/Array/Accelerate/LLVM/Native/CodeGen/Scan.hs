{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Scan
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Scan
  where

import Data.Array.Accelerate.AST                                    ( Direction(..) )
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic                as A
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Loop
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Generate
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop
import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )

import Control.Applicative
import Control.Monad
import Data.String                                                  ( fromString )
import Data.Coerce                                                  as Safe
import Prelude                                                      as P


-- 'Data.List.scanl' or 'Data.List.scanl1' style exclusive scan,
-- but with the restriction that the combination function must be associative
-- to enable efficient parallel implementation.
--
-- > scanl (+) 10 (use $ fromList (Z :. 10) [0..])
-- >
-- > ==> Array (Z :. 11) [10,10,11,13,16,20,25,31,38,46,55]
--
mkScan
    :: UID
    -> Gamma             aenv
    -> ArrayR                   (Array (sh, Int) e)
    -> Direction
    -> IRFun2       Native aenv (e -> e -> e)
    -> Maybe (IRExp Native aenv e)
    -> MIRDelayed   Native aenv (Array (sh, Int) e)
    -> CodeGen      Native      (IROpenAcc Native aenv (Array (sh, Int) e))
mkScan uid aenv aR dir combine seed arr
  = foldr1 (+++) <$> sequence (codeScanS ++ codeScanP ++ codeScanFill)
  where
    codeScanS = [ mkScanS dir uid aenv aR combine seed arr ]
    codeScanP = case aR of
      ArrayR (ShapeRsnoc ShapeRz) eR -> [ mkScanP dir uid aenv eR combine seed arr ]
      _                              -> []
    -- Input can be empty iff a seed is given. We then need to compile a fill kernel
    codeScanFill = case seed of
      Just s  -> [ mkScanFill uid aenv aR s ]
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
    :: UID
    -> Gamma             aenv
    -> ArrayR                 (Array (sh, Int) e)
    -> Direction
    -> IRFun2     Native aenv (e -> e -> e)
    -> IRExp      Native aenv e
    -> MIRDelayed Native aenv (Array (sh, Int) e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array (sh, Int) e, Array sh e))
mkScan' uid aenv aR dir combine seed arr
  | ArrayR (ShapeRsnoc ShapeRz) eR <- aR
  = foldr1 (+++) <$> sequence [ mkScan'S dir uid aenv aR combine seed arr
                              , mkScan'P dir uid aenv eR combine seed arr
                              , mkScan'Fill uid aenv aR seed
                              ]
  --
  | otherwise
  = (+++) <$> mkScan'S dir uid aenv aR combine seed arr
          <*> mkScan'Fill uid aenv aR seed

-- If the innermost dimension of an exclusive scan is empty, then we just fill
-- the result with the seed element.
--
mkScanFill
    :: UID
    -> Gamma          aenv
    -> ArrayR (Array sh e)
    -> IRExp   Native aenv e
    -> CodeGen Native      (IROpenAcc Native aenv (Array sh e))
mkScanFill uid aenv aR seed =
  mkGenerate uid aenv aR (IRFun1 (const seed))

mkScan'Fill
    :: UID
    -> Gamma          aenv
    -> ArrayR (Array (sh, Int) e)
    -> IRExp   Native aenv e
    -> CodeGen Native     (IROpenAcc Native aenv (Array (sh, Int) e, Array sh e))
mkScan'Fill uid aenv aR seed =
  Safe.coerce <$> mkScanFill uid aenv (reduceRank aR) seed


-- A single thread sequentially scans along an entire innermost dimension. For
-- inclusive scans we can assume that the innermost-dimension is at least one
-- element.
--
-- Note that we can use this both when there is a single thread, or in parallel
-- where threads are scheduled over the outer dimensions (segments).
--
mkScanS
    :: Direction
    -> UID
    -> Gamma             aenv
    -> ArrayR (Array (sh, Int) e)
    -> IRFun2     Native aenv (e -> e -> e)
    -> MIRExp     Native aenv e
    -> MIRDelayed Native aenv (Array (sh, Int) e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array (sh, Int) e))
mkScanS dir uid aenv aR combine mseed marr =
  let
      (start, end, paramGang) = gangParam shR
      (arrOut, paramOut)      = mutableArray aR "out"
      (arrIn,  paramIn)       = delayedArray    "in"  marr
      paramEnv                = envParam aenv
      ShapeRsnoc shR          = arrayRshape aR
      --
      next i                  = case dir of
                                  LeftToRight -> A.add numType i (liftInt 1)
                                  RightToLeft -> A.sub numType i (liftInt 1)
  in
  makeOpenAcc uid "scanS" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do

    -- The dimensions of the input and output arrays are (almost) the same
    -- but LLVM can't know that so make it explicit so that we reuse loop
    -- variables and index calculations
    shIn <- delayedExtent arrIn
    let sz    = indexHead shIn
        shOut = case mseed of
                  Nothing -> shIn
                  Just{}  -> indexCons (indexTail shIn) (indexHead (irArrayShape arrOut))

    -- Loop over the outer dimensions
    imapNestFromTo shR start end (indexTail shIn) $ \ix _ -> do

      -- index i* is the index that we will read data from. Recall that the
      -- supremum index is exclusive
      i0 <- case dir of
              LeftToRight -> return (liftInt 0)
              RightToLeft -> A.sub numType sz (liftInt 1)

      -- index j* is the index that we write to. Recall that for exclusive scans
      -- the output array inner dimension is one larger than the input.
      j0 <- case mseed of
              Nothing -> return i0        -- merge 'i' and 'j' indices whenever we can
              Just{}  -> case dir of
                           LeftToRight -> return i0
                           RightToLeft -> return sz

      -- Evaluate or read the initial element. Update the read-from index
      -- appropriately.
      (v0,i1) <- case mseed of
                   Just seed -> (,) <$> seed                                        <*> pure i0
                   Nothing   -> (,) <$> app1 (delayedIndex arrIn) (indexCons ix i0) <*> next i0

      -- Write first element, then continue looping through the rest of
      -- this innermost dimension
      k0 <- intOfIndex (arrayRshape aR) shOut (indexCons ix j0)
      j1 <- next j0
      writeArray TypeInt arrOut k0 v0

      void $ while (TupRunit `TupRpair` TupRsingle scalarTypeInt `TupRpair` TupRsingle scalarTypeInt `TupRpair` arrayRtype aR)
                   (\(A.untrip -> (i,_,_)) -> do
                       case dir of
                         LeftToRight -> A.lt  singleType i sz
                         RightToLeft -> A.gte singleType i (liftInt 0))
                   (\(A.untrip -> (i,j,u)) -> do
                       v <- app1 (delayedIndex arrIn) (indexCons ix i)
                       w <- case dir of
                              LeftToRight -> app2 combine u v
                              RightToLeft -> app2 combine v u
                       k <- intOfIndex (arrayRshape aR) shOut (indexCons ix j)
                       writeArray TypeInt arrOut k w
                       A.trip <$> next i <*> next j <*> pure w)
                   (A.trip i1 j1 v0)

    return_


mkScan'S
    :: Direction
    -> UID
    -> Gamma             aenv
    -> ArrayR (Array (sh, Int) e)
    -> IRFun2     Native aenv (e -> e -> e)
    -> IRExp      Native aenv e
    -> MIRDelayed Native aenv (Array (sh, Int) e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Array (sh, Int) e, Array sh e))
mkScan'S dir uid aenv aR combine seed marr =
  let
      (start, end, paramGang) = gangParam    shR
      (arrOut, paramOut)      = mutableArray aR              "out"
      (arrSum, paramSum)      = mutableArray (reduceRank aR) "sum"
      (arrIn,  paramIn)       = delayedArray "in" marr
      paramEnv                = envParam aenv
      ShapeRsnoc shR          = arrayRshape aR
      --
      next i                  = case dir of
                                  LeftToRight -> A.add numType i (liftInt 1)
                                  RightToLeft -> A.sub numType i (liftInt 1)
  in
  makeOpenAcc uid "scanS" (paramGang ++ paramOut ++ paramSum ++ paramIn ++ paramEnv) $ do

    shIn  <- delayedExtent arrIn
    let sz    = indexHead shIn
        shOut = shIn

    imapNestFromTo shR start end (indexTail shIn) $ \ix ii -> do

      -- index to read data from
      i0 <- case dir of
              LeftToRight -> return (liftInt 0)
              RightToLeft -> A.sub numType sz (liftInt 1)

      -- initial element
      v0 <- seed

      -- Loop through the input. Only at the top of the loop to we write the
      -- carry-in value (i.e. value from the last loop iteration) to the output
      -- array. This ensures correct behaviour if the input array was empty.
      r  <- while (TupRsingle scalarTypeInt `TupRpair` arrayRtype aR)
                  (\(A.unpair -> (i,_)) -> do
                      case dir of
                        LeftToRight -> A.lt  singleType i sz
                        RightToLeft -> A.gte singleType i (liftInt 0))
                  (\(A.unpair -> (i,u)) -> do
                      k <- intOfIndex (arrayRshape aR) shOut (indexCons ix i)
                      writeArray TypeInt arrOut k u

                      v <- app1 (delayedIndex arrIn) (indexCons ix i)
                      w <- case dir of
                             LeftToRight -> app2 combine u v
                             RightToLeft -> app2 combine v u
                      A.pair <$> next i <*> pure w)
                  (A.pair i0 v0)

      writeArray TypeInt arrSum ii (A.snd r)

    return_


mkScanP
    :: Direction
    -> UID
    -> Gamma             aenv
    -> TypeR e
    -> IRFun2     Native aenv (e -> e -> e)
    -> MIRExp     Native aenv e
    -> MIRDelayed Native aenv (Vector e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Vector e))
mkScanP dir uid aenv eR combine mseed marr =
  foldr1 (+++) <$> sequence [ mkScanP1 dir uid aenv eR combine mseed marr
                            , mkScanP2 dir uid aenv eR combine
                            , mkScanP3 dir uid aenv eR combine mseed
                            ]

-- Parallel scan, step 1.
--
-- Threads scan a stripe of the input into a temporary array, incorporating the
-- initial element and any fused functions on the way. The final reduction
-- result of this chunk is written to a separate array.
--
mkScanP1
    :: Direction
    -> UID
    -> Gamma             aenv
    -> TypeR e
    -> IRFun2     Native aenv (e -> e -> e)
    -> MIRExp     Native aenv e
    -> MIRDelayed Native aenv (Vector e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Vector e))
mkScanP1 dir uid aenv eR combine mseed marr =
  let
      (start, end, paramGang) = gangParam    dim1
      (arrOut, paramOut)      = mutableArray (ArrayR dim1 eR) "out"
      (arrTmp, paramTmp)      = mutableArray (ArrayR dim1 eR) "tmp"
      (arrIn,  paramIn)       = delayedArray "in" marr
      paramEnv                = envParam aenv
      --
      steps                   = local     (TupRsingle scalarTypeInt) "ix.steps"
      paramSteps              = parameter (TupRsingle scalarTypeInt) "ix.steps"
      piece                   = local     (TupRsingle scalarTypeInt) "ix.piece"
      paramPiece              = parameter (TupRsingle scalarTypeInt) "ix.piece"
      --
      next i                  = case dir of
                                  LeftToRight -> A.add numType i (liftInt 1)
                                  RightToLeft -> A.sub numType i (liftInt 1)
      firstPiece              = case dir of
                                  LeftToRight -> liftInt 0
                                  RightToLeft -> steps
  in
  makeOpenAcc uid "scanP1" (paramGang ++ paramPiece ++ paramSteps ++ paramOut ++ paramTmp ++ paramIn ++ paramEnv) $ do

    -- A thread scans a non-empty stripe of the input, storing the final
    -- reduction result into a separate array.
    --
    -- For exclusive scans the first chunk must incorporate the initial element
    -- into the input and output, while all other chunks increment their output
    -- index by one.
    --
    -- index i* is the index that we read data from. Recall that the supremum
    -- index is exclusive
    i0  <- case dir of
             LeftToRight -> return (indexHead start)
             RightToLeft -> next (indexHead end)

    -- index j* is the index that we write to. Recall that for exclusive scan
    -- the output array is one larger than the input; the first piece uses
    -- this spot to write the initial element, all other chunks shift by one.
    j0  <- case mseed of
             Nothing -> return i0
             Just _  -> case dir of
                          LeftToRight -> if (TupRsingle scalarTypeInt, A.eq singleType piece firstPiece)
                                         then return i0
                                         else next i0
                          RightToLeft -> if (TupRsingle scalarTypeInt, A.eq singleType piece firstPiece)
                                         then return (indexHead end)
                                         else return i0

    -- Evaluate/read the initial element for this piece. Update the read-from
    -- index appropriately
    (v0,i1) <- A.unpair <$> case mseed of
                 Just seed -> if (eR `TupRpair` TupRsingle scalarTypeInt, A.eq singleType piece firstPiece)
                                then A.pair <$> seed                               <*> pure i0
                                else A.pair <$> app1 (delayedLinearIndex arrIn) i0 <*> next i0
                 Nothing   ->        A.pair <$> app1 (delayedLinearIndex arrIn) i0 <*> next i0

    -- Write first element
    writeArray TypeInt arrOut j0 v0
    j1  <- next j0

    -- Continue looping through the rest of the input
    let cont i =
           case dir of
             LeftToRight -> A.lt  singleType i (indexHead end)
             RightToLeft -> A.gte singleType i (indexHead start)

    r   <- while (TupRunit `TupRpair` TupRsingle scalarTypeInt `TupRpair` TupRsingle scalarTypeInt `TupRpair` eR)
                 (cont . A.fst3)
                 (\(A.untrip -> (i,j,v)) -> do
                     u  <- app1 (delayedLinearIndex arrIn) i
                     v' <- case dir of
                             LeftToRight -> app2 combine v u
                             RightToLeft -> app2 combine u v
                     writeArray TypeInt arrOut j v'
                     A.trip <$> next i <*> next j <*> pure v')
                 (A.trip i1 j1 v0)

    -- Final reduction result of this piece
    writeArray TypeInt arrTmp piece (A.thd3 r)

    return_


-- Parallel scan, step 2.
--
-- A single thread performs an in-place inclusive scan of the partial block
-- sums. This forms the carry-in value which are added to the stripe partial
-- results in the final step.
--
mkScanP2
    :: Direction
    -> UID
    -> Gamma          aenv
    -> TypeR e
    -> IRFun2  Native aenv (e -> e -> e)
    -> CodeGen Native      (IROpenAcc Native aenv (Vector e))
mkScanP2 dir uid aenv eR combine =
  let
      (start, end, paramGang) = gangParam    dim1
      (arrTmp, paramTmp)      = mutableArray (ArrayR dim1 eR) "tmp"
      paramEnv                = envParam aenv
      --
      cont i                  = case dir of
                                  LeftToRight -> A.lt  singleType i (indexHead end)
                                  RightToLeft -> A.gte singleType i (indexHead start)

      next i                  = case dir of
                                  LeftToRight -> A.add numType i (liftInt 1)
                                  RightToLeft -> A.sub numType i (liftInt 1)
  in
  makeOpenAcc uid "scanP2" (paramGang ++ paramTmp ++ paramEnv) $ do

    i0 <- case dir of
            LeftToRight -> return (indexHead start)
            RightToLeft -> next (indexHead end)

    v0 <- readArray TypeInt arrTmp i0
    i1 <- next i0

    void $ while (TupRsingle scalarTypeInt `TupRpair` eR)
                 (cont . A.fst)
                 (\(A.unpair -> (i,v)) -> do
                    u  <- readArray TypeInt arrTmp i
                    i' <- next i
                    v' <- case dir of
                            LeftToRight -> app2 combine v u
                            RightToLeft -> app2 combine u v
                    writeArray TypeInt arrTmp i v'
                    return $ A.pair i' v')
                 (A.pair i1 v0)

    return_


-- Parallel scan, step 3.
--
-- Threads combine every element of the partial block results with the carry-in
-- value computed from step 2.
--
-- Note that first chunk does not need extra processing (has no carry-in value).
--
mkScanP3
    :: Direction
    -> UID
    -> Gamma aenv
    -> TypeR e
    -> IRFun2  Native aenv (e -> e -> e)
    -> MIRExp  Native aenv e
    -> CodeGen Native      (IROpenAcc Native aenv (Vector e))
mkScanP3 dir uid aenv eR combine mseed =
  let
      (start, end, paramGang) = gangParam    dim1
      (arrOut, paramOut)      = mutableArray (ArrayR dim1 eR) "out"
      (arrTmp, paramTmp)      = mutableArray (ArrayR dim1 eR) "tmp"
      paramEnv                = envParam aenv
      --
      steps                   = local     (TupRsingle scalarTypeInt) "ix.steps"
      paramSteps              = parameter (TupRsingle scalarTypeInt) "ix.steps"
      piece                   = local     (TupRsingle scalarTypeInt) "ix.piece"
      paramPiece              = parameter (TupRsingle scalarTypeInt) "ix.piece"
      --
      next i                  = case dir of
                                  LeftToRight -> A.add numType i (liftInt 1)
                                  RightToLeft -> A.sub numType i (liftInt 1)
      prev i                  = case dir of
                                  LeftToRight -> A.sub numType i (liftInt 1)
                                  RightToLeft -> A.add numType i (liftInt 1)
      firstPiece              = case dir of
                                  LeftToRight -> liftInt 0
                                  RightToLeft -> steps
  in
  makeOpenAcc uid "scanP3" (paramGang ++ paramPiece ++ paramSteps ++ paramOut ++ paramTmp ++ paramEnv) $ do

    -- TODO: Don't schedule the "first" piece. In the scheduler this corresponds
    -- to the split range with the smallest/largest linear index for left/right
    -- scans respectively. For right scans this is not necessarily the last piece(?).
    --
    A.when (neq singleType piece firstPiece) $ do

      -- Compute start and end indices, leaving space for the initial element
      (inf,sup) <- case (dir, mseed) of
                     (LeftToRight, Just{}) -> (,) <$> next (indexHead start) <*> next (indexHead end)
                     _                     -> (,) <$> pure (indexHead start) <*> pure (indexHead end)

      -- Read in the carry in value for this piece
      c <- readArray TypeInt arrTmp =<< prev piece

      imapFromTo inf sup $ \i -> do
        x <- readArray TypeInt arrOut i
        y <- case dir of
               LeftToRight -> app2 combine c x
               RightToLeft -> app2 combine x c
        writeArray TypeInt arrOut i y

    return_


mkScan'P
    :: Direction
    -> UID
    -> Gamma             aenv
    -> TypeR e
    -> IRFun2     Native aenv (e -> e -> e)
    -> IRExp      Native aenv e
    -> MIRDelayed Native aenv (Vector e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Vector e, Scalar e))
mkScan'P dir uid aenv eR combine seed arr =
  foldr1 (+++) <$> sequence [ mkScan'P1 dir uid aenv eR combine seed arr
                            , mkScan'P2 dir uid aenv eR combine
                            , mkScan'P3 dir uid aenv eR combine
                            ]

-- Parallel scan', step 1
--
-- Threads scan a stripe of the input into a temporary array. Similar to
-- exclusive scan, the output indices are shifted by one relative to the input
-- indices to make space for the initial element.
--
mkScan'P1
    :: Direction
    -> UID
    -> Gamma             aenv
    -> TypeR e
    -> IRFun2     Native aenv (e -> e -> e)
    -> IRExp      Native aenv e
    -> MIRDelayed Native aenv (Vector e)
    -> CodeGen    Native      (IROpenAcc Native aenv (Vector e, Scalar e))
mkScan'P1 dir uid aenv eR combine seed marr =
  let
      (start, end, paramGang) = gangParam    dim1
      (arrOut, paramOut)      = mutableArray (ArrayR dim1 eR) "out"
      (arrTmp, paramTmp)      = mutableArray (ArrayR dim1 eR) "tmp"
      (arrIn,  paramIn)       = delayedArray "in" marr
      paramEnv                = envParam aenv
      --
      steps                   = local     (TupRsingle scalarTypeInt) "ix.steps"
      paramSteps              = parameter (TupRsingle scalarTypeInt) "ix.steps"
      piece                   = local     (TupRsingle scalarTypeInt) "ix.piece"
      paramPiece              = parameter (TupRsingle scalarTypeInt) "ix.piece"
      --
      next i                  = case dir of
                                  LeftToRight -> A.add numType i (liftInt 1)
                                  RightToLeft -> A.sub numType i (liftInt 1)

      firstPiece              = case dir of
                                  LeftToRight -> liftInt 0
                                  RightToLeft -> steps
  in
  makeOpenAcc uid "scanP1" (paramGang ++ paramPiece ++ paramSteps ++ paramOut ++ paramTmp ++ paramIn ++ paramEnv) $ do

    -- index i* is the index that we pull data from.
    i0 <- case dir of
            LeftToRight -> return (indexHead start)
            RightToLeft -> next (indexHead end)

    -- index j* is the index that we write results to. The first piece needs to
    -- include the initial element, and all other chunks shift their results
    -- across by one to make space.
    j0      <- if (TupRsingle scalarTypeInt, A.eq singleType piece firstPiece)
                 then pure i0
                 else next i0

    -- Evaluate/read the initial element. Update the read-from index
    -- appropriately.
    (v0,i1) <- A.unpair <$> if (eR `TupRpair` TupRsingle scalarTypeInt, A.eq singleType piece firstPiece)
                              then A.pair <$> seed                               <*> pure i0
                              else A.pair <$> app1 (delayedLinearIndex arrIn) i0 <*> pure j0

    -- Write the first element
    writeArray TypeInt arrOut j0 v0
    j1 <- next j0

    -- Continue looping through the rest of the input
    let cont i =
           case dir of
             LeftToRight -> A.lt  singleType i (indexHead end)
             RightToLeft -> A.gte singleType i (indexHead start)

    r  <- while (TupRunit `TupRpair` TupRsingle scalarTypeInt `TupRpair` TupRsingle scalarTypeInt `TupRpair` eR)
                (cont . A.fst3)
                (\(A.untrip-> (i,j,v)) -> do
                    u  <- app1 (delayedLinearIndex arrIn) i
                    v' <- case dir of
                            LeftToRight -> app2 combine v u
                            RightToLeft -> app2 combine u v
                    writeArray TypeInt arrOut j v'
                    A.trip <$> next i <*> next j <*> pure v')
                (A.trip i1 j1 v0)

    -- Write the final reduction result of this piece
    writeArray TypeInt arrTmp piece (A.thd3 r)

    return_


-- Parallel scan', step 2
--
-- Identical to mkScanP2, except we store the total scan result into a separate
-- array (rather than discard it).
--
mkScan'P2
    :: Direction
    -> UID
    -> Gamma          aenv
    -> TypeR e
    -> IRFun2  Native aenv (e -> e -> e)
    -> CodeGen Native      (IROpenAcc Native aenv (Vector e, Scalar e))
mkScan'P2 dir uid aenv eR combine =
  let
      (start, end, paramGang) = gangParam    dim1
      (arrTmp, paramTmp)      = mutableArray (ArrayR dim1 eR) "tmp"
      (arrSum, paramSum)      = mutableArray (ArrayR dim0 eR) "sum"
      paramEnv                = envParam aenv
      --
      cont i                  = case dir of
                                  LeftToRight -> A.lt  singleType i (indexHead end)
                                  RightToLeft -> A.gte singleType i (indexHead start)

      next i                  = case dir of
                                  LeftToRight -> A.add numType i (liftInt 1)
                                  RightToLeft -> A.sub numType i (liftInt 1)
  in
  makeOpenAcc uid "scanP2" (paramGang ++ paramSum ++ paramTmp ++ paramEnv) $ do

    i0 <- case dir of
            LeftToRight -> return (indexHead start)
            RightToLeft -> next (indexHead end)

    v0 <- readArray TypeInt arrTmp i0
    i1 <- next i0

    r  <- while (TupRpair (TupRsingle scalarTypeInt) eR)
                (cont . A.fst)
                (\(A.unpair -> (i,v)) -> do
                   u  <- readArray TypeInt arrTmp i
                   i' <- next i
                   v' <- case dir of
                           LeftToRight -> app2 combine v u
                           RightToLeft -> app2 combine u v
                   writeArray TypeInt arrTmp i v'
                   return $ A.pair i' v')
                (A.pair i1 v0)

    writeArray TypeInt arrSum (liftInt 0) (A.snd r)

    return_


-- Parallel scan', step 3
--
-- Similar to mkScanP3, except that indices are shifted by one since the output
-- array is the same size as the input (despite being an exclusive scan).
--
-- Note that the first chunk does not need to do any extra processing (has no
-- carry-in value).
--
mkScan'P3
    :: Direction
    -> UID
    -> Gamma          aenv
    -> TypeR e
    -> IRFun2  Native aenv (e -> e -> e)
    -> CodeGen Native      (IROpenAcc Native aenv (Vector e, Scalar e))
mkScan'P3 dir uid aenv eR combine =
  let
      (start, end, paramGang) = gangParam    dim1
      (arrOut, paramOut)      = mutableArray (ArrayR dim1 eR) "out"
      (arrTmp, paramTmp)      = mutableArray (ArrayR dim1 eR) "tmp"
      paramEnv                = envParam aenv
      --
      steps                   = local     (TupRsingle scalarTypeInt) "ix.steps"
      paramSteps              = parameter (TupRsingle scalarTypeInt) "ix.steps"
      piece                   = local     (TupRsingle scalarTypeInt) "ix.piece"
      paramPiece              = parameter (TupRsingle scalarTypeInt) "ix.piece"
      --
      next i                  = case dir of
                                  LeftToRight -> A.add numType i (liftInt 1)
                                  RightToLeft -> A.sub numType i (liftInt 1)
      prev i                  = case dir of
                                  LeftToRight -> A.sub numType i (liftInt 1)
                                  RightToLeft -> A.add numType i (liftInt 1)
      firstPiece              = case dir of
                                  LeftToRight -> liftInt 0
                                  RightToLeft -> steps
  in
  makeOpenAcc uid "scanP3" (paramGang ++ paramPiece ++ paramSteps ++ paramOut ++ paramTmp ++ paramEnv) $ do

    -- TODO: don't schedule the "first" piece.
    --
    A.when (neq singleType piece firstPiece) $ do

      -- Compute start and end indices, leaving space for the initial element
      inf <- next (indexHead start)
      sup <- next (indexHead end)

      -- Read the carry-in value for this piece
      c   <- readArray TypeInt arrTmp =<< prev piece

      -- Apply the carry-in value to all elements of the output
      imapFromTo inf sup $ \i -> do
        x <- readArray TypeInt arrOut i
        y <- case dir of
               LeftToRight -> app2 combine c x
               RightToLeft -> app2 combine x c
        writeArray TypeInt arrOut i y

    return_

