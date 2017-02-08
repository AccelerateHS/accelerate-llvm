{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute (

  Execute(..), Gamma,
  executeAcc, executeAfun1, executeSeq

) where

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Lifted                       ( LiftedType(..), LiftedTupleType(..), Segments )
import Data.Array.Accelerate.Array.Representation               ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                        hiding ( Foreign, Segments )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Interpreter                        ( evalPrim, evalPrimConst, evalPrj )
import qualified Data.Array.Accelerate.Array.Sugar              as S
import qualified Data.Array.Accelerate.Array.Representation     as R
import qualified Data.Array.Accelerate.Debug                    as Debug

import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Foreign
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )

import Data.Array.Accelerate.LLVM.Execute.Async                 hiding ( join )
import Data.Array.Accelerate.LLVM.Execute.Environment
import Data.Array.Accelerate.LLVM.Execute.Schedule

-- library
import Control.Monad                                            ( join )
import Control.Monad.Trans                                      ( liftIO )
import Control.Applicative                                      hiding ( Const )
import Data.Traversable                                         ( sequenceA, mapM )
import Data.Typeable                                            ( eqT )
import System.IO.Unsafe                                         ( unsafePerformIO )
import Prelude                                                  hiding ( exp, map, unzip, scanl, scanr, scanl1, scanr1, mapM )


class (Remote arch, Foreign arch) => Execute arch where
  map           :: (Shape sh, Elt b)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh
                -> LLVM arch (Array sh b)

  generate      :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh
                -> LLVM arch (Array sh e)

  transform     :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh
                -> LLVM arch (Array sh e)

  backpermute   :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh
                -> LLVM arch (Array sh e)

  fold          :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh :. Int
                -> LLVM arch (Array sh e)

  fold1         :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh :. Int
                -> LLVM arch (Array sh e)

  foldSeg       :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh :. Int
                -> DIM1
                -> LLVM arch (Array (sh:.Int) e)

  fold1Seg      :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh :. Int
                -> DIM1
                -> LLVM arch (Array (sh:.Int) e)

  scanl         :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh :. Int
                -> LLVM arch (Array (sh:.Int) e)

  scanl1        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh :. Int
                -> LLVM arch (Array (sh:.Int) e)

  scanl'        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh :. Int
                -> LLVM arch (Array (sh:.Int) e, Array sh e)

  scanr         :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh :. Int
                -> LLVM arch (Array (sh:.Int) e)

  scanr1        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh :. Int
                -> LLVM arch (Array (sh:.Int) e)

  scanr'        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> sh :. Int
                -> LLVM arch (Array (sh:.Int) e, Array sh e)

  permute       :: (Shape sh, Shape sh', Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> Bool
                -> sh
                -> Array sh' e
                -> LLVM arch (Array sh' e)

  stencil1      :: (Shape sh, Elt a, Elt b)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> Array sh a
                -> LLVM arch (Array sh b)

  stencil2      :: (Shape sh, Elt a, Elt b, Elt c)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> Array sh a
                -> Array sh b
                -> LLVM arch (Array sh c)


-- Array expression evaluation
-- ---------------------------

-- Computations are evaluated by traversing the AST bottom up, and for each node
-- distinguishing between three cases:
--
--  1. If it is a Use node, we return a reference to the array data.
--
--  2. If it is a non-skeleton node, such as a let-binding or shape conversion,
--     then execute directly by updating the environment or similar.
--
--  3. If it is a skeleton node, then we need to execute the compiled kernel for
--     that node.
--
{-# INLINEABLE executeAcc #-}
executeAcc
    :: forall arch a. Execute arch
    => ExecAcc arch a
    -> LLVM arch a
executeAcc acc =
  get =<< async (executeOpenAcc acc Aempty)

{-# INLINEABLE executeAfun1 #-}
executeAfun1
    :: forall arch a b. (Execute arch, Arrays a)
    => ExecAfun arch (a -> b)
    -> a
    -> LLVM arch b
executeAfun1 afun arrs = do
  AsyncR _ a <- async (useRemoteAsync arrs)
  get =<< async (executeOpenAfun1 afun Aempty a)


-- Execute an open array function of one argument
--
{-# INLINEABLE executeOpenAfun1 #-}
executeOpenAfun1
    :: Execute arch
    => ExecOpenAfun arch aenv (a -> b)
    -> AvalR arch aenv
    -> AsyncR arch a
    -> StreamR arch
    -> LLVM arch b
executeOpenAfun1 (Alam (Abody f)) aenv a = executeOpenAcc f (aenv `Apush` a)
executeOpenAfun1 _                _    _ = error "boop!"


-- Execute an open array function of two arguments
--
{-# INLINEABLE executeOpenAfun2 #-}
executeOpenAfun2
    :: Execute arch
    => ExecOpenAfun arch aenv (a -> b -> c)
    -> AvalR arch aenv
    -> AsyncR arch a
    -> AsyncR arch b
    -> StreamR arch
    -> LLVM arch c
executeOpenAfun2 (Alam (Alam (Abody f))) aenv a b = executeOpenAcc f (aenv `Apush` a `Apush` b)
executeOpenAfun2 _                       _    _ _ = error "boop!"


-- Execute an open array computation
--
{-# INLINEABLE executeOpenAcc #-}
executeOpenAcc
    :: forall arch aenv arrs. Execute arch
    => ExecOpenAcc arch aenv arrs
    -> AvalR arch aenv
    -> StreamR arch
    -> LLVM arch arrs
executeOpenAcc EmbedAcc{} _ _ =
  $internalError "execute" "unexpected delayed array"
executeOpenAcc (ExecAcc kernel gamma pacc) aenv stream =
  case pacc of

    -- Array introduction
    Use arr                     -> return (toArr arr)
    Unit x                      -> newRemote Z . const =<< travE x
    Subarray ix sh arr          -> join $ newRemoteSubarray <$> travE ix <*> travE sh <*> pure arr

    -- Environment manipulation
    Avar ix                     -> do let AsyncR event arr = aprj ix aenv
                                      after stream event
                                      return arr
    Alet bnd body               -> do bnd'  <- async (executeOpenAcc bnd aenv)
                                      body' <- executeOpenAcc body (aenv `Apush` bnd') stream
                                      return body'
    Apply f a                   -> flip (executeOpenAfun1 f aenv) stream =<< async (executeOpenAcc a aenv)
    Atuple tup                  -> toAtuple <$> travT tup
    Aprj ix tup                 -> evalPrj ix . fromAtuple <$> travA tup
    Acond p t e                 -> acond t e =<< travE p
    Awhile p f a                -> awhile p f =<< travA a

    -- Sequence computation
    Collect l u i s             -> do l' <- travE l
                                      u' <- mapM travE u
                                      i' <- mapM travE i
                                      executeOpenSeq l' u' i' s aenv stream

    -- Foreign function
    Aforeign asm _ a            -> foreignA asm =<< travA a

    -- Producers
    Map _ a                     -> map kernel gamma aenv stream         =<< extent a
    Generate sh _               -> generate kernel gamma aenv stream    =<< travE sh
    Transform sh _ _ _          -> transform kernel gamma aenv stream   =<< travE sh
    Backpermute sh _ _          -> backpermute kernel gamma aenv stream =<< travE sh
    Reshape sh a                -> reshape <$> travE sh <*> travA a

    -- Consumers
    Fold _ _ a                  -> fold  kernel gamma aenv stream =<< extent a
    Fold1 _ a                   -> fold1 kernel gamma aenv stream =<< extent a
    FoldSeg _ _ a s             -> join $ foldSeg  kernel gamma aenv stream <$> extent a <*> extent s
    Fold1Seg _ a s              -> join $ fold1Seg kernel gamma aenv stream <$> extent a <*> extent s
    Scanl _ _ a                 -> scanl kernel gamma aenv stream =<< extent a
    Scanr _ _ a                 -> scanr kernel gamma aenv stream =<< extent a
    Scanl1 _ a                  -> scanl1 kernel gamma aenv stream =<< extent a
    Scanr1 _ a                  -> scanr1 kernel gamma aenv stream =<< extent a
    Scanl' _ _ a                -> scanl' kernel gamma aenv stream =<< extent a
    Scanr' _ _ a                -> scanr' kernel gamma aenv stream =<< extent a
    Permute _ d _ a             -> join $ permute kernel gamma aenv stream (inplace d) <$> extent a <*> travA d
    Stencil _ _ a               -> stencil1 kernel gamma aenv stream =<< travA a
    Stencil2 _ _ a _ b          -> join $ stencil2 kernel gamma aenv stream <$> travA a <*> travA b

    -- Removed by fusion
    Replicate{}                 -> fusionError
    Slice{}                     -> fusionError
    ZipWith{}                   -> fusionError

  where
    fusionError :: error
    fusionError = $internalError "execute" $ "unexpected fusible material: " ++ showPreAccOp pacc

    -- Term traversals
    -- ---------------
    travA :: ExecOpenAcc arch aenv a -> LLVM arch a
    travA acc = executeOpenAcc acc aenv stream

    travE :: ExecExp arch aenv t -> LLVM arch t
    travE exp = executeExp exp aenv stream

    travT :: Atuple (ExecOpenAcc arch aenv) t -> LLVM arch t
    travT NilAtup        = return ()
    travT (SnocAtup t a) = (,) <$> travT t <*> travA a

    -- get the extent of an embedded array
    extent :: Shape sh => ExecOpenAcc arch aenv (Array sh e) -> LLVM arch sh
    extent ExecAcc{}       = $internalError "executeOpenAcc" "expected delayed array"
    extent (EmbedAcc sh)   = travE sh
    extent (UnzipAcc _ ix) = let AsyncR _ a = aprj ix aenv
                             in  return $ shape a

    inplace :: ExecOpenAcc arch aenv a -> Bool
    inplace (ExecAcc _ _ Avar{}) = False
    inplace _                    = True

    -- Skeleton implementation
    -- -----------------------

    -- Change the shape of an array without altering its contents. This does not
    -- execute any kernel programs.
    reshape :: Shape sh => sh -> Array sh' e -> Array sh e
    reshape sh (Array sh' adata)
      = $boundsCheck "reshape" "shape mismatch" (size sh == R.size sh')
      $ Array (fromElt sh) adata

    -- Array level conditional
    acond :: ExecOpenAcc arch aenv a -> ExecOpenAcc arch aenv a -> Bool -> LLVM arch a
    acond yes _  True  = travA yes
    acond _   no False = travA no

    -- Array loops
    awhile :: ExecOpenAfun arch aenv (a -> Scalar Bool)
           -> ExecOpenAfun arch aenv (a -> a)
           -> a
           -> LLVM arch a
    awhile p f a = do
      e   <- checkpoint stream
      r   <- get =<< async (executeOpenAfun1 p aenv (AsyncR e a))
      ok  <- indexRemote r 0
      if ok then awhile p f =<< executeOpenAfun1 f aenv (AsyncR e a) stream
            else return a

    -- Foreign functions
    foreignA :: (Arrays a, Arrays b, Foreign arch, S.Foreign asm)
             => asm (a -> b)
             -> a
             -> LLVM arch b
    foreignA asm a =
      case foreignAcc (undefined :: arch) asm of
        Just f  -> f stream a
        Nothing -> $internalError "foreignA" "failed to recover foreign function the second time"

executeOpenAcc (UnzipAcc tup v) aenv stream = do
  let AsyncR event arr = aprj v aenv
  after stream event
  return $ unzip tup arr
  where
    unzip :: forall t sh e. (Elt t, Elt e) => TupleIdx (TupleRepr t) e -> Array sh t -> Array sh e
    unzip tix (Array sh adata) = Array sh $ go tix (eltType (undefined::t)) adata
      where
        go :: TupleIdx v e -> TupleType t' -> ArrayData t' -> ArrayData (EltRepr e)
        go (SuccTupIdx ix) (PairTuple t _) (AD_Pair x _)           = go ix t x
        go ZeroTupIdx      (PairTuple _ t) (AD_Pair _ x)
          | Just Refl <- matchTupleType t (eltType (undefined::e)) = x
        go _ _ _                                                   = $internalError "unzip" "inconsistent valuation"


-- Scalar expression evaluation
-- ----------------------------

{-# INLINEABLE executeExp #-}
executeExp
    :: Execute arch
    => ExecExp arch aenv t
    -> AvalR arch aenv
    -> StreamR arch
    -> LLVM arch t
executeExp exp aenv stream = executeOpenExp exp Empty aenv stream

{-# INLINEABLE executeOpenExp #-}
executeOpenExp
    :: forall arch env aenv exp. Execute arch
    => ExecOpenExp arch env aenv exp
    -> Val env
    -> AvalR arch aenv
    -> StreamR arch
    -> LLVM arch exp
executeOpenExp rootExp env aenv stream = travE rootExp
  where
    travE :: ExecOpenExp arch env aenv t -> LLVM arch t
    travE exp = case exp of
      Var ix                    -> return (prj ix env)
      Let bnd body              -> travE bnd >>= \x -> executeOpenExp body (env `Push` x) aenv stream
      Const c                   -> return (toElt c)
      PrimConst c               -> return (evalPrimConst c)
      PrimApp f x               -> evalPrim f <$> travE x
      Tuple t                   -> toTuple <$> travT t
      Prj ix e                  -> evalPrj ix . fromTuple <$> travE e
      Cond p t e                -> travE p >>= \x -> if x then travE t else travE e
      While p f x               -> while p f =<< travE x
      IndexAny                  -> return Any
      IndexNil                  -> return Z
      IndexCons sh sz           -> (:.) <$> travE sh <*> travE sz
      IndexHead sh              -> (\(_  :. ix) -> ix) <$> travE sh
      IndexTail sh              -> (\(ix :.  _) -> ix) <$> travE sh
      IndexSlice ix slix sh     -> indexSlice ix slix <$> travE sh
      IndexFull ix slix sl      -> indexFull  ix <$> travE slix <*> travE sl
      ToIndex sh ix             -> toIndex   <$> travE sh  <*> travE ix
      FromIndex sh ix           -> fromIndex <$> travE sh  <*> travE ix
      IndexTrans sh             -> transpose <$> travE sh
      ToSlice _ sh n            -> toSlice <$> travE sh <*> travE n
      Intersect sh1 sh2         -> intersect <$> travE sh1 <*> travE sh2
      Union sh1 sh2             -> union <$> travE sh1 <*> travE sh2
      ShapeSize sh              -> size  <$> travE sh
      Shape acc                 -> shape <$> travA acc
      Index acc ix              -> join $ index       <$> travA acc <*> travE ix
      LinearIndex acc ix        -> join $ indexRemote <$> travA acc <*> travE ix
      Foreign _ f x             -> foreignE f x

    -- Helpers
    -- -------

    travT :: Tuple (ExecOpenExp arch env aenv) t -> LLVM arch t
    travT tup = case tup of
      NilTup            -> return ()
      SnocTup t e       -> (,) <$> travT t <*> travE e

    travA :: ExecOpenAcc arch aenv a -> LLVM arch a
    travA acc = executeOpenAcc acc aenv stream

    foreignE :: ExecFun arch () (a -> b) -> ExecOpenExp arch env aenv a -> LLVM arch b
    foreignE (Lam (Body f)) x = travE x >>= \e -> executeOpenExp f (Empty `Push` e) Aempty stream
    foreignE _              _ = error "I bless the rains down in Africa"

    travF1 :: ExecOpenFun arch env aenv (a -> b) -> a -> LLVM arch b
    travF1 (Lam (Body f)) x = executeOpenExp f (env `Push` x) aenv stream
    travF1 _              _ = error "LANAAAAAAAA!"

    while :: ExecOpenFun arch env aenv (a -> Bool) -> ExecOpenFun arch env aenv (a -> a) -> a -> LLVM arch a
    while p f x = do
      ok <- travF1 p x
      if ok then while p f =<< travF1 f x
            else return x

    indexSlice :: (Elt slix, Elt sh, Elt sl)
               => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
               -> proxy slix
               -> sh
               -> sl
    indexSlice ix _ sh = toElt $ restrict ix (fromElt sh)
      where
        restrict :: SliceIndex slix sl co sh -> sh -> sl
        restrict SliceNil              ()       = ()
        restrict (SliceAll   sliceIdx) (sl, sz) = (restrict sliceIdx sl, sz)
        restrict (SliceFixed sliceIdx) (sl,  _) = restrict sliceIdx sl

    indexFull :: (Elt slix, Elt sh, Elt sl)
              => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
              -> slix
              -> sl
              -> sh
    indexFull ix slix sl = toElt $ extend ix (fromElt slix) (fromElt sl)
      where
        extend :: SliceIndex slix sl co sh -> slix -> sl -> sh
        extend SliceNil              ()        ()       = ()
        extend (SliceAll sliceIdx)   (slx, ()) (sh, sz) = (extend sliceIdx slx sh, sz)
        extend (SliceFixed sliceIdx) (slx, sz) sh       = (extend sliceIdx slx sh, sz)

    index :: Shape sh => Array sh e -> sh -> LLVM arch e
    index arr ix = linearIndex arr (toIndex (shape arr) ix)

    linearIndex :: Array sh e -> Int -> LLVM arch e
    linearIndex arr ix = do
      block =<< checkpoint stream
      indexRemote arr ix

executeSeq
  :: Execute arch
  => StreamSeq (Int,Int) (ExecOpenAcc arch) arrs
  -> LLVM arch arrs
executeSeq (StreamSeq binds s) = do
  aenv <- executeExtend binds Aempty
  get =<< async (executeOpenSeq 1 Nothing Nothing s aenv)

executeOpenSeq
  :: forall arch index aenv arrs. (Execute arch, SeqIndex index)
  => Int
  -> Maybe Int
  -> Maybe Int
  -> PreOpenSeq index (ExecOpenAcc arch) aenv arrs
  -> AvalR arch aenv
  -> StreamR arch
  -> LLVM arch arrs
executeOpenSeq mi _ma i s aenv stream =
    case s of
      Producer (ProduceAccum l f a) (Consumer (Last a' d)) -> (last <$>) $
        join $ go i
          <$> fmap schedule (executeExp (initialIndex (Const mi)) aenv stream)
          <*> sequenceA (executeExp <$> l <*> pure aenv <*> pure stream)
          <*> pure (executeOpenAfun2 f aenv)
          <*> async (executeOpenAcc a aenv)
          <*> (sequence [executeOpenAcc d (aenv `Apush` undefined) stream])
          <*> pure (\aenv' -> executeOpenAcc a' aenv' stream)
      Producer (ProduceAccum l f a) (Reify ty a') -> (concatMap (divide ty) <$>) $
        join $ go i
          <$> fmap schedule (executeExp (initialIndex (Const mi)) aenv stream)
          <*> sequenceA (executeExp <$> l <*> pure aenv <*> pure stream)
          <*> pure (executeOpenAfun2 f aenv)
          <*> async (executeOpenAcc a aenv)
          <*> pure []
          <*> pure (\aenv' -> executeOpenAcc a' aenv' stream)
      _ -> $internalError "evalSeq" "Sequence computation does not appear to be delayed"
  where
    go :: forall arrs a b. Maybe Int
       -> Schedule index
       -> Maybe Int
       -> (AsyncR arch (Scalar index) -> AsyncR arch b -> StreamR arch -> LLVM arch (a, b))
       -> AsyncR arch b
       -> [arrs]
       -> (AvalR arch (aenv, a) -> LLVM arch arrs)
       -> LLVM arch [arrs]
    go (Just 0) _ _ _ _ a _ = return a
    go i Schedule{..} l f s a unwrap = do
      index'   <- async (\_ -> newRemote Z (const index))

      if maybe True (contains' index) l
           then do
             liftIO $ Debug.traceIO Debug.dump_exec ("Computing sequence chunk " ++ show index)
             (time, (a', s')) <- timed (f index' s)
             event <- checkpoint stream
             a'' <- unwrap (Apush aenv (AsyncR event a'))
             rest <- unsafeInterleaveLLVM (go (subtract 1 <$> i) (next time) l f (AsyncR event s') [] unwrap)
             return (a'' : rest)
           else return a

    schedule :: SeqIndex index => index -> Schedule index
    schedule | Just Refl <- eqT :: Maybe (index :~: Int)
             = sequential
             | Just Refl <- eqT :: Maybe (index :~: (Int,Int))
             = doubleSizeChunked
             | otherwise
             = $internalError "executeOpenSeq" "Unknown sequence index type"

    divide :: LiftedType a a' -> a' -> [a]
    divide UnitT       _ = [()]
    divide LiftedUnitT a = replicate (a ! Z) ()
    divide AvoidedT    a = [a]
    divide RegularT    a = regular a
    divide IrregularT  a = irregular a
    divide (TupleT t)  a = fmap toAtuple (divideT t (fromAtuple a))
      where
        divideT :: LiftedTupleType t t' -> t' -> [t]
        divideT NilLtup          ()    = [()]
        divideT (SnocLtup lt ty) (t,a) = zip (divideT lt t) (divide ty a)

    regular :: forall sh e. Shape sh => Array (sh:.Int) e -> [Array sh e]
    regular arr@(Array _ adata) = [Array (fromElt sh') (copy (i * size sh') (size sh')) | i <- [0..n-1]]
      where
        sh  = shapeToList (shape arr)
        n   = last sh
        --
        sh' :: sh
        sh' = listToShape (init sh)
        --
        copy start n = unsafePerformIO (unsafeCopyArrayData adata start n)

    irregular :: forall sh e. Shape sh => (Segments sh, Vector e) -> [Array sh e]
    irregular (segs, (Array _ adata))
      = [Array (fromElt (shs ! (Z:.i))) (copy (offs ! (Z:.i)) (size (shs ! (Z:.i)))) | i <- [0..n-1]]
      where
        (_, offs, shs) = segs
        n              = size (shape shs)
        --
        copy start n = unsafePerformIO (unsafeCopyArrayData adata start n)


executeExtend :: Execute arch
              => Extend (ExecOpenAcc arch) aenv aenv'
              -> AvalR arch aenv
              -> LLVM arch (AvalR arch aenv')
executeExtend (PushEnv env a) aenv = do
  aenv' <- executeExtend env aenv
  a'    <- async (executeOpenAcc a aenv')
  return (Apush aenv' a')
executeExtend BaseEnv aenv = return aenv