{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute (

  Execute(..),Gamma,
  executeAcc, executeAfun1,

) where

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Representation               ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Interpreter                        ( evalPrim, evalPrimConst, evalPrj )
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )

import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.Execute.Environment

-- library
import Prelude                                                  hiding ( exp, map, scanl, scanr, scanl1, scanr1 )
import Control.Applicative                                      hiding ( Const )
import Control.Monad


class Remote arch => Execute arch where
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

  scanl         :: Elt e
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> DIM1
                -> LLVM arch (Vector e)

  scanl1        :: Elt e
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> DIM1
                -> LLVM arch (Vector e)

  scanl'        :: Elt e
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> DIM1
                -> LLVM arch (Vector e, Scalar e)

  scanr         :: Elt e
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> DIM1
                -> LLVM arch (Vector e)

  scanr1        :: Elt e
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> DIM1
                -> LLVM arch (Vector e)

  scanr'        :: Elt e
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
                -> DIM1
                -> LLVM arch (Vector e, Scalar e)

  permute       :: (Shape sh, Shape sh', Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> AvalR arch aenv
                -> StreamR arch
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

  aforeign      :: (Arrays as, Arrays bs, Foreign f)
                => f as bs
                -> ExecAfun arch (as -> bs)
                -> as
                -> LLVM arch bs


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
executeAcc
    :: forall arch a. (Execute arch, Arrays a)
    => ExecAcc arch a
    -> LLVM arch a
executeAcc acc =
  streaming (executeOpenAcc acc Aempty) wait

executeAfun1
    :: forall arch a b. (Execute arch, Arrays a, Arrays b)
    => ExecAfun arch (a -> b)
    -> a
    -> LLVM arch b
executeAfun1 afun arrs =
  streaming (\s -> useArrays (arrays arrs) (fromArr arrs) s >> return arrs)
            (executeOpenAfun1 afun Aempty)
  where
    useArrays :: ArraysR arrs -> arrs -> StreamR arch -> LLVM arch ()
    useArrays ArraysRunit         ()       _  = return ()
    useArrays (ArraysRpair r1 r0) (a1, a0) st = useArrays r1 a1 st >> useArrays r0 a0 st
    useArrays ArraysRarray        arr      st = void $ copyToRemoteAsync st arr


-- Execute an open array function of one argument
--
executeOpenAfun1
    :: Execute arch
    => PreOpenAfun (ExecOpenAcc arch) aenv (a -> b)
    -> AvalR arch aenv
    -> AsyncR arch a
    -> LLVM arch b
executeOpenAfun1 (Alam (Abody f)) aenv a = streaming (executeOpenAcc f (aenv `Apush` a)) wait
executeOpenAfun1 _                _    _ = error "boop!"


-- Execute an open array computation
--
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

    -- Environment manipulation
    Avar ix                     -> after stream (aprj ix aenv)
    Alet bnd body               -> streaming (executeOpenAcc bnd aenv) (\x -> executeOpenAcc body (aenv `Apush` x) stream)
    Apply f a                   -> streaming (executeOpenAcc a aenv)   (executeOpenAfun1 f aenv)
    Atuple tup                  -> toTuple <$> travT tup
    Aprj ix tup                 -> evalPrj ix . fromTuple <$> travA tup
    Acond p t e                 -> acond t e =<< travE p
    Awhile p f a                -> awhile p f =<< travA a

    -- Foreign function
    Aforeign ff afun a          -> aforeign ff afun =<< travA a

    -- Producers
    Map _ a                     -> map kernel gamma aenv stream         =<< extent a
    Generate sh _               -> generate kernel gamma aenv stream    =<< travE sh
    Transform sh _ _ _          -> transform kernel gamma aenv stream   =<< travE sh
    Backpermute sh _ _          -> backpermute kernel gamma aenv stream =<< travE sh
    Reshape sh a                -> reshapeOp <$> travE sh <*> travA a

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
    Permute _ d _ a             -> join $ permute kernel gamma aenv stream <$> extent a <*> travA d
    Stencil _ _ a               -> stencil1 kernel gamma aenv stream =<< travA a
    Stencil2 _ _ a _ b          -> join $ stencil2 kernel gamma aenv stream <$> travA a <*> travA b

    -- Removed by fusion
    Replicate{}                 -> fusionError
    Slice{}                     -> fusionError
    ZipWith{}                   -> fusionError

  where
    fusionError = $internalError "execute" "unexpected fusible matter"

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
    extent ExecAcc{}     = $internalError "executeOpenAcc" "expected delayed array"
    extent (EmbedAcc sh) = travE sh

    -- Skeleton implementation
    -- -----------------------

    -- Change the shape of an array without altering its contents. This does not
    -- execute any kernel programs.
    reshapeOp :: Shape sh => sh -> Array sh' e -> Array sh e
    reshapeOp sh (Array sh' adata)
      = $boundsCheck "reshape" "shape mismatch" (size sh == R.size sh')
      $ Array (fromElt sh) adata

    -- Array level conditional
    acond :: ExecOpenAcc arch aenv a -> ExecOpenAcc arch aenv a -> Bool -> LLVM arch a
    acond yes _  True  = travA yes
    acond _   no False = travA no

    -- Array loops
    awhile :: PreOpenAfun (ExecOpenAcc arch) aenv (a -> Scalar Bool)
           -> PreOpenAfun (ExecOpenAcc arch) aenv (a -> a)
           -> a
           -> LLVM arch a
    awhile p f a = do
      a'  <- async stream (return a)
      r   <- executeOpenAfun1 p aenv a'
      ok  <- indexRemote r 0
      if ok then awhile p f =<< executeOpenAfun1 f aenv a'
            else return a


-- Scalar expression evaluation
-- ----------------------------

executeExp
    :: Execute arch
    => ExecExp arch aenv t
    -> AvalR arch aenv
    -> StreamR arch
    -> LLVM arch t
executeExp exp aenv stream = executeOpenExp exp Empty aenv stream

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
      IndexSlice ix slix sh     -> indexSlice ix <$> travE slix <*> travE sh
      IndexFull ix slix sl      -> indexFull  ix <$> travE slix <*> travE sl
      ToIndex sh ix             -> toIndex   <$> travE sh  <*> travE ix
      FromIndex sh ix           -> fromIndex <$> travE sh  <*> travE ix
      Intersect sh1 sh2         -> intersect <$> travE sh1 <*> travE sh2
      ShapeSize sh              -> size  <$> travE sh
      Shape acc                 -> shape <$> travA acc
      Index acc ix              -> join $ index       <$> travA acc <*> travE ix
      LinearIndex acc ix        -> join $ indexRemote <$> travA acc <*> travE ix
      Foreign _ f x             -> eforeign f x

    -- Helpers
    -- -------

    travT :: Tuple (ExecOpenExp arch env aenv) t -> LLVM arch t
    travT tup = case tup of
      NilTup            -> return ()
      SnocTup t e       -> (,) <$> travT t <*> travE e

    travA :: ExecOpenAcc arch aenv a -> LLVM arch a
    travA acc = executeOpenAcc acc aenv stream

    eforeign :: ExecFun arch () (a -> b) -> ExecOpenExp arch env aenv a -> LLVM arch b
    eforeign _ _ = error "TODO: execute Foreign"
--    eforeign (Lam (Body f)) x = travE x >>= \e -> executeOpenExp f (Empty `Push` e) Aempty
--    eforeign _              _ = error "I bless the rains down in Africa"

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
               -> slix
               -> sh
               -> sl
    indexSlice ix slix sh = toElt $ restrict ix (fromElt slix) (fromElt sh)
      where
        restrict :: SliceIndex slix sl co sh -> slix -> sh -> sl
        restrict SliceNil              ()        ()       = ()
        restrict (SliceAll   sliceIdx) (slx, ()) (sl, sz) = (restrict sliceIdx slx sl, sz)
        restrict (SliceFixed sliceIdx) (slx,  _) (sl,  _) = restrict sliceIdx slx sl

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

    index :: (Shape sh, Elt e) => Array sh e -> sh -> LLVM arch e
    index arr ix = indexRemote arr (toIndex (shape arr) ix)

