{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute (

  Execute(..), Gamma,
  executeAcc, executeAfun,
  executeOpenAcc,

) where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Representation               ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                        hiding ( Foreign )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Interpreter                        ( evalPrim, evalPrimConst, evalPrj )
import qualified Data.Array.Accelerate.Array.Sugar              as S

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.Foreign
import Data.Array.Accelerate.LLVM.Link
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )

import Data.Array.Accelerate.LLVM.Execute.Async                 hiding ( join )
import Data.Array.Accelerate.LLVM.Execute.Environment

-- library
import Control.Monad
import Control.Applicative                                      hiding ( Const )
import Prelude                                                  hiding ( exp, map, unzip, scanl, scanr, scanl1, scanr1 )


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
--  1. If it is a Use node, asynchronously transfer the data to the remote
--     device (if necessary).
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
executeAcc !acc =
  get =<< async (executeOpenAcc acc Aempty)


-- Execute a variadic array function
--
{-# INLINEABLE executeAfun #-}
executeAfun
    :: ExecuteAfun arch f
    => ExecAfun arch (ExecAfunR arch f)
    -> f
executeAfun f = executeOpenAfun f (return Aempty)

class ExecuteAfun arch f where
  type ExecAfunR arch f
  executeOpenAfun :: ExecOpenAfun arch aenv (ExecAfunR arch f) -> LLVM arch (AvalR arch aenv) -> f

instance (Remote arch, ExecuteAfun arch b) => ExecuteAfun arch (a -> b) where
  type ExecAfunR arch (a -> b) = a -> ExecAfunR arch b
  {-# INLINEABLE executeOpenAfun #-}
  executeOpenAfun Abody{}  _ _    = $internalError "executeOpenAfun" "malformed array function"
  executeOpenAfun (Alam f) k arrs =
    let k' = do aenv       <- k
                AsyncR _ a <- async (useRemoteAsync arrs)
                return (aenv `Apush` a)
    in
    executeOpenAfun f k'

instance Execute arch => ExecuteAfun arch (LLVM arch b) where
  type ExecAfunR arch (LLVM arch b) = b
  {-# INLINEABLE executeOpenAfun #-}
  executeOpenAfun Alam{}    _ = $internalError "executeOpenAfun" "function not fully applied"
  executeOpenAfun (Abody b) k = do
    aenv <- k
    get =<< async (executeOpenAcc b aenv)


-- NOTE: [ExecuteAfun and closed type families]
--
-- It would be nice to use something like the following closed type family
-- instance, and implement 'executeOpenAfun' as a regular recursive function,
-- rather than as a class function.
--
-- > type family ExecAfunR arch r :: * where
-- >   ExecAfunR arch (a -> b) = a -> ExecAfunR arch b
-- >   ExecAfunR arch r        = LLVM arch r
-- >
-- > executeOpenAfun
-- >     :: Execute arch
-- >     => ExecOpenAfun arch aenv f
-- >     -> LLVM arch (AvalR arch aenv)
-- >     -> ExecAfunR arch f
-- > executeOpenAfun (Alam f)  k = \arrs -> ...
-- > executeOpenAfun (Abody b) k = do ...
--
-- However, closed type families don't quite work the way that we might think.
-- It seems that they rely on some notion of type inequality, or at least types
-- which don't have a unifier.
--
-- When we match of the `Abody` constructor, we expose a constraint of the form
-- `Arrays a, T a ~ a0`. For the type checker to figure out that
-- `a0 ~ LLVM arch a`, it needs to know that it _can not_ match on the first
-- case of the type family; i.e., that `a` can't unify with `b -> c`. Since it
-- doesn't have constraints to figure that out, it doesn't proceed and fall
-- through to the case that we want. If we had something like `a ~ Array sh e`,
-- then it could.
--


-- Execute an open array computation
--
{-# INLINEABLE executeOpenAcc #-}
executeOpenAcc
    :: forall arch aenv arrs. Execute arch
    => ExecOpenAcc arch aenv arrs
    -> AvalR arch aenv
    -> StreamR arch
    -> LLVM arch arrs
executeOpenAcc !topAcc !aenv !stream = travA topAcc
  where
    travA :: ExecOpenAcc arch aenv a -> LLVM arch a
    travA (EvalAcc pacc) =
      case pacc of
        Use arrs        -> get =<< useRemoteAsync (toArr arrs) stream
        Unit x          -> newRemote Z . const =<< travE x
        Avar ix         -> avar ix
        Alet bnd body   -> alet bnd body
        Apply f a       -> travAF f =<< async (executeOpenAcc a aenv)
        Atuple tup      -> toAtuple <$> travT tup
        Aprj ix tup     -> evalPrj ix . fromAtuple <$> travA tup
        Acond p t e     -> acond t e =<< travE p
        Awhile p f a    -> awhile p f =<< travA a
        Reshape sh ix   -> reshape <$> travE sh <*> avar ix
        Unzip tix ix    -> unzip tix <$> avar ix
        Aforeign asm a  -> aforeign asm =<< travA a

    travA (ExecAcc !gamma !kernel pacc) =
      case pacc of
        -- Producers
        Map sh          -> map kernel gamma aenv stream =<< travE sh
        Generate sh     -> generate kernel gamma aenv stream =<< travE sh
        Transform sh    -> transform kernel gamma aenv stream =<< travE sh
        Backpermute sh  -> backpermute kernel gamma aenv stream =<< travE sh

        -- Consumers
        Fold sh         -> fold  kernel gamma aenv stream =<< travE sh
        Fold1 sh        -> fold1 kernel gamma aenv stream =<< travE sh
        FoldSeg sa ss   -> id =<< foldSeg  kernel gamma aenv stream <$> travE sa <*> travE ss
        Fold1Seg sa ss  -> id =<< fold1Seg kernel gamma aenv stream <$> travE sa <*> travE ss
        Scanl sh        -> scanl  kernel gamma aenv stream =<< travE sh
        Scanr sh        -> scanr  kernel gamma aenv stream =<< travE sh
        Scanl1 sh       -> scanl1 kernel gamma aenv stream =<< travE sh
        Scanr1 sh       -> scanr1 kernel gamma aenv stream =<< travE sh
        Scanl' sh       -> scanl' kernel gamma aenv stream =<< travE sh
        Scanr' sh       -> scanr' kernel gamma aenv stream =<< travE sh
        Permute sh d    -> id =<< permute kernel gamma aenv stream (inplace d) <$> travE sh <*> travA d
        Stencil2 a b    -> id =<< stencil2 kernel gamma aenv stream <$> avar a <*> avar b
        Stencil a       -> stencil1 kernel gamma aenv stream =<< avar a

    travAF :: ExecOpenAfun arch aenv (a -> b) -> AsyncR arch a -> LLVM arch b
    travAF (Alam (Abody f)) a = get =<< async (executeOpenAcc f (aenv `Apush` a))
    travAF _                _ = error "boop!"

    travE :: ExecExp arch aenv t -> LLVM arch t
    travE exp = executeExp exp aenv stream

    travT :: Atuple (ExecOpenAcc arch aenv) t -> LLVM arch t
    travT NilAtup        = return ()
    travT (SnocAtup t a) = (,) <$> travT t <*> travA a

    -- Bound terms. Let-bound input arrays (Use nodes) are copied to the device
    -- asynchronously, so that they may overlap other computations if possible.
    alet :: ExecOpenAcc arch aenv bnd -> ExecOpenAcc arch (aenv, bnd) body -> LLVM arch body
    alet bnd body = do
      bnd'  <- case bnd of
                 EvalAcc (Use arrs) -> do AsyncR _ bnd' <- async (useRemoteAsync (toArr arrs))
                                          return bnd'
                 _                  -> async (executeOpenAcc bnd aenv)
      body' <- executeOpenAcc body (aenv `Apush` bnd') stream
      return body'

    -- Access bound variables
    avar :: Idx aenv a -> LLVM arch a
    avar ix = do
      let AsyncR event arr = aprj ix aenv
      after stream event
      return arr

    -- Array level conditionals
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
      r   <- travAF p (AsyncR e a)
      ok  <- indexRemote r 0
      if ok then awhile p f =<< travAF f (AsyncR e a)
            else return a

    -- Pull apart the unzipped struct-of-array representation
    unzip :: forall t sh e. (Elt t, Elt e) => TupleIdx (TupleRepr t) e -> Array sh t -> Array sh e
    unzip tix (Array sh adata) = Array sh $ go tix (eltType (undefined::t)) adata
      where
        go :: TupleIdx v e -> TupleType t' -> ArrayData t' -> ArrayData (EltRepr e)
        go (SuccTupIdx ix) (PairTuple t _) (AD_Pair x _)           = go ix t x
        go ZeroTupIdx      (PairTuple _ t) (AD_Pair _ x)
          | Just Refl <- matchTupleType t (eltType (undefined::e)) = x
        go _ _ _                                                   = $internalError "unzip" "inconsistent valuation"

    -- Foreign functions
    aforeign :: (Arrays a, Arrays b, Foreign arch, S.Foreign asm)
             => asm (a -> b)
             -> a
             -> LLVM arch b
    aforeign asm a =
      case foreignAcc (undefined :: arch) asm of
        Just f  -> f stream a
        Nothing -> $internalError "foreignA" "failed to recover foreign function the second time"

    -- Can the permutation function write directly into the results array?
    inplace :: ExecOpenAcc arch aenv a -> Bool
    inplace (EvalAcc Avar{}) = False
    inplace _                = True


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
      IndexSlice ix slix sh     -> indexSlice ix <$> travE slix <*> travE sh
      IndexFull ix slix sl      -> indexFull  ix <$> travE slix <*> travE sl
      ToIndex sh ix             -> toIndex   <$> travE sh  <*> travE ix
      FromIndex sh ix           -> fromIndex <$> travE sh  <*> travE ix
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

    index :: Shape sh => Array sh e -> sh -> LLVM arch e
    index arr ix = linearIndex arr (toIndex (shape arr) ix)

    linearIndex :: Array sh e -> Int -> LLVM arch e
    linearIndex arr ix = do
      block =<< checkpoint stream
      indexRemote arr ix

