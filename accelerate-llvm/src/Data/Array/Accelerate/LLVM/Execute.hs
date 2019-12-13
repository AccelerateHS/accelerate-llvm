{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute (

  Execute(..), Delayed(..), Gamma,
  executeAcc,
  executeOpenAcc,

) where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Representation               ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                        hiding ( Foreign )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Interpreter                        ( evalPrim, evalPrimConst, evalPrj, evalUndef, evalCoerce )
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Debug                    as Debug

import Data.Array.Accelerate.LLVM.AST                           hiding ( Delayed, Manifest )
import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.Execute.Environment
import Data.Array.Accelerate.LLVM.Link
import qualified Data.Array.Accelerate.LLVM.AST                 as AST

-- library
import Control.Monad
import System.IO.Unsafe
import Unsafe.Coerce
import Prelude                                                  hiding ( exp, map, unzip, scanl, scanr, scanl1, scanr1 )


class Remote arch => Execute arch where
  map           :: (Shape sh, Elt a, Elt b)
                => Maybe (a :~: b)              -- update values in-place?
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Array sh a
                -> Par arch (FutureR arch (Array sh b))

  generate      :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh
                -> Par arch (FutureR arch (Array sh e))

  transform     :: (Shape sh, Shape sh', Elt a, Elt b)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh'
                -> Array sh a
                -> Par arch (FutureR arch (Array sh' b))

  backpermute   :: (Shape sh, Shape sh', Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh'
                -> Array sh e
                -> Par arch (FutureR arch (Array sh' e))

  fold          :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh :. Int) e)
                -> Par arch (FutureR arch (Array sh e))

  fold1         :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh :. Int) e)
                -> Par arch (FutureR arch (Array sh e))

  foldSeg       :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh :. Int) e)
                -> Delayed (Segments i)
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  fold1Seg      :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh :. Int) e)
                -> Delayed (Segments i)
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  scanl         :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh :. Int) e)
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  scanl1        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh :. Int) e)
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  scanl'        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh :. Int) e)
                -> Par arch (FutureR arch (Array (sh:.Int) e, Array sh e))

  scanr         :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh :. Int) e)
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  scanr1        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh :. Int) e)
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  scanr'        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh :. Int) e)
                -> Par arch (FutureR arch (Array (sh:.Int) e, Array sh e))

  permute       :: (Shape sh, Shape sh', Elt e)
                => Bool                         -- ^ update defaults array in-place?
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Array sh' e
                -> Delayed (Array sh e)
                -> Par arch (FutureR arch (Array sh' e))

  stencil1      :: (Shape sh, Elt a, Elt b)
                => sh
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array sh a)
                -> Par arch (FutureR arch (Array sh b))

  stencil2      :: (Shape sh, Elt a, Elt b, Elt c)
                => sh
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array sh a)
                -> Delayed (Array sh b)
                -> Par arch (FutureR arch (Array sh c))

  aforeign      :: String
                -> ArraysR bs
                -> (as -> Par arch (FutureR arch bs))
                -> as
                -> Par arch (FutureR arch bs)

data Delayed a where
  Delayed       :: sh -> Delayed (Array sh e)
  Manifest      :: a  -> Delayed a


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
    :: Execute arch
    => ExecAcc arch a
    -> Par arch (FutureArraysR arch a)
executeAcc !acc =
  executeOpenAcc acc Empty


{--
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
  executeOpenAfun :: ExecOpenAfun arch aenv (ExecAfunR arch f) -> Par arch (AvalR arch aenv) -> f

instance (Remote arch, ExecuteAfun arch b) => ExecuteAfun arch (a -> b) where
  type ExecAfunR arch (a -> b) = a -> ExecAfunR arch b
  {-# INLINEABLE executeOpenAfun #-}
  executeOpenAfun Abody{}      _ _    = $internalError "executeOpenAfun" "malformed array function"
  executeOpenAfun (Alam lhs f) k arrs =
    let k' = do aenv <- k
                a    <- useRemoteAsync arrs
                return (aenv `Apush` a)
    in
    executeOpenAfun f k'

instance Execute arch => ExecuteAfun arch (Par arch b) where
  type ExecAfunR arch (Par arch b) = b
  {-# INLINEABLE executeOpenAfun #-}
  executeOpenAfun Alam{}    _ = $internalError "executeOpenAfun" "function not fully applied"
  executeOpenAfun (Abody b) k = do
    aenv <- k
    executeOpenAcc b aenv
--}


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
    -> ValR arch aenv
    -> Par arch (FutureArraysR arch arrs)
executeOpenAcc !topAcc !aenv = travA topAcc
  where
    travA :: ExecOpenAcc arch aenv a -> Par arch (FutureArraysR arch a)
    travA (EvalAcc pacc) =
      case pacc of
        Use arrs            -> spawn $ useRemoteAsync ArraysRarray arrs
        Unit x              -> unit x
        Avar (ArrayVar ix)  -> return $ prj ix aenv
        Alet lhs bnd body   -> alet lhs bnd body
        Apair a1 a2         -> liftM2 (,) (travA a1) (travA a2)
        Anil                -> return ()
        Alloc sh            -> allocate sh
        Apply f a           -> travAF f =<< spawn (travA a)
        Acond p t e         -> acond t e =<< travE p
        Awhile p f a        -> awhile p f =<< spawn (travA a)
        Reshape sh (ArrayVar ix)
                            -> liftF2 reshape (travE sh) (return $ prj ix aenv)
        Unzip tix (ArrayVar ix)
                            -> liftF1 (unzip tix) (return $ prj ix aenv)
        Aforeign str r asm a -> do
          x <- travA a
          y <- spawn $ aforeign str r asm =<< getArrays (arraysRepr a) x
          split r y

    travA (ExecAcc !gamma !kernel pacc) =
      case pacc of
        -- Producers
        Map a               -> exec1 (map_ a)    (travA a)
        Generate sh         -> exec1 generate    (travE sh)
        Transform sh a      -> exec2 transform   (travE sh) (travA a)
        Backpermute sh a    -> exec2 backpermute (travE sh) (travA a)

        -- Consumers
        Fold a              -> exec1 fold     (travD a)
        Fold1 a             -> exec1 fold1    (travD a)
        FoldSeg a s         -> exec2 foldSeg  (travD a) (travD s)
        Fold1Seg a s        -> exec2 fold1Seg (travD a) (travD s)
        Scanl a             -> exec1 scanl    (travD a)
        Scanr a             -> exec1 scanr    (travD a)
        Scanl1 a            -> exec1 scanl1   (travD a)
        Scanr1 a            -> exec1 scanr1   (travD a)
        Scanl' a            -> splitPair
                             $ exec1 scanl'   (travD a)
        Scanr' a            -> splitPair
                             $ exec1 scanr'   (travD a)
        Permute d a         -> exec2 (permute_ d) (travA d) (travD a)
        Stencil1 h a        -> exec1 (stencil1 h) (travD a)
        Stencil2 h a b      -> exec2 (stencil2 h) (travD a) (travD b)

      where
        exec1 :: (ExecutableR arch -> Gamma aenv -> ValR arch aenv -> a -> Par arch (FutureR arch b))
              -> Par arch (FutureR arch a)
              -> Par arch (FutureR arch b)
        exec1 f x = do
          x' <- x
          spawn $ f kernel gamma aenv =<< get x'

        exec2 :: (ExecutableR arch -> Gamma aenv -> ValR arch aenv -> a -> b -> Par arch (FutureR arch c))
              -> Par arch (FutureR arch a)
              -> Par arch (FutureR arch b)
              -> Par arch (FutureR arch c)
        exec2 f x y = do
          x' <- x
          y' <- y
          spawn $ id =<< liftM2 (f kernel gamma aenv) (get x') (get y')

        splitPair :: forall a b. Par arch (FutureR arch (a, b))
              -> Par arch (((), FutureR arch a), FutureR arch b)
        splitPair x = do
          r1 <- new
          r2 <- new
          fork $ do
            x' <- x
            (a, b) <- get x'
            put r1 a
            put r2 b
          return (((), r1), r2)

    travAF :: ExecOpenAfun arch aenv (a -> b) -> FutureArraysR arch a -> Par arch (FutureArraysR arch b)
    travAF (Alam lhs (Abody f)) a = executeOpenAcc f $ aenv `push` (lhs, a)
    travAF _                    _ = error "boop!"

    travE :: ExecExp arch aenv t -> Par arch (FutureR arch t)
    travE exp = executeExp exp aenv

    travD :: DelayedOpenAcc ExecOpenAcc arch aenv a -> Par arch (FutureR arch (Delayed a))
    travD (AST.Delayed sh) = liftF1 Delayed  (travE sh)
    travD (AST.Manifest a) = liftF1 Manifest (travA a)

    unit :: Elt t => ExecExp arch aenv t -> Par arch (FutureR arch (Scalar t))
    unit x = do
      x'   <- travE x
      spawn $ newRemoteAsync Z . const =<< get x'

    -- Let bindings
    alet :: LeftHandSide a aenv aenv' -> ExecOpenAcc arch aenv a -> ExecOpenAcc arch aenv' b -> Par arch (FutureArraysR arch b)
    alet lhs bnd body = do
      bnd'  <- spawn $ executeOpenAcc bnd aenv
      body' <- spawn $ executeOpenAcc body $ aenv `push` (lhs, bnd')
      return body'

    -- Allocate an array on the remote device
    allocate :: (Shape sh, Elt e) => ExecExp arch aenv sh -> Par arch (FutureR arch (Array sh e))
    allocate sh = do
      r    <- new
      sh'  <- travE sh
      fork $ do
        arr <- allocateRemote =<< get sh'
        put r arr
      return r

    -- Array level conditionals
    acond :: ExecOpenAcc arch aenv a -> ExecOpenAcc arch aenv a -> FutureR arch Bool -> Par arch (FutureArraysR arch a)
    acond yes no p =
      spawn $ do
        c <- block p
        if c then travA yes
             else travA no

    -- Array loops
    awhile :: ExecOpenAfun arch aenv (a -> Scalar Bool)
           -> ExecOpenAfun arch aenv (a -> a)
           -> FutureArraysR arch a
           -> Par arch (FutureArraysR arch a)
    awhile p f a = do
      r  <- get =<< travAF p a
      ok <- indexRemote r 0
      if ok then awhile p f =<< travAF f a
            else return a

    -- Pull apart the unzipped struct-of-array representation
    unzip :: forall t sh e. (Elt t, Elt e) => TupleIdx (TupleRepr t) e -> Array sh t -> Array sh e
    unzip tix (Array sh adata) = Array sh $ go tix (eltType @t) adata
      where
        go :: TupleIdx v e -> TupleType t' -> ArrayData t' -> ArrayData (EltRepr e)
        go (SuccTupIdx ix) (TypeRpair t _) (AD_Pair x _)           = go ix t x
        go ZeroTupIdx      (TypeRpair _ t) (AD_Pair _ x)
          | Just Refl <- matchTupleType t (eltType @e) = x
        go _ _ _                                                   = $internalError "unzip" "inconsistent valuation"

    map_ :: forall sh a b. (Shape sh, Elt a, Elt b)
         => ExecOpenAcc arch aenv (Array sh a)
         -> ExecutableR arch
         -> Gamma aenv
         -> ValR arch aenv
         -> Array sh a
         -> Par arch (FutureR arch (Array sh b))
    map_ a
      | inplace a
      , Just Refl <- matchTupleType (eltType @a) (eltType @b)
      = map (Just (unsafeCoerce Refl))
        -- XXX: We only require that the representation types are identical
        -- so that we can reuse the underlying storage; we don't care if
        -- the surface types differ. -- TLM 2019-06-24
      | otherwise
      = map Nothing

    permute_ :: (Shape sh, Shape sh', Elt e)
             => ExecOpenAcc arch aenv (Array sh' e)
             -> ExecutableR arch
             -> Gamma aenv
             -> ValR arch aenv
             -> Array sh' e
             -> Delayed (Array sh e)
             -> Par arch (FutureR arch (Array sh' e))
    permute_ d = permute (inplace d)

    -- Can the function store its results in-place to the input array?
    inplace :: ExecOpenAcc arch aenv a -> Bool
    inplace a
      | unsafePerformIO (Debug.getFlag Debug.inplace) -- liftPar :: IO a -> Par arch a
      = case a of
          ExecAcc{}    -> True
          EvalAcc pacc ->
            case pacc of
              Avar{} -> False
              Use{}  -> False
              Unit{} -> False
              _      -> True
      --
      | otherwise
      = False


-- Scalar expression evaluation
-- ----------------------------

-- TLM: Returning a future seems the correct thing to do here, but feels pretty
--      heavy-weight. In particular, perhaps we only need to know the shape of
--      an array before proceeding (i.e. scheduling execution of the next array)
--      without having to wait for the array elements to be evaluated.
--
--      Additionally, most operations do not interact with arrays and could be
--      evaluated directly (e.g. shape/index manipulations) (currently futures
--      are implemented in both backends as a data structure in an IORef, so we
--      could avoid some indirections).
--

{-# INLINEABLE executeExp #-}
executeExp
    :: Execute arch
    => ExecExp arch aenv t
    -> ValR arch aenv
    -> Par arch (FutureR arch t)
executeExp exp aenv = executeOpenExp exp Empty aenv

{-# INLINEABLE executeOpenExp #-}
executeOpenExp
    :: forall arch env aenv exp. Execute arch
    => ExecOpenExp arch env aenv exp
    -> ValR arch env
    -> ValR arch aenv
    -> Par arch (FutureR arch exp)
executeOpenExp rootExp env aenv = travE rootExp
  where
    travE :: ExecOpenExp arch env aenv t -> Par arch (FutureR arch t)
    travE = \case
      Var ix                    -> return $ prj ix env
      Let bnd body              -> travE bnd >>= \x -> executeOpenExp body (env `Push` x) aenv
      Undef                     -> newFull evalUndef
      Const c                   -> newFull (toElt c)
      PrimConst c               -> newFull (evalPrimConst c)
      PrimApp f x               -> lift1 (newFull . evalPrim f) (travE x)
      Tuple t                   -> lift1 (newFull . toTuple) (travT t)
      Prj ix e                  -> lift1 (newFull . evalPrj ix . fromTuple) (travE e)
      Cond p t e                -> cond t e =<< travE p
      While p f x               -> while p f =<< travE x
      IndexAny                  -> newFull Any
      IndexNil                  -> newFull Z
      IndexCons sh sz           -> lift2 (newFull $$ (:.)) (travE sh) (travE sz)
      IndexHead sh              -> lift1 (\(_  :. ix) -> newFull ix) (travE sh)
      IndexTail sh              -> lift1 (\(ix :.  _) -> newFull ix) (travE sh)
      IndexSlice ix slix sh     -> lift2 (newFull $$ indexSlice ix) (travE slix) (travE sh)
      IndexFull ix slix sl      -> lift2 (newFull $$ indexFull  ix) (travE slix) (travE sl)
      ToIndex sh ix             -> lift2 (newFull $$ toIndex) (travE sh) (travE ix)
      FromIndex sh ix           -> lift2 (newFull $$ fromIndex) (travE sh) (travE ix)
      Intersect sh1 sh2         -> lift2 (newFull $$ intersect) (travE sh1) (travE sh2)
      Union sh1 sh2             -> lift2 (newFull $$ union) (travE sh1) (travE sh2)
      ShapeSize sh              -> lift1 (newFull . size)  (travE sh)
      Shape acc                 -> lift1 (newFull . shape) (travA acc)
      Index acc ix              -> lift2 index (travA acc) (travE ix)
      LinearIndex acc ix        -> lift2 indexRemoteAsync (travA acc) (travE ix)
      Coerce x                  -> lift1 (newFull . evalCoerce) (travE x)
      Foreign _ f x             -> foreignE f x

    -- Helpers
    -- -------

    travT :: Tuple (ExecOpenExp arch env aenv) t -> Par arch (FutureR arch t)
    travT = \case
      NilTup      -> newFull ()
      SnocTup t e -> liftF2 (,) (travT t) (travE e)

    travA :: ExecOpenAcc arch aenv a -> Par arch (FutureArraysR arch a)
    travA acc = executeOpenAcc acc aenv

    foreignE :: ExecFun arch () (a -> b) -> ExecOpenExp arch env aenv a -> Par arch (FutureR arch b)
    foreignE (Lam (Body f)) x = travE x >>= \e -> executeOpenExp f (Empty `Push` e) Empty
    foreignE _              _ = error "I bless the rains down in Africa"

    travF1 :: ExecOpenFun arch env aenv (a -> b) -> FutureR arch a -> Par arch (FutureR arch b)
    travF1 (Lam (Body f)) x = executeOpenExp f (env `Push` x) aenv
    travF1 _              _ = error "LANAAAAAAAA!"

    while :: ExecOpenFun arch env aenv (a -> Bool) -> ExecOpenFun arch env aenv (a -> a) -> FutureR arch a -> Par arch (FutureR arch a)
    while p f x = do
      ok <- block =<< travF1 p x
      if ok then while p f =<< travF1 f x
            else return x

    cond :: ExecOpenExp arch env aenv a -> ExecOpenExp arch env aenv a -> FutureR arch Bool -> Par arch (FutureR arch a)
    cond yes no p =
      spawn $ do
        c <- block p
        if c then travE yes
             else travE no

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

    index :: (Shape sh, Elt e) => Array sh e -> sh -> Par arch (FutureR arch e)
    index arr ix = indexRemoteAsync arr (toIndex (shape arr) ix)


-- Utilities
-- ---------

{-# INLINE lift1 #-}
lift1 :: Async arch
      => (a -> Par arch (FutureR arch b))
      -> Par arch (FutureR arch a)
      -> Par arch (FutureR arch b)
lift1 f x = do
  x' <- x
  spawn $ f =<< get x'

{-# INLINE lift2 #-}
lift2 :: Async arch
      => (a -> b -> Par arch (FutureR arch c))
      -> Par arch (FutureR arch a)
      -> Par arch (FutureR arch b)
      -> Par arch (FutureR arch c)
lift2 f x y = do
  x' <- x
  y' <- y
  spawn $ id =<< liftM2 f (get x') (get y')

{-# INLINE liftF1 #-}
liftF1 :: Async arch
       => (a -> b)
       -> Par arch (FutureR arch a)
       -> Par arch (FutureR arch b)
liftF1 f x = do
  r  <- new
  x' <- x
  fork $ put r . f =<< get x'
  return r

{-# INLINE liftF2 #-}
liftF2 :: Async arch
       => (a -> b -> c)
       -> Par arch (FutureR arch a)
       -> Par arch (FutureR arch b)
       -> Par arch (FutureR arch c)
liftF2 f x y = do
  r  <- new
  x' <- x
  y' <- y
  fork $ put r =<< liftM2 f (get x') (get y')
  return r

{-# INLINE ($$) #-}
infixr 0 $$
($$) :: (b -> a) -> (c -> d -> b) -> c -> d -> a
(f $$ g) x y = f (g x y)

split :: Execute arch => ArraysR a -> FutureR arch a -> Par arch (FutureArraysR arch a)
split repr x = do
  rs <- newArrays repr
  fork $ get x >>= fill repr rs
  return rs
  where
    fill :: Execute arch => ArraysR a -> FutureArraysR arch a -> a -> Par arch ()
    fill ArraysRunit _ _ = return ()
    fill ArraysRarray r a = put r a
    fill (ArraysRpair repr1 repr2) (r1, r2) (a1, a2) = fill repr1 r1 a1 >> fill repr2 r2 a2
