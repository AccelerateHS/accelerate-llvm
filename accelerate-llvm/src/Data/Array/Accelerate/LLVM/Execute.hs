{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute
-- Copyright   : [2014..2018] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute (

  Execute(..), Gamma,
  executeAcc,
  executeOpenAcc,

) where

-- accelerate
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Representation               ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                        hiding ( Foreign )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Interpreter                        ( evalPrim, evalPrimConst, evalPrj, evalUndef, evalCoerce )

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.Execute.Environment
import Data.Array.Accelerate.LLVM.Link

-- library
import Control.Monad
import Prelude                                                  hiding ( exp, map, unzip, scanl, scanr, scanl1, scanr1 )


class Remote arch => Execute arch where
  map           :: (Shape sh, Elt b)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh
                -> Par arch (FutureR arch (Array sh b))

  generate      :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh
                -> Par arch (FutureR arch (Array sh e))

  transform     :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh
                -> Par arch (FutureR arch (Array sh e))

  backpermute   :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh
                -> Par arch (FutureR arch (Array sh e))

  fold          :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh :. Int
                -> Par arch (FutureR arch (Array sh e))

  fold1         :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh :. Int
                -> Par arch (FutureR arch (Array sh e))

  foldSeg       :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh :. Int
                -> DIM1
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  fold1Seg      :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh :. Int
                -> DIM1
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  scanl         :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh :. Int
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  scanl1        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh :. Int
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  scanl'        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh :. Int
                -> Par arch (FutureR arch (Array (sh:.Int) e, Array sh e))

  scanr         :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh :. Int
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  scanr1        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh :. Int
                -> Par arch (FutureR arch (Array (sh:.Int) e))

  scanr'        :: (Shape sh, Elt e)
                => ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh :. Int
                -> Par arch (FutureR arch (Array (sh:.Int) e, Array sh e))

  permute       :: (Shape sh, Shape sh', Elt e)
                => Bool               -- ^ update defaults array in-place?
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh
                -> Array sh' e
                -> Par arch (FutureR arch (Array sh' e))

  stencil1      :: (Shape sh, Elt e)
                => sh
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh
                -> Par arch (FutureR arch (Array sh e))

  stencil2      :: (Shape sh, Elt e)
                => sh
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh
                -> sh
                -> Par arch (FutureR arch (Array sh e))

  aforeign      :: (Arrays as, Arrays bs)
                => String
                -> (as -> Par arch (FutureR arch bs))
                -> as
                -> Par arch (FutureR arch bs)


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
    -> Par arch (FutureR arch a)
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
  executeOpenAfun Abody{}  _ _    = $internalError "executeOpenAfun" "malformed array function"
  executeOpenAfun (Alam f) k arrs =
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
    -> Par arch (FutureR arch arrs)
executeOpenAcc !topAcc !aenv = travA topAcc
  where
    travA :: ExecOpenAcc arch aenv a -> Par arch (FutureR arch a)
    travA (EvalAcc pacc) =
      case pacc of
        Use arrs            -> spawn $ useRemoteAsync (toArr arrs)
        Unit x              -> unit x
        Avar ix             -> return $ prj ix aenv
        Alet bnd body       -> alet bnd body
        Atuple tup          -> atuple tup
        Alloc sh            -> allocate sh
        Apply f a           -> travAF f =<< spawn (travA a)
        Aprj ix tup         -> liftF1 (evalPrj ix . fromAtuple) (travA tup)
        Acond p t e         -> acond t e =<< travE p
        Awhile p f a        -> awhile p f =<< spawn (travA a)
        Reshape sh ix       -> liftF2 reshape (travE sh) (return $ prj ix aenv)
        Unzip tix ix        -> liftF1 (unzip tix) (return $ prj ix aenv)
        Aforeign str asm a  -> do
          x <- travA a
          spawn $ aforeign str asm =<< get x

    travA (ExecAcc !gamma !kernel pacc) =
      case pacc of
        -- Producers
        Map sh              -> exec1 map         (travE sh)
        Generate sh         -> exec1 generate    (travE sh)
        Transform sh        -> exec1 transform   (travE sh)
        Backpermute sh      -> exec1 backpermute (travE sh)

        -- Consumers
        Fold sh             -> exec1 fold     (travE sh)
        Fold1 sh            -> exec1 fold1    (travE sh)
        FoldSeg sa ss       -> exec2 foldSeg  (travE sa) (travE ss)
        Fold1Seg sa ss      -> exec2 fold1Seg (travE sa) (travE ss)
        Scanl sh            -> exec1 scanl    (travE sh)
        Scanr sh            -> exec1 scanr    (travE sh)
        Scanl1 sh           -> exec1 scanl1   (travE sh)
        Scanr1 sh           -> exec1 scanr1   (travE sh)
        Scanl' sh           -> exec1 scanl'   (travE sh)
        Scanr' sh           -> exec1 scanr'   (travE sh)
        Permute sh d        -> exec2 (permute (inplace d)) (travE sh) (travA d)
        Stencil1 h sh       -> exec1 (stencil1 h) (travE sh)
        Stencil2 h sh1 sh2  -> exec2 (stencil2 h) (travE sh2) (travE sh1)

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

    travAF :: ExecOpenAfun arch aenv (a -> b) -> FutureR arch a -> Par arch (FutureR arch b)
    travAF (Alam (Abody f)) a = executeOpenAcc f (aenv `Push` a)
    travAF _                _ = error "boop!"

    travE :: ExecExp arch aenv t -> Par arch (FutureR arch t)
    travE exp = executeExp exp aenv

    travT :: Atuple (ExecOpenAcc arch aenv) t -> Par arch (FutureR arch t)
    travT NilAtup        = newFull ()
    travT (SnocAtup t a) = liftF2 (,) (travT t) (travA a)

    unit :: Elt t => ExecExp arch aenv t -> Par arch (FutureR arch (Scalar t))
    unit x = do
      x'   <- travE x
      spawn $ newRemoteAsync Z . const =<< get x'

    atuple :: (Arrays t, IsAtuple t)
           => Atuple (ExecOpenAcc arch aenv) (TupleRepr t)
           -> Par arch (FutureR arch t)
    atuple tup = do
      r    <- new
      tup' <- travT tup
      fork $ put r . toAtuple =<< get tup'
      return r

    -- Let bindings
    alet :: ExecOpenAcc arch aenv a -> ExecOpenAcc arch (aenv, a) b -> Par arch (FutureR arch b)
    alet bnd body = do
      bnd'  <- spawn $ executeOpenAcc bnd aenv
      body' <- spawn $ executeOpenAcc body (aenv `Push` bnd')
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
    acond :: ExecOpenAcc arch aenv a -> ExecOpenAcc arch aenv a -> FutureR arch Bool -> Par arch (FutureR arch a)
    acond yes no p =
      spawn $ do
        c <- block p
        if c then travA yes
             else travA no

    -- Array loops
    awhile :: ExecOpenAfun arch aenv (a -> Scalar Bool)
           -> ExecOpenAfun arch aenv (a -> a)
           -> FutureR arch a
           -> Par arch (FutureR arch a)
    awhile p f a = do
      r  <- get =<< travAF p a
      ok <- indexRemote r 0
      if ok then awhile p f =<< travAF f a
            else return a

    -- Pull apart the unzipped struct-of-array representation
    unzip :: forall t sh e. (Elt t, Elt e) => TupleIdx (TupleRepr t) e -> Array sh t -> Array sh e
    unzip tix (Array sh adata) = Array sh $ go tix (eltType (undefined::t)) adata
      where
        go :: TupleIdx v e -> TupleType t' -> ArrayData t' -> ArrayData (EltRepr e)
        go (SuccTupIdx ix) (TypeRpair t _) (AD_Pair x _)           = go ix t x
        go ZeroTupIdx      (TypeRpair _ t) (AD_Pair _ x)
          | Just Refl <- matchTupleType t (eltType (undefined::e)) = x
        go _ _ _                                                   = $internalError "unzip" "inconsistent valuation"

    -- Can the permutation function write directly into the results array?
    inplace :: ExecOpenAcc arch aenv a -> Bool
    inplace (EvalAcc Avar{}) = False
    inplace _                = True


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

    travA :: ExecOpenAcc arch aenv a -> Par arch (FutureR arch a)
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

    index :: Shape sh => Array sh e -> sh -> Par arch (FutureR arch e)
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

