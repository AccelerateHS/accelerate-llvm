{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute
-- Copyright   : [2014..2020] The Accelerate Team
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

import Data.Array.Accelerate.AST                                ( Direction, PreOpenAfun(..), ALeftHandSide, ArrayVar, Fun, OpenFun(..), Exp, OpenExp(..), PrimBool, arraysR, arrayR )
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Interpreter                        ( evalPrim, evalPrimConst, evalCoerceScalar, atraceOp )
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Slice
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Representation.Vec
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Debug.Internal           as Debug

import Data.Array.Accelerate.LLVM.AST                           hiding ( Delayed, Manifest )
import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.Execute.Environment
import Data.Array.Accelerate.LLVM.Link
import qualified Data.Array.Accelerate.LLVM.AST                 as AST

import Control.Monad
import Control.Monad.Trans                                      ( liftIO )
import System.IO.Unsafe
import Prelude                                                  hiding ( exp, map, unzip, scanl, scanr, scanl1, scanr1 )


class Remote arch => Execute arch where
  map           :: Maybe (a :~: b)              -- update values in-place?
                -> ArrayR (Array sh a)
                -> TypeR b
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Array sh a
                -> Par arch (FutureR arch (Array sh b))

  generate      :: ArrayR (Array sh e)
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh
                -> Par arch (FutureR arch (Array sh e))

  transform     :: ArrayR (Array sh a)
                -> ArrayR (Array sh' b)
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh'
                -> Array sh a
                -> Par arch (FutureR arch (Array sh' b))

  backpermute   :: ArrayR (Array sh e)
                -> ShapeR sh'
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> sh'
                -> Array sh e
                -> Par arch (FutureR arch (Array sh' e))

  fold          :: HasInitialValue
                -> ArrayR (Array sh e)
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh, Int) e)
                -> Par arch (FutureR arch (Array sh e))

  foldSeg       :: IntegralType i
                -> HasInitialValue
                -> ArrayR (Array (sh, Int) e)
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh, Int) e)
                -> Delayed (Segments i)
                -> Par arch (FutureR arch (Array (sh, Int) e))

  scan          :: Direction
                -> HasInitialValue
                -> ArrayR (Array (sh, Int) e)
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh, Int) e)
                -> Par arch (FutureR arch (Array (sh, Int) e))

  scan'         :: Direction
                -> ArrayR (Array (sh, Int) e)
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array (sh, Int) e)
                -> Par arch (FutureR arch (Array (sh, Int) e, Array sh e))

  permute       :: Bool                         -- ^ update defaults array in-place?
                -> ArrayR (Array sh e)
                -> ShapeR sh'
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Array sh' e
                -> Delayed (Array sh e)
                -> Par arch (FutureR arch (Array sh' e))

  stencil1      :: TypeR a
                -> ArrayR (Array sh b)
                -> sh
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array sh a)
                -> Par arch (FutureR arch (Array sh b))

  stencil2      :: TypeR a
                -> TypeR b
                -> ArrayR (Array sh c)
                -> sh
                -> ExecutableR arch
                -> Gamma aenv
                -> ValR arch aenv
                -> Delayed (Array sh a)
                -> Delayed (Array sh b)
                -> Par arch (FutureR arch (Array sh c))

  aforeign      :: String
                -> ArraysR as
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
    travA (EvalAcc _ pacc) =
      case pacc of
        Use repr arr        -> spawn $ useRemoteAsync (TupRsingle repr) arr
        Unit tp x           -> unit tp x
        Avar (Var ArrayR{} ix) -> return $ prj ix aenv
        Alet lhs bnd body      -> alet lhs bnd body
        Apair a1 a2            -> liftM2 (,) (travA a1) (travA a2)
        Anil                   -> return ()
        Alloc repr sh          -> allocate repr sh
        Apply _ f a            -> travAF f =<< spawn (travA a)
        Atrace msg a1 a2       -> do
          let repr = arraysR a1
          a1' <- travA a1 >>= blockArrays repr >>= copyToHost repr
          liftIO $ atraceOp msg a1'
          travA a2

        -- We need quite some type applications in the rules for acond and awhile, and cannot use do notation.
        -- For some unknown reason, GHC will "simplify" 'FutureArraysR arch a' to 'FutureR arch a', which is not sound.
        -- It then complains that 'FutureR arch a' isn't assignable to 'FutureArraysR arch a'. By adding explicit
        -- type applications, type checking works fine. This appears to be fixed in GHC 8.8; we don't have problems
        -- with type inference there after removing the explicit type applications.
        Acond p (t :: ExecOpenAcc arch aenv a) e
                               -> (>>=) @(Par arch) @(FutureR arch PrimBool) @(FutureArraysR arch a) (travE p) (acond t e)
        Awhile p f (a :: ExecOpenAcc arch aenv a)
                               -> (>>=) @(Par arch) @(FutureArraysR arch a) @(FutureArraysR arch a)
                                    (spawn @arch @(FutureArraysR arch a) $ travA a)
                                    (awhile p f)
        Reshape shr sh (Var (ArrayR shr' _) ix)
                               -> liftF2 (\s -> reshape shr s shr') (travE sh) (return $ prj ix aenv)
        Unzip tix (Var _ ix)   -> liftF1 (unzip tix) (return $ prj ix aenv)
        Aforeign r str asm a   -> do
          x <- travA a
          y <- spawn $ aforeign str (arraysR a) r asm =<< getArrays (arraysR a) x
          split r y

    travA (ExecAcc _ !gamma !kernel pacc) =
      case pacc of
        -- Producers
        Map tp a               -> exec1 (map_ a (arrayR a) tp) (travA a)
        Generate repr sh       -> exec1 (generate repr) (travE sh)
        Transform repr sh a    -> exec2 (transform (arrayR a) repr) (travE sh) (travA a)
        Backpermute shr sh a   -> exec2 (backpermute (arrayR a) shr) (travE sh) (travA a)

        -- Consumers
        Fold z a               -> exec1 (fold z     $ reduceRank $ arrayR a) (travD a)
        FoldSeg i z a s        -> exec2 (foldSeg i z $             arrayR a) (travD a) (travD s)
        Scan d z a             -> exec1 (scan d z   $              arrayR a) (travD a)
        Scan' d a              -> splitPair
                                $ exec1 (scan' d    $              arrayR a) (travD a)
        Permute d a            -> exec2 (permute_ d (arrayR a) $ arrayRshape $ arrayR d) (travA d) (travD a)
        Stencil1 tpB h a       -> let ArrayR shr tpA = arrayR a
                                  in  exec1 (stencil1 tpA (ArrayR shr tpB) h) (travD a)
        Stencil2 tpC h a b     -> let ArrayR shr tpA = arrayR a
                                      ArrayR _   tpB = arrayR b
                                  in  exec2 (stencil2 tpA tpB (ArrayR shr tpC) h) (travD a) (travD b)

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
              -> Par arch (FutureR arch a, FutureR arch b)
        splitPair x = do
          r1 <- new
          r2 <- new
          fork $ do
            x' <- x
            (a, b) <- get x'
            put r1 a
            put r2 b
          return (r1, r2)

    travAF :: ExecOpenAfun arch aenv (a -> b) -> FutureArraysR arch a -> Par arch (FutureArraysR arch b)
    travAF (Alam lhs (Abody f)) a = executeOpenAcc f $ aenv `push` (lhs, a)
    travAF _                    _ = error "boop!"

    travE :: Exp aenv t -> Par arch (FutureR arch t)
    travE exp = executeExp exp aenv

    travD :: DelayedOpenAcc ExecOpenAcc arch aenv a -> Par arch (FutureR arch (Delayed a))
    travD (AST.Delayed _ sh) = liftF1 Delayed  (travE sh)
    travD (AST.Manifest _ a) = liftF1 Manifest (travA a)

    unit :: TypeR t -> Exp aenv t -> Par arch (FutureR arch (Scalar t))
    unit tp x = do
      x'   <- travE x
      spawn $ newRemoteAsync (ArrayR ShapeRz tp) () . const =<< get x'

    -- Let bindings
    alet :: ALeftHandSide a aenv aenv' -> ExecOpenAcc arch aenv a -> ExecOpenAcc arch aenv' b -> Par arch (FutureArraysR arch b)
    alet lhs bnd body = do
      bnd'  <- spawn $ executeOpenAcc bnd aenv
      body' <- spawn $ executeOpenAcc body $ aenv `push` (lhs, bnd')
      return body'

    -- Allocate an array on the remote device
    allocate :: ArrayR (Array sh e) -> Exp aenv sh -> Par arch (FutureR arch (Array sh e))
    allocate repr sh = do
      r    <- new
      sh'  <- travE sh
      fork $ do
        arr <- allocateRemote repr =<< get sh'
        put r arr
      return r

    -- Array level conditionals
    acond :: ExecOpenAcc arch aenv a
          -> ExecOpenAcc arch aenv a
          -> FutureR arch PrimBool
          -> Par arch (FutureArraysR arch a)
    acond yes no p =
      spawn $ do
        c <- block p
        if toBool c then travA yes
                    else travA no

    -- Array loops
    awhile :: ExecOpenAfun arch aenv (a -> Scalar PrimBool)
           -> ExecOpenAfun arch aenv (a -> a)
           -> FutureArraysR arch a
           -> Par arch (FutureArraysR arch a)
    awhile p f a = do
      r  <- get =<< travAF p a
      ok <- indexRemote (TupRsingle scalarType) r 0
      if toBool ok then awhile p f =<< travAF f a
                   else return a

    -- Pull apart the unzipped struct-of-array representation
    unzip :: UnzipIdx t e -> Array sh t -> Array sh e
    unzip tix (Array sh adata) = Array sh $ go tix adata
      where
        go :: UnzipIdx a b -> ArrayData a -> ArrayData b
        go UnzipUnit                  _       = ()
        go UnzipId                    ad      = ad
        go (UnzipPrj PairIdxLeft  ix) (ad, _) = go ix ad
        go (UnzipPrj PairIdxRight ix) (_, ad) = go ix ad
        go (UnzipPair ix1 ix2)        ad      = (go ix1 ad, go ix2 ad)

    map_ :: ExecOpenAcc arch aenv (Array sh a)
         -> ArrayR (Array sh a)
         -> TypeR b
         -> ExecutableR arch
         -> Gamma aenv
         -> ValR arch aenv
         -> Array sh a
         -> Par arch (FutureR arch (Array sh b))
    map_ a repr@(ArrayR _ tp) tp'
      = map (if inplace a then matchTypeR tp tp' else Nothing) repr tp'

    permute_ :: ExecOpenAcc arch aenv (Array sh' e)
             -> ArrayR (Array sh e)
             -> ShapeR sh'
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
          EvalAcc _ pacc ->
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
    => Exp aenv t
    -> ValR arch aenv
    -> Par arch (FutureR arch t)
executeExp exp aenv = executeOpenExp exp Empty aenv

{-# INLINEABLE executeOpenExp #-}
executeOpenExp
    :: forall arch env aenv exp. Execute arch
    => OpenExp env aenv exp
    -> ValR arch env
    -> ValR arch aenv
    -> Par arch (FutureR arch exp)
executeOpenExp rootExp env aenv = travE rootExp
  where
    travE :: OpenExp env aenv t -> Par arch (FutureR arch t)
    travE = \case
      Evar (Var _ ix)           -> return $ prj ix env
      Let lhs bnd body          -> do
                                     x <- travE bnd
                                     env' <- env `pushE` (lhs, x)
                                     executeOpenExp body env' aenv
      Undef tp                  -> newFull $ undefElt (TupRsingle tp)
      Const _ c                 -> newFull c
      PrimConst c               -> newFull (evalPrimConst c)
      PrimApp f x               -> lift1 (newFull . evalPrim f) (travE x)
      Nil                       -> newFull ()
      Pair e1 e2                -> liftF2 (,) (travE e1) (travE e2)
      VecPack   vecr e          -> liftF1 (pack   vecr) (travE e)
      VecUnpack vecr e          -> liftF1 (unpack vecr) (travE e)
      Case p xs x               -> caseof xs x =<< travE p
      Cond p t e                -> cond t e =<< travE p
      While p f x               -> while p f =<< travE x
      IndexSlice ix slix sh     -> lift2 (newFull $$ indexSlice ix) (travE slix) (travE sh)
      IndexFull ix slix sl      -> lift2 (newFull $$ indexFull  ix) (travE slix) (travE sl)
      ToIndex shr sh ix         -> lift2 (newFull $$ toIndex shr) (travE sh) (travE ix)
      FromIndex shr sh ix       -> lift2 (newFull $$ fromIndex shr) (travE sh) (travE ix)
      ShapeSize shr sh          -> lift1 (newFull . size shr) (travE sh)
      Shape var                 -> lift1 (newFull . shape) (travAvar var)
      Index (Var repr a) ix     -> lift2 (index repr) (travAIdx a) (travE ix)
      LinearIndex (Var (ArrayR _ tp) a) ix -> lift2 (indexRemoteAsync tp) (travAIdx a) (travE ix)
      Coerce t1 t2 x            -> lift1 (newFull . evalCoerceScalar t1 t2) (travE x)
      Foreign _ _ f x           -> foreignE f x

    -- Helpers
    -- -------

    travAvar :: ArrayVar aenv a -> Par arch (FutureR arch a)
    travAvar (Var _ ix) = travAIdx ix

    travAIdx :: Idx aenv a -> Par arch (FutureR arch a)
    travAIdx a = return $ prj a aenv

    foreignE :: Fun () (a -> b) -> OpenExp env aenv a -> Par arch (FutureR arch b)
    foreignE (Lam lhs (Body f)) x = do e    <- travE x
                                       env' <- Empty `pushE` (lhs, e)
                                       executeOpenExp f env' Empty
    foreignE _                  _ = error "I bless the rains down in Africa"

    travF1 :: OpenFun env aenv (a -> b) -> FutureR arch a -> Par arch (FutureR arch b)
    travF1 (Lam lhs (Body f)) x = do env' <- env `pushE` (lhs, x)
                                     executeOpenExp f env' aenv
    travF1 _                  _ = error "LANAAAAAAAA!"

    while :: OpenFun env aenv (a -> PrimBool)
          -> OpenFun env aenv (a -> a)
          -> FutureR arch a
          -> Par arch (FutureR arch a)
    while p f x = do
      ok <- block =<< travF1 p x
      if toBool ok then while p f =<< travF1 f x
                   else return x

    cond :: OpenExp env aenv a
         -> OpenExp env aenv a
         -> FutureR arch PrimBool
         -> Par arch (FutureR arch a)
    cond yes no p =
      spawn $ do
        c <- block p
        if toBool c then travE yes
                    else travE no

    caseof :: [(TAG, OpenExp env aenv a)]
           -> Maybe (OpenExp env aenv a)
           -> FutureR arch TAG
           -> Par arch (FutureR arch a)
    caseof xs d p =
      spawn $ do
        t <- block p
        case lookup t xs of
          Just r  -> travE r
          Nothing -> case d of
                       Just r  -> travE r
                       Nothing -> error "unmatched case"

    indexSlice :: SliceIndex slix sl co sh
               -> slix
               -> sh
               -> sl
    indexSlice ix slix sh = restrict ix slix sh
      where
        restrict :: SliceIndex slix sl co sh -> slix -> sh -> sl
        restrict SliceNil              ()        ()       = ()
        restrict (SliceAll   sliceIdx) (slx, ()) (sl, sz) = (restrict sliceIdx slx sl, sz)
        restrict (SliceFixed sliceIdx) (slx,  _) (sl,  _) = restrict sliceIdx slx sl

    indexFull :: SliceIndex slix sl co sh
              -> slix
              -> sl
              -> sh
    indexFull ix slix sl = extend ix slix sl
      where
        extend :: SliceIndex slix sl co sh -> slix -> sl -> sh
        extend SliceNil              ()        ()       = ()
        extend (SliceAll sliceIdx)   (slx, ()) (sh, sz) = (extend sliceIdx slx sh, sz)
        extend (SliceFixed sliceIdx) (slx, sz) sh       = (extend sliceIdx slx sh, sz)

    index :: ArrayR (Array sh e) -> Array sh e -> sh -> Par arch (FutureR arch e)
    index (ArrayR shr tp) arr ix = indexRemoteAsync tp arr (toIndex shr (shape arr) ix)


-- Utilities
-- ---------

{-# INLINE toBool #-}
toBool :: PrimBool -> Bool
toBool 0 = False
toBool _ = True

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
    fill TupRunit               _        _        = return ()
    fill (TupRsingle ArrayR{})  r        a        = put r a
    fill (TupRpair repr1 repr2) (r1, r2) (a1, a2) = fill repr1 r1 a1 >> fill repr2 r2 a2

