{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Compile
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Compile (

  Compile(..),
  compileAcc, compileAfun,

  ExecOpenAcc(..), ExecOpenAfun,
  ExecAcc, ExecAfun,
  ExecExp, ExecOpenExp,
  ExecFun, ExecOpenFun

) where

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar                        hiding ( Foreign )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo
import qualified Data.Array.Accelerate.Array.Sugar              as A

import Data.Array.Accelerate.LLVM.Array.Data
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.Foreign
import Data.Array.Accelerate.LLVM.State

-- standard library
import Data.IntMap                                              ( IntMap )
import Data.Monoid
import Control.Applicative                                      hiding ( Const )
import Prelude                                                  hiding ( exp, unzip )


class Foreign arch => Compile arch where
  data ExecutableR arch

  -- | Compile an accelerate computation into some backend-specific executable format
  --
  compileForTarget
      :: DelayedOpenAcc aenv a
      -> Gamma aenv
      -> LLVM arch (ExecutableR arch)


-- | Annotate an open array expression with the information necessary to execute
-- each node directly.
--
data ExecOpenAcc arch aenv a where
  ExecAcc  :: ExecutableR arch
           -> Gamma aenv
           -> PreOpenAcc (ExecOpenAcc arch) aenv a
           -> ExecOpenAcc arch aenv a

  EmbedAcc :: (Shape sh, Elt e)
           => PreExp (ExecOpenAcc arch) aenv sh
           -> ExecOpenAcc arch aenv (Array sh e)

  UnzipAcc :: (Elt t, Elt e)
           => TupleIdx (TupleRepr t) e
           -> Idx aenv (Array sh t)
           -> ExecOpenAcc arch aenv (Array sh e)


-- An annotated AST suitable for execution
--
type ExecAcc arch a     = ExecOpenAcc arch () a
type ExecAfun arch a    = PreAfun (ExecOpenAcc arch) a

type ExecOpenAfun arch  = PreOpenAfun (ExecOpenAcc arch)
type ExecOpenExp arch   = PreOpenExp (ExecOpenAcc arch)
type ExecOpenFun arch   = PreOpenFun (ExecOpenAcc arch)

type ExecExp arch       = ExecOpenExp arch ()
type ExecFun arch       = ExecOpenFun arch ()


-- | Initialise code generation, compilation, and data transfer (if required)
-- for an array expression. The returned array computation is annotated to be
-- suitable for execution on the target:
--
--   * A list of the array variables embedded within scalar expressions
--
--   * The compiled LLVM code required to execute the kernel
--
{-# INLINEABLE compileAcc #-}
compileAcc
    :: (Compile arch, Remote arch)
    => DelayedAcc a
    -> LLVM arch (ExecAcc arch a)
compileAcc = compileOpenAcc

{-# INLINEABLE compileAfun #-}
compileAfun
    :: (Compile arch, Remote arch)
    => DelayedAfun f
    -> LLVM arch (ExecAfun arch f)
compileAfun = compileOpenAfun


{-# INLINEABLE compileOpenAfun #-}
compileOpenAfun
    :: (Compile arch, Remote arch)
    => DelayedOpenAfun aenv f
    -> LLVM arch (PreOpenAfun (ExecOpenAcc arch) aenv f)
compileOpenAfun (Alam l)  = Alam  <$> compileOpenAfun l
compileOpenAfun (Abody b) = Abody <$> compileOpenAcc b


{-# INLINEABLE compileOpenAcc #-}
compileOpenAcc
    :: forall arch _aenv _a. (Compile arch, Remote arch)
    => DelayedOpenAcc _aenv _a
    -> LLVM arch (ExecOpenAcc arch _aenv _a)
compileOpenAcc = traverseAcc
  where
    -- Traverse an open array expression in depth-first order. The top-level
    -- function 'traverseAcc' is intended for manifest arrays that we will
    -- generate LLVM code for. Array valued sub-terms, which might be manifest
    -- or delayed, are handled separately.
    --
    -- As the AST is traversed, we also collect a set of the indices of free
    -- array variables that were referred to within scalar sub-expressions.
    -- These will be required during code generation and execution.
    --
    traverseAcc :: forall aenv arrs. DelayedOpenAcc aenv arrs -> LLVM arch (ExecOpenAcc arch aenv arrs)
    traverseAcc Delayed{}              = $internalError "compileOpenAcc" "unexpected delayed array"
    traverseAcc topAcc@(Manifest pacc) =
      case pacc of
        -- Environment and control flow
        Avar ix                 -> node $ pure (Avar ix)
        Alet a b                -> node . pure =<< Alet         <$> traverseAcc a <*> traverseAcc b
        Apply f a               -> node =<< liftA2 Apply        <$> travAF f <*> travA a
        Awhile p f a            -> node =<< liftA3 Awhile       <$> travAF p <*> travAF f <*> travA a
        Acond p t e             -> node =<< liftA3 Acond        <$> travE  p <*> travA  t <*> travA e
        Atuple tup              -> node =<< liftA  Atuple       <$> travAtup tup
        Aprj ix tup             -> node =<< liftA (Aprj ix)     <$> travA    tup

        -- Foreign
        Aforeign ff afun a      -> foreignA ff afun a

        -- Array injection
        Unit e                  -> node =<< liftA  Unit         <$> travE e
        Use arrs                -> useRemote (toArr arrs::arrs) >> node (pure (Use arrs))

        -- Index space transforms
        Reshape s a             -> node =<< liftA2 Reshape              <$> travE s <*> travA a
        Replicate slix e a      -> exec =<< liftA2 (Replicate slix)     <$> travE e <*> travA a
        Slice slix a e          -> exec =<< liftA2 (Slice slix)         <$> travA a <*> travE e
        Backpermute e f a       -> exec =<< liftA3 Backpermute          <$> travE e <*> travF f <*> travA a

        -- Producers
        Generate e f            -> exec =<< liftA2 Generate             <$> travE e <*> travF f
        Map f a
          | Just b <- unzip f a -> return b
          | otherwise           -> exec =<< liftA2 Map                  <$> travF f <*> travA a
        ZipWith f a b           -> exec =<< liftA3 ZipWith              <$> travF f <*> travA a <*> travA b
        Transform e p f a       -> exec =<< liftA4 Transform            <$> travE e <*> travF p <*> travF f <*> travA a

        -- Consumers
        Fold f z a              -> exec =<< liftA3 Fold                 <$> travF f <*> travE z <*> travA a
        Fold1 f a               -> exec =<< liftA2 Fold1                <$> travF f <*> travA a
        FoldSeg f e a s         -> exec =<< liftA4 FoldSeg              <$> travF f <*> travE e <*> travA a <*> travA s
        Fold1Seg f a s          -> exec =<< liftA3 Fold1Seg             <$> travF f <*> travA a <*> travA s
        Scanl f e a             -> exec =<< liftA3 Scanl                <$> travF f <*> travE e <*> travA a
        Scanl' f e a            -> exec =<< liftA3 Scanl'               <$> travF f <*> travE e <*> travA a
        Scanl1 f a              -> exec =<< liftA2 Scanl1               <$> travF f <*> travA a
        Scanr f e a             -> exec =<< liftA3 Scanr                <$> travF f <*> travE e <*> travA a
        Scanr' f e a            -> exec =<< liftA3 Scanr'               <$> travF f <*> travE e <*> travA a
        Scanr1 f a              -> exec =<< liftA2 Scanr1               <$> travF f <*> travA a
        Permute f d g a         -> exec =<< liftA4 Permute              <$> travF f <*> travA d <*> travF g <*> travA a
        Stencil f b a           -> exec =<< liftA2 (flip Stencil b)     <$> travF f <*> travM a
        Stencil2 f b1 a1 b2 a2  -> exec =<< liftA3 stencil2             <$> travF f <*> travM a1 <*> travM a2
          where stencil2 f' a1' a2' = Stencil2 f' b1 a1' b2 a2'

      where
        travA :: DelayedOpenAcc aenv a -> LLVM arch (IntMap (Idx' aenv), ExecOpenAcc arch aenv a)
        travA acc = case acc of
          Manifest{}    -> pure                    <$> traverseAcc acc
          Delayed{..}   -> liftA2 (const EmbedAcc) <$> travF indexD <*> travE extentD

        travM :: (Shape sh, Elt e)
              => DelayedOpenAcc aenv (Array sh e) -> LLVM arch (IntMap (Idx' aenv), ExecOpenAcc arch aenv (Array sh e))
        travM acc = case acc of
          Manifest (Avar ix) -> (freevar ix,) <$> traverseAcc acc
          _                  -> $internalError "compileOpenAcc" "expected array variable"

        travAF :: DelayedOpenAfun aenv f
               -> LLVM arch (IntMap (Idx' aenv), PreOpenAfun (ExecOpenAcc arch) aenv f)
        travAF afun = pure <$> compileOpenAfun afun

        travAtup :: Atuple (DelayedOpenAcc aenv) a
                 -> LLVM arch (IntMap (Idx' aenv), Atuple (ExecOpenAcc arch aenv) a)
        travAtup NilAtup        = return (pure NilAtup)
        travAtup (SnocAtup t a) = liftA2 SnocAtup <$> travAtup t <*> travA a

        travF :: DelayedOpenFun env aenv t
              -> LLVM arch (IntMap (Idx' aenv), PreOpenFun (ExecOpenAcc arch) env aenv t)
        travF (Body b)  = liftA Body <$> travE b
        travF (Lam  f)  = liftA Lam  <$> travF f

        exec :: (IntMap (Idx' aenv), PreOpenAcc (ExecOpenAcc arch) aenv arrs)
             -> LLVM arch (ExecOpenAcc arch aenv arrs)
        exec (aenv, eacc) = do
          let aval = makeGamma aenv
          kernel <- build topAcc aval
          return $! ExecAcc kernel aval eacc

        node :: (IntMap (Idx' aenv'), PreOpenAcc (ExecOpenAcc arch) aenv' arrs')
             -> LLVM arch (ExecOpenAcc arch aenv' arrs')
        node = fmap snd . wrap

        wrap :: (IntMap (Idx' aenv'), PreOpenAcc (ExecOpenAcc arch) aenv' arrs')
             -> LLVM arch (IntMap (Idx' aenv'), ExecOpenAcc arch aenv' arrs')
        wrap = return . liftA (ExecAcc noKernel mempty)

        -- Unzips of manifest array data can be done in constant time without
        -- executing any array programs. We split them out here into a separate
        -- case of 'ExecAcc' so that the execution phase does not have to
        -- continually perform the below check (in particular; 'run1').
        unzip :: PreFun DelayedOpenAcc aenv (a -> b)
              -> DelayedOpenAcc aenv (Array sh a)
              -> Maybe (ExecOpenAcc arch aenv (Array sh b))
        unzip f a
          | Lam (Body (Prj tix (Var ZeroIdx)))  <- f
          , Delayed sh index _                  <- a
          , Shape u                             <- sh
          , Manifest (Avar ix)                  <- u
          , Lam (Body (Index v (Var ZeroIdx)))  <- index
          , Just Refl                           <- match u v
          = Just (UnzipAcc tix ix)
        unzip _ _
          = Nothing

        -- Is there a foreign version available for this backend? If so, we
        -- leave that node in the AST and strip out the remaining terms.
        -- Subsequent phases, if they encounter a foreign node, can assume that
        -- it is for them. Otherwise, drop this term and continue walking down
        -- the list of alternate implementations.
        foreignA :: (Arrays a, Arrays b, A.Foreign asm)
                 => asm         (a -> b)
                 -> DelayedAfun (a -> b)
                 -> DelayedOpenAcc aenv a
                 -> LLVM arch (ExecOpenAcc arch aenv b)
        foreignA asm f a =
          case foreignAcc (undefined :: arch) asm of
            Just{}  -> node =<< liftA (Aforeign asm err) <$> travA a
            Nothing -> traverseAcc $ Manifest (Apply (weaken absurd f) a)
            where
              absurd :: Idx () t -> Idx aenv t
              absurd = error "complicated stuff in simple words"
              err    = $internalError "compile" "attempt to use fallback in foreign function"

        -- sadness
        noKernel  = $internalError "compile" "no kernel module for this node"


    -- Traverse a scalar expression
    --
    travE :: DelayedOpenExp env aenv e
          -> LLVM arch (IntMap (Idx' aenv), PreOpenExp (ExecOpenAcc arch) env aenv e)
    travE exp =
      case exp of
        Var ix                  -> return $ pure (Var ix)
        Const c                 -> return $ pure (Const c)
        PrimConst c             -> return $ pure (PrimConst c)
        IndexAny                -> return $ pure IndexAny
        IndexNil                -> return $ pure IndexNil
        Foreign ff f x          -> foreignE ff f x
        --
        Let a b                 -> liftA2 Let                   <$> travE a <*> travE b
        IndexCons t h           -> liftA2 IndexCons             <$> travE t <*> travE h
        IndexHead h             -> liftA  IndexHead             <$> travE h
        IndexTail t             -> liftA  IndexTail             <$> travE t
        IndexSlice slix x s     -> liftA2 (IndexSlice slix)     <$> travE x <*> travE s
        IndexFull slix x s      -> liftA2 (IndexFull slix)      <$> travE x <*> travE s
        ToIndex s i             -> liftA2 ToIndex               <$> travE s <*> travE i
        FromIndex s i           -> liftA2 FromIndex             <$> travE s <*> travE i
        Tuple t                 -> liftA  Tuple                 <$> travT t
        Prj ix e                -> liftA  (Prj ix)              <$> travE e
        Cond p t e              -> liftA3 Cond                  <$> travE p <*> travE t <*> travE e
        While p f x             -> liftA3 While                 <$> travF p <*> travF f <*> travE x
        PrimApp f e             -> liftA  (PrimApp f)           <$> travE e
        Index a e               -> liftA2 Index                 <$> travA a <*> travE e
        LinearIndex a e         -> liftA2 LinearIndex           <$> travA a <*> travE e
        Shape a                 -> liftA  Shape                 <$> travA a
        ShapeSize e             -> liftA  ShapeSize             <$> travE e
        Intersect x y           -> liftA2 Intersect             <$> travE x <*> travE y
        Union x y               -> liftA2 Union                 <$> travE x <*> travE y

      where
        travA :: (Shape sh, Elt e)
              => DelayedOpenAcc aenv (Array sh e)
              -> LLVM arch (IntMap (Idx' aenv), ExecOpenAcc arch aenv (Array sh e))
        travA a = do
          a'    <- traverseAcc a
          return $ (bind a', a')

        travT :: Tuple (DelayedOpenExp env aenv) t
              -> LLVM arch (IntMap (Idx' aenv), Tuple (PreOpenExp (ExecOpenAcc arch) env aenv) t)
        travT NilTup        = return (pure NilTup)
        travT (SnocTup t e) = liftA2 SnocTup <$> travT t <*> travE e

        travF :: DelayedOpenFun env aenv t
              -> LLVM arch (IntMap (Idx' aenv), PreOpenFun (ExecOpenAcc arch) env aenv t)
        travF (Body b)  = liftA Body <$> travE b
        travF (Lam  f)  = liftA Lam  <$> travF f

        bind :: (Shape sh, Elt e) => ExecOpenAcc arch aenv (Array sh e) -> IntMap (Idx' aenv)
        bind (ExecAcc _ _ (Avar ix)) = freevar ix
        bind _                       = $internalError "bind" "expected array variable"

        foreignE :: (Elt a, Elt b, A.Foreign asm)
                 => asm           (a -> b)
                 -> DelayedFun () (a -> b)
                 -> DelayedOpenExp env aenv a
                 -> LLVM arch (IntMap (Idx' aenv), PreOpenExp (ExecOpenAcc arch) env aenv b)
        foreignE asm f x =
          case foreignExp (undefined :: arch) asm of
            Just{}                      -> liftA (Foreign asm err) <$> travE x
            Nothing | Lam (Body b) <- f -> liftA2 Let              <$> travE x <*> travE (weaken absurd (weakenE zero b))
            _                           -> error "the slow regard of silent things"
          where
            absurd :: Idx () t -> Idx aenv t
            absurd = error "Look to my coming, at first light, on the fifth day. At dawn, look to the East."

            zero :: Idx ((), a) t -> Idx (env,a) t
            zero ZeroIdx = ZeroIdx
            zero _       = error "There are three things all wise men fear: the sea in storm, a night with no moon, and the anger of a gentle man."

            err :: ExecFun arch () (a -> b)
            err = $internalError "foreignE" "attempt to use fallback in foreign expression"


-- Compilation
-- -----------

-- | Generate code that will be used to evaluate an array computation. Pass the
-- generated code to the appropriate backend handler, which may then, for
-- example, compile and link the code into the running executable.
--
-- TODO:
--  * asynchronous compilation
--  * kernel caching
--
{-# INLINEABLE build #-}
build :: forall arch aenv a. Compile arch
      => DelayedOpenAcc aenv a
      -> Gamma aenv
      -> LLVM arch (ExecutableR arch)
build acc aenv =
  compileForTarget acc aenv


-- Applicative
-- -----------
--
liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

