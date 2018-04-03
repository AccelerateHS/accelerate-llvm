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

  CompiledOpenAcc(..), CompiledOpenAfun,
  CompiledAcc, CompiledAfun,
  CompiledExp, CompiledOpenExp,
  CompiledFun, CompiledOpenFun

) where

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar                            hiding ( Foreign )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Sugar                  as A

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.Foreign
import Data.Array.Accelerate.LLVM.State
import qualified Data.Array.Accelerate.LLVM.AST                     as AST

-- standard library
import Data.IntMap                                                  ( IntMap )
import Control.Applicative                                          hiding ( Const )
import Prelude                                                      hiding ( map, unzip, zipWith, scanl, scanl1, scanr, scanr1, exp )


class Foreign arch => Compile arch where
  data ObjectR arch
  -- TODO: Provide serialisation facilities, for on-disk caching etc.

  -- | Compile an accelerate computation into some backend-specific code that
  -- will be used to execute the given array expression. The code is not yet
  -- linked into the running executable.
  --
  compileForTarget
      :: DelayedOpenAcc aenv a
      -> Gamma aenv
      -> LLVM arch (ObjectR arch)


data CompiledOpenAcc arch aenv a where
  BuildAcc  :: Gamma aenv
            -> ObjectR arch
            -> AST.PreOpenAccSkeleton CompiledOpenAcc arch aenv a
            -> CompiledOpenAcc arch aenv a

  PlainAcc  :: Arrays a
            => AST.PreOpenAccCommand  CompiledOpenAcc arch aenv a
            -> CompiledOpenAcc arch aenv a


-- An annotated AST with embedded build products
--
type CompiledOpenAfun arch  = PreOpenAfun (CompiledOpenAcc arch)
type CompiledOpenExp arch   = PreOpenExp (CompiledOpenAcc arch)
type CompiledOpenFun arch   = PreOpenFun (CompiledOpenAcc arch)

type CompiledAcc arch a     = CompiledOpenAcc arch () a
type CompiledAfun arch a    = CompiledOpenAfun arch () a

type CompiledExp arch       = CompiledOpenExp arch ()
type CompiledFun arch       = CompiledOpenFun arch ()


-- | Generate and compile code for an array expression. The returned expression
-- is annotated with the compilation products required to executed each
-- operation on the given target, together with the list of array variables
-- referenced from the embedded scalar expressions.
--
{-# INLINEABLE compileAcc #-}
compileAcc
    :: Compile arch
    => DelayedAcc a
    -> LLVM arch (CompiledAcc arch a)
compileAcc = compileOpenAcc

{-# INLINEABLE compileAfun #-}
compileAfun
    :: Compile arch
    => DelayedAfun f
    -> LLVM arch (CompiledAfun arch f)
compileAfun = compileOpenAfun


{-# INLINEABLE compileOpenAfun #-}
compileOpenAfun
    :: Compile arch
    => DelayedOpenAfun aenv f
    -> LLVM arch (CompiledOpenAfun arch aenv f)
compileOpenAfun (Alam l)  = Alam  <$> compileOpenAfun l
compileOpenAfun (Abody b) = Abody <$> compileOpenAcc b


{-# INLINEABLE compileOpenAcc #-}
compileOpenAcc
    :: forall arch _aenv _a. Compile arch
    => DelayedOpenAcc _aenv _a
    -> LLVM arch (CompiledOpenAcc arch _aenv _a)
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
    traverseAcc :: forall aenv arrs. DelayedOpenAcc aenv arrs -> LLVM arch (CompiledOpenAcc arch aenv arrs)
    traverseAcc Delayed{}              = $internalError "compileOpenAcc" "unexpected delayed array"
    traverseAcc topAcc@(Manifest pacc) =
      case pacc of
        -- Environment and control flow
        Avar ix                     -> plain $ pure (AST.Avar ix)
        Alet a b                    -> plain . pure =<< AST.Alet      <$> traverseAcc a <*> traverseAcc b
        Apply f a                   -> plain =<< liftA2 AST.Apply     <$> travAF f <*> travA a
        Awhile p f a                -> plain =<< liftA3 AST.Awhile    <$> travAF p <*> travAF f <*> travA a
        Acond p t e                 -> plain =<< liftA3 AST.Acond     <$> travE  p <*> travA  t <*> travA e
        Atuple tup                  -> plain =<< liftA  AST.Atuple    <$> travAtup tup
        Aprj ix tup                 -> plain =<< liftA (AST.Aprj ix)  <$> travA    tup

        -- Foreign arrays operations
        Aforeign ff afun a          -> foreignA ff afun a

        -- Uninitialised array allocation
        Generate sh f
          | alloc f                 -> plain =<< liftA AST.Alloc      <$> travE sh

        -- Array injection & manipulation
        Reshape sh a                -> plain =<< liftA2 AST.Reshape   <$> travE sh <*> travM a
        Unit e                      -> plain =<< liftA  AST.Unit      <$> travE e
        Use arrs                    -> plain $ pure (AST.Use arrs)
        Map f a
          | Just (t,x) <- unzip f a -> plain $ pure (AST.Unzip t x)

        -- Skeleton operations resulting in compiled code
        -- Producers
        Map f a                     -> build =<< liftA2 map           <$> travF f  <*> travD a
        Generate sh f               -> build =<< liftA2 generate      <$> travE sh <*> travF f
        Transform sh p f a          -> build =<< liftA4 transform     <$> travE sh <*> travF p <*> travF f <*> travD a
        Backpermute sh f a          -> build =<< liftA3 backpermute   <$> travE sh <*> travF f <*> travD a

        -- Consumers
        Fold f z a                  -> build =<< liftA3 fold          <$> travF f <*> travE z <*> travD a
        Fold1 f a                   -> build =<< liftA2 fold1         <$> travF f <*> travD a
        FoldSeg f z a s             -> build =<< liftA4 foldSeg       <$> travF f <*> travE z <*> travD a <*> travD s
        Fold1Seg f a s              -> build =<< liftA3 fold1Seg      <$> travF f <*> travD a <*> travD s
        Scanl f z a                 -> build =<< liftA3 scanl         <$> travF f <*> travE z <*> travD a
        Scanl' f z a                -> build =<< liftA3 scanl'        <$> travF f <*> travE z <*> travD a
        Scanl1 f a                  -> build =<< liftA2 scanl1        <$> travF f <*> travD a
        Scanr f z a                 -> build =<< liftA3 scanr         <$> travF f <*> travE z <*> travD a
        Scanr' f z a                -> build =<< liftA3 scanr'        <$> travF f <*> travE z <*> travD a
        Scanr1 f a                  -> build =<< liftA2 scanr1        <$> travF f <*> travD a
        Permute f d g a             -> build =<< liftA4 permute       <$> travF f <*> travA d <*> travF g <*> travD a
        Stencil f x a               -> build =<< liftA3 stencil1      <$> travF f <*> travB x <*> travD a
        Stencil2 f x a y b          -> build =<< liftA5 stencil2      <$> travF f <*> travB x <*> travD a <*> travB y <*> travD b

        -- Removed by fusion
        Replicate{}                 -> fusionError
        Slice{}                     -> fusionError
        ZipWith{}                   -> fusionError

      where
        map _ a             = AST.Map a
        generate sh _       = AST.Generate sh
        transform sh _ _ _  = AST.Transform sh
        backpermute sh _ _  = AST.Backpermute sh
        fold _ _ a          = AST.Fold a
        fold1 _ a           = AST.Fold1 a
        foldSeg _ _ a s     = AST.FoldSeg a s
        fold1Seg _ a s      = AST.Fold1Seg a s
        scanl _ _ a         = AST.Scanl a
        scanl1 _ a          = AST.Scanl1 a
        scanl' _ _ a        = AST.Scanl' a
        scanr _ _ a         = AST.Scanr a
        scanr1 _ a          = AST.Scanr1 a
        scanr' _ _ a        = AST.Scanr' a
        permute _ d _ a     = AST.Permute a d
        stencil1 _ _ a      = AST.Stencil a
        stencil2 _ _ a _ b  = AST.Stencil2 a b

        fusionError :: error
        fusionError = $internalError "execute" $ "unexpected fusible material: " ++ showPreAccOp pacc

        travA :: DelayedOpenAcc aenv a -> LLVM arch (IntMap (Idx' aenv), CompiledOpenAcc arch aenv a)
        travA acc = pure <$> traverseAcc acc

        travD :: (Shape sh, Elt e)
              => DelayedOpenAcc aenv (Array sh e)
              -> LLVM arch (IntMap (Idx' aenv), PreExp (CompiledOpenAcc arch) aenv sh)
        travD Manifest{}  = $internalError "compileOpenAcc" "expected delayed array"
        travD Delayed{..} = liftA2 (flip const) <$> travF indexD <*> travE extentD

        travM :: (Shape sh, Elt e)
              => DelayedOpenAcc aenv (Array sh e)
              -> LLVM arch (IntMap (Idx' aenv), Idx aenv (Array sh e))
        travM (Manifest (Avar ix)) = return (freevar ix, ix)
        travM _                    = $internalError "compileOpenAcc" "expected array variable"

        travAF :: DelayedOpenAfun aenv f
               -> LLVM arch (IntMap (Idx' aenv), CompiledOpenAfun arch aenv f)
        travAF afun = pure <$> compileOpenAfun afun

        travAtup :: Atuple (DelayedOpenAcc aenv) a
                 -> LLVM arch (IntMap (Idx' aenv), Atuple (CompiledOpenAcc arch aenv) a)
        travAtup NilAtup        = return (pure NilAtup)
        travAtup (SnocAtup t a) = liftA2 SnocAtup <$> travAtup t <*> travA a

        travF :: DelayedOpenFun env aenv t
              -> LLVM arch (IntMap (Idx' aenv), CompiledOpenFun arch env aenv t)
        travF (Body b)  = liftA Body <$> travE b
        travF (Lam  f)  = liftA Lam  <$> travF f

        travB :: PreBoundary DelayedOpenAcc aenv t
              -> LLVM arch (IntMap (Idx' aenv), PreBoundary (CompiledOpenAcc arch) aenv t)
        travB Clamp        = return $ pure Clamp
        travB Mirror       = return $ pure Mirror
        travB Wrap         = return $ pure Wrap
        travB (Constant c) = return $ pure (Constant c)
        travB (Function f) = liftA Function <$> travF f

        build :: (IntMap (Idx' aenv), AST.PreOpenAccSkeleton CompiledOpenAcc arch aenv arrs)
              -> LLVM arch (CompiledOpenAcc arch aenv arrs)
        build (aenv, eacc) = do
          let aval = makeGamma aenv
          kernel <- compileForTarget topAcc aval
          return $! BuildAcc aval kernel eacc

        plain :: Arrays arrs'
              => (IntMap (Idx' aenv'), AST.PreOpenAccCommand CompiledOpenAcc arch aenv' arrs')
              -> LLVM arch (CompiledOpenAcc arch aenv' arrs')
        plain (_, eacc) = return (PlainAcc eacc)

        -- Filling an array with undefined values is equivalent to allocating an
        -- uninitialised array. We look for this specific pattern because we
        -- expect it to appear only in certain places, e.g. as the default array
        -- in a 'permute' where the default values are never used. Note however
        -- that the simplifier does not take into account 'undef' values. For
        -- example, the following transformation is valid:
        --
        --   x + undef  ~~>  undef
        --
        -- so it is still possible to generate empty functions which we will
        -- execute, even though they do nothing (except incur scheduler
        -- overhead).
        --
        alloc :: (Shape sh, Elt e)
              => PreFun DelayedOpenAcc aenv (sh -> e)
              -> Bool
        alloc f
          | Lam (Body Undef) <- f = True
          | otherwise             = False

        -- Unzips of manifest array data can be done in constant time without
        -- executing any array programs. We split them out here into a separate
        -- case so that the execution phase does not have to continually perform
        -- the below check.
        --
        unzip :: forall sh a b. Elt a
              => PreFun DelayedOpenAcc aenv (a -> b)
              -> DelayedOpenAcc aenv (Array sh a)
              -> Maybe (TupleIdx (TupleRepr a) b, Idx aenv (Array sh a))
        unzip _ _
          | TypeRscalar VectorScalarType{}      <- eltType (undefined::a)
          = Nothing
        unzip f a
          | Lam (Body (Prj tix (Var ZeroIdx)))  <- f
          , Delayed sh index _                  <- a
          , Shape u                             <- sh
          , Manifest (Avar ix)                  <- u
          , Lam (Body (Index v (Var ZeroIdx)))  <- index
          , Just Refl                           <- match u v
          = Just (tix, ix)
        unzip _ _
          = Nothing

        -- Is there a foreign version available for this backend? If so, take
        -- the foreign function and drop the remaining terms. Otherwise, drop
        -- this term and continue walking down the list of alternate
        -- implementations.
        --
        foreignA :: (Arrays a, Arrays b, A.Foreign asm)
                 => asm         (a -> b)
                 -> DelayedAfun (a -> b)
                 -> DelayedOpenAcc aenv a
                 -> LLVM arch (CompiledOpenAcc arch aenv b)
        foreignA ff f a =
          case foreignAcc (undefined :: arch) ff of
            Just asm -> plain =<< liftA (AST.Aforeign (strForeign ff) asm) <$> travA a
            Nothing  -> traverseAcc $ Manifest (Apply (weaken absurd f) a)
            where
              absurd :: Idx () t -> Idx aenv t
              absurd = error "complicated stuff in simple words"

    -- Traverse a scalar expression
    --
    travE :: DelayedOpenExp env aenv e
          -> LLVM arch (IntMap (Idx' aenv), PreOpenExp (CompiledOpenAcc arch) env aenv e)
    travE exp =
      case exp of
        Var ix                  -> return $ pure (Var ix)
        Const c                 -> return $ pure (Const c)
        PrimConst c             -> return $ pure (PrimConst c)
        Undef                   -> return $ pure Undef
        IndexAny                -> return $ pure IndexAny
        IndexNil                -> return $ pure IndexNil
        Foreign ff f x          -> foreignE ff f x
        --
        Let a b                 -> liftA2 Let               <$> travE a <*> travE b
        IndexCons t h           -> liftA2 IndexCons         <$> travE t <*> travE h
        IndexHead h             -> liftA  IndexHead         <$> travE h
        IndexTail t             -> liftA  IndexTail         <$> travE t
        IndexSlice slix x s     -> liftA2 (IndexSlice slix) <$> travE x <*> travE s
        IndexFull slix x s      -> liftA2 (IndexFull slix)  <$> travE x <*> travE s
        ToIndex s i             -> liftA2 ToIndex           <$> travE s <*> travE i
        FromIndex s i           -> liftA2 FromIndex         <$> travE s <*> travE i
        Tuple t                 -> liftA  Tuple             <$> travT t
        Prj ix e                -> liftA  (Prj ix)          <$> travE e
        Cond p t e              -> liftA3 Cond              <$> travE p <*> travE t <*> travE e
        While p f x             -> liftA3 While             <$> travF p <*> travF f <*> travE x
        PrimApp f e             -> liftA  (PrimApp f)       <$> travE e
        Index a e               -> liftA2 Index             <$> travA a <*> travE e
        LinearIndex a e         -> liftA2 LinearIndex       <$> travA a <*> travE e
        Shape a                 -> liftA  Shape             <$> travA a
        ShapeSize e             -> liftA  ShapeSize         <$> travE e
        Intersect x y           -> liftA2 Intersect         <$> travE x <*> travE y
        Union x y               -> liftA2 Union             <$> travE x <*> travE y
        Coerce x                -> liftA  Coerce            <$> travE x

      where
        travA :: (Shape sh, Elt e)
              => DelayedOpenAcc aenv (Array sh e)
              -> LLVM arch (IntMap (Idx' aenv), CompiledOpenAcc arch aenv (Array sh e))
        travA a = do
          a'    <- traverseAcc a
          return $ (bind a', a')

        travT :: Tuple (DelayedOpenExp env aenv) t
              -> LLVM arch (IntMap (Idx' aenv), Tuple (PreOpenExp (CompiledOpenAcc arch) env aenv) t)
        travT NilTup        = return (pure NilTup)
        travT (SnocTup t e) = liftA2 SnocTup <$> travT t <*> travE e

        travF :: DelayedOpenFun env aenv t
              -> LLVM arch (IntMap (Idx' aenv), PreOpenFun (CompiledOpenAcc arch) env aenv t)
        travF (Body b)  = liftA Body <$> travE b
        travF (Lam  f)  = liftA Lam  <$> travF f

        bind :: (Shape sh, Elt e) => CompiledOpenAcc arch aenv (Array sh e) -> IntMap (Idx' aenv)
        bind (PlainAcc (AST.Avar ix)) = freevar ix
        bind _                        = $internalError "bind" "expected array variable"

        foreignE :: (Elt a, Elt b, A.Foreign asm)
                 => asm           (a -> b)
                 -> DelayedFun () (a -> b)
                 -> DelayedOpenExp env aenv a
                 -> LLVM arch (IntMap (Idx' aenv), PreOpenExp (CompiledOpenAcc arch) env aenv b)
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

            err :: CompiledFun arch () (a -> b)
            err = $internalError "foreignE" "attempt to use fallback in foreign expression"


-- Applicative
-- -----------
--
liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d g = f <$> a <*> b <*> c <*> d <*> g

