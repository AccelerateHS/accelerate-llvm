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
import qualified Data.Array.Accelerate.Array.Sugar                  as A

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.Foreign
import Data.Array.Accelerate.LLVM.State
import qualified Data.Array.Accelerate.LLVM.AST                     as AST

-- standard library
import Data.IntMap                                                  ( IntMap )
import Control.Applicative                                          hiding ( Const )
import Prelude                                                      hiding ( exp, unzip )


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
            -> AST.PreOpenAccSkeleton (CompiledOpenAcc arch) aenv a
            -> CompiledOpenAcc arch aenv a

  PlainAcc  :: Arrays a
            => AST.PreOpenAccCommand  (CompiledOpenAcc arch) aenv a
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
        Alet a b                    -> plain . pure =<< AST.Alet        <$> traverseAcc a <*> traverseAcc b
        Apply f a                   -> plain =<< liftA2 AST.Apply       <$> travAF f <*> travA a
        Awhile p f a                -> plain =<< liftA3 AST.Awhile      <$> travAF p <*> travAF f <*> travA a
        Acond p t e                 -> plain =<< liftA3 AST.Acond       <$> travE  p <*> travA  t <*> travA e
        Atuple tup                  -> plain =<< liftA  AST.Atuple      <$> travAtup tup
        Aprj ix tup                 -> plain =<< liftA (AST.Aprj ix)    <$> travA    tup

        -- Foreign arrays operations
        Aforeign ff afun a          -> foreignA ff afun a

        -- Array injection & manipulation
        Reshape sh a                -> plain =<< liftA2 AST.Reshape     <$> travE sh <*> travM a
        Unit e                      -> plain =<< liftA  AST.Unit        <$> travE e
        Use arrs                    -> plain $ pure (AST.Use arrs)
        Map f a
          | Just (t,x) <- unzip f a -> plain $ pure (AST.Unzip t x)

        -- Skeleton operations resulting in compiled code
        -- Producers
        Map f a                     -> build =<< liftA  AST.Map         <$> travD a  <*  travF f
        Generate sh f               -> build =<< liftA  AST.Generate    <$> travE sh <*  travF f
        Transform sh p f a          -> build =<< liftA  AST.Transform   <$> travE sh <*  travF p <* travF f <* travD a
        Backpermute sh f a          -> build =<< liftA  AST.Backpermute <$> travE sh <*  travF f <* travD a

        -- Consumers
        Fold f z a                  -> build =<< liftA  AST.Fold        <$> travD a  <*  travF f <* travE z
        Fold1 f a                   -> build =<< liftA  AST.Fold1       <$> travD a  <*  travF f
        FoldSeg f z a s             -> build =<< liftA2 AST.FoldSeg     <$> travD a  <*> travD s <* travF f <* travE z
        Fold1Seg f a s              -> build =<< liftA2 AST.Fold1Seg    <$> travD a  <*> travD s <* travF f
        Scanl f z a                 -> build =<< liftA  AST.Scanl       <$> travD a  <*  travF f <* travE z
        Scanl' f z a                -> build =<< liftA  AST.Scanl'      <$> travD a  <*  travF f <* travE z
        Scanl1 f a                  -> build =<< liftA  AST.Scanl1      <$> travD a  <*  travF f
        Scanr f z a                 -> build =<< liftA  AST.Scanr       <$> travD a  <*  travF f <* travE z
        Scanr' f z a                -> build =<< liftA  AST.Scanr'      <$> travD a  <*  travF f <* travE z
        Scanr1 f a                  -> build =<< liftA  AST.Scanr1      <$> travD a  <*  travF f
        Permute f d g a             -> build =<< liftA2 AST.Permute     <$> travD a  <*> travA d <* travF f <* travF g
        Stencil f _ a               -> build =<< liftA  AST.Stencil     <$> travM a  <*  travF f
        Stencil2 f _ a _ b          -> build =<< liftA2 AST.Stencil2    <$> travM a  <*> travM b <* travF f

        -- Removed by fusion
        Replicate{}                 -> fusionError
        Slice{}                     -> fusionError
        ZipWith{}                   -> fusionError

      where
        fusionError :: error
        fusionError = $internalError "execute" $ "unexpected fusible material: " ++ showPreAccOp pacc

        travA :: DelayedOpenAcc aenv a -> LLVM arch (IntMap (Idx' aenv), CompiledOpenAcc arch aenv a)
        travA acc = pure <$> traverseAcc acc

        travD :: (Shape sh, Elt e)
              => DelayedOpenAcc aenv (Array sh e)
              -> LLVM arch (IntMap (Idx' aenv), PreExp (CompiledOpenAcc arch) aenv sh)
        travD Manifest{}  = $internalError "compileOpenAcc" "expected delayed array"
        travD Delayed{..} = travF indexD *> travE extentD

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

        build :: (IntMap (Idx' aenv), AST.PreOpenAccSkeleton (CompiledOpenAcc arch) aenv arrs)
              -> LLVM arch (CompiledOpenAcc arch aenv arrs)
        build (aenv, eacc) = do
          let aval = makeGamma aenv
          kernel <- compileForTarget topAcc aval
          return $! BuildAcc aval kernel eacc

        plain :: Arrays arrs'
              => (IntMap (Idx' aenv'), AST.PreOpenAccCommand (CompiledOpenAcc arch) aenv' arrs')
              -> LLVM arch (CompiledOpenAcc arch aenv' arrs')
        plain (_, eacc) = return (PlainAcc eacc)

        -- Unzips of manifest array data can be done in constant time without
        -- executing any array programs. We split them out here into a separate
        -- case so that the execution phase does not have to continually perform
        -- the below check.
        --
        unzip :: PreFun DelayedOpenAcc aenv (a -> b)
              -> DelayedOpenAcc aenv (Array sh a)
              -> Maybe (TupleIdx (TupleRepr a) b, Idx aenv (Array sh a))
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

        -- Is there a foreign version available for this backend? If so, we
        -- leave that node in the AST and strip out the remaining terms.
        -- Subsequent phases, if they encounter a foreign node, can assume that
        -- it is for them. Otherwise, drop this term and continue walking down
        -- the list of alternate implementations.
        --
        foreignA :: (Arrays a, Arrays b, A.Foreign asm)
                 => asm         (a -> b)
                 -> DelayedAfun (a -> b)
                 -> DelayedOpenAcc aenv a
                 -> LLVM arch (CompiledOpenAcc arch aenv b)
        foreignA asm f a =
          case foreignAcc (undefined :: arch) asm of
            Just{}  -> plain =<< liftA (AST.Aforeign asm) <$> travA a
            Nothing -> traverseAcc $ Manifest (Apply (weaken absurd f) a)
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

