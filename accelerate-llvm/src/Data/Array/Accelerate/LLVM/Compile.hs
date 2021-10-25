{-# LANGUAGE CPP                 #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Compile
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Compile (

  Compile(..),
  compileAcc, compileAfun,

  CompiledOpenAcc(..), CompiledOpenAfun,
  CompiledAcc, CompiledAfun,

) where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Stencil
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Trafo.Delayed
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Sugar.Foreign                as A

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.Foreign
import Data.Array.Accelerate.LLVM.State
import qualified Data.Array.Accelerate.LLVM.AST                     as AST

import Data.IntMap                                                  ( IntMap )
import Formatting                                                   ( (%) )
import Control.Applicative                                          hiding ( Const )
import Prelude                                                      hiding ( map, unzip, zipWith, scanl, scanl1, scanr, scanr1, exp )
import qualified Data.IntMap                                        as IntMap


class Foreign arch => Compile arch where
  data ObjectR arch

  -- | Compile an accelerate computation into some backend-specific code that
  -- will be used to execute the given array expression. The code is not yet
  -- linked into the running executable.
  --
  compileForTarget
      :: PreOpenAcc DelayedOpenAcc aenv a
      -> Gamma aenv
      -> LLVM arch (ObjectR arch)


data CompiledOpenAcc arch aenv a where
  BuildAcc  :: ArraysR a
            -> Gamma aenv
            -> ObjectR arch
            -> AST.PreOpenAccSkeleton CompiledOpenAcc arch aenv a
            -> CompiledOpenAcc arch aenv a

  PlainAcc  :: ArraysR a
            -> AST.PreOpenAccCommand  CompiledOpenAcc arch aenv a
            -> CompiledOpenAcc arch aenv a


-- An annotated AST with embedded build products
--
type CompiledOpenAfun arch  = PreOpenAfun (CompiledOpenAcc arch)

type CompiledAcc arch a     = CompiledOpenAcc arch () a
type CompiledAfun arch a    = CompiledOpenAfun arch () a


-- | Generate and compile code for an array expression. The returned expression
-- is annotated with the compilation products required to executed each
-- operation on the given target, together with the list of array variables
-- referenced from the embedded scalar expressions.
--
{-# INLINEABLE compileAcc #-}
compileAcc
    :: (HasCallStack, Compile arch)
    => DelayedAcc a
    -> LLVM arch (CompiledAcc arch a)
compileAcc = compileOpenAcc

{-# INLINEABLE compileAfun #-}
compileAfun
    :: (HasCallStack, Compile arch)
    => DelayedAfun f
    -> LLVM arch (CompiledAfun arch f)
compileAfun = compileOpenAfun


{-# INLINEABLE compileOpenAfun #-}
compileOpenAfun
    :: (HasCallStack, Compile arch)
    => DelayedOpenAfun aenv f
    -> LLVM arch (CompiledOpenAfun arch aenv f)
compileOpenAfun (Alam lhs l) = Alam lhs <$> compileOpenAfun l
compileOpenAfun (Abody b)    = Abody    <$> compileOpenAcc b


{-# INLINEABLE compileOpenAcc #-}
compileOpenAcc
    :: forall arch _aenv _a. (HasCallStack, Compile arch)
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
    -- TODO: Let's not throw away all annotations.
    --
    traverseAcc
        :: forall aenv arrs. HasCallStack
        => DelayedOpenAcc aenv arrs
        -> LLVM arch (CompiledOpenAcc arch aenv arrs)
    traverseAcc Delayed{}       = internalError "unexpected delayed array"
    traverseAcc (Manifest pacc) =
      case pacc of
        -- Environment and control flow
        Avar ix                       -> plain $ pure (AST.Avar ix)
        Alet _ lhs a b                -> plain . pure =<< AST.Alet lhs  <$> traverseAcc a <*> traverseAcc b
        Apply _ r f a                 -> plain =<< liftA2 (AST.Apply r) <$> travAF f <*> travA a
        Awhile _ p f a                -> plain =<< liftA3 AST.Awhile    <$> travAF p <*> travAF f <*> travA a
        Acond _ p t e                 -> plain =<< liftA3 AST.Acond     <$> travE  p <*> travA  t <*> travA e
        Apair _ a1 a2                 -> plain =<< liftA2 AST.Apair     <$> travA a1 <*> travA a2
        Anil _                        -> plain $ pure AST.Anil
        Atrace _ msg a1 a2            -> plain =<< liftA2 (AST.Atrace msg) <$> travA a1 <*> travA a2

        -- Foreign arrays operations
        Aforeign ann repr ff afun a   -> foreignA ann repr ff afun a

        -- Uninitialised array allocation
        Generate _ r sh f
          | alloc f                   -> plain =<< liftA (AST.Alloc r)  <$> travE sh

        -- Array injection & manipulation
        Reshape _ shr sh a            -> plain =<< liftA2 (AST.Reshape shr) <$> travE sh <*> travM a
        Unit _ tp e                   -> plain =<< liftA (AST.Unit tp)  <$> travE e
        Use _ repr arrs               -> plain $ pure (AST.Use repr arrs)
        Map _ _ f a
          | Just (t,x) <- unzip f a -> plain $ pure (AST.Unzip t x)

        -- Skeleton operations resulting in compiled code
        -- Producers
        Map _ tp f a                  -> build =<< liftA2 (map tp)      <$> travF f  <*> travA a
        Generate _ r sh f             -> build =<< liftA2 (generate r)  <$> travE sh <*> travF f
        Transform _ r sh p f a        -> build =<< liftA4 (transform r) <$> travE sh <*> travF p <*> travF f <*> travA a
        Backpermute _ shr sh f a      -> build =<< liftA3 (backpermute shr) <$> travE sh <*> travF f <*> travA a

        -- Consumers
        Fold _ f z a                  -> build =<< liftA3 fold          <$> travF f <*> travME z <*> travD a
        FoldSeg _ i f z a s           -> build =<< liftA4 (foldSeg i)   <$> travF f <*> travME z <*> travD a <*> travD s
        Scan  _ d f z a               -> build =<< liftA3 (scan  d)     <$> travF f <*> travME z <*> travD a
        Scan' _ d f z a               -> build =<< liftA3 (scan' d)     <$> travF f <*> travE z <*> travD a
        Permute _ f d g a             -> build =<< liftA4 permute       <$> travF f <*> travA d <*> travF g <*> travD a
        Stencil _ s tp f x a          -> build =<< liftA3 (stencil1 s tp) <$> travF f <*> travB x <*> travD a
        Stencil2 _ s1 s2 tp f x a y b -> build =<< liftA5 (stencil2 s1 s2 tp) <$> travF f <*> travB x <*> travD a <*> travB y <*> travD b

        -- Removed by fusion
        Replicate{}                   -> fusionError
        Slice{}                       -> fusionError
        ZipWith{}                     -> fusionError

      where
        map tp _ a             = AST.Map tp a
        generate r sh _        = AST.Generate r sh
        transform r sh _ _ a   = AST.Transform r sh a
        backpermute shr sh _ a = AST.Backpermute shr sh a
        fold _ z a             = AST.Fold z a
        foldSeg i _ z a s      = AST.FoldSeg i z a s
        scan d _ z a           = AST.Scan d z a
        scan' d _ _ a          = AST.Scan' d a
        permute _ d _ a        = AST.Permute d a

        stencil1 :: StencilR sh a stencil
                 -> TypeR b
                 -> Fun      aenv (stencil -> b)
                 -> Boundary aenv (Array sh a)
                 -> AST.DelayedOpenAcc     CompiledOpenAcc arch aenv (Array sh a)
                 -> AST.PreOpenAccSkeleton CompiledOpenAcc arch aenv (Array sh b)
        stencil1 s tp _ _ a = AST.Stencil1 tp (snd $ stencilHalo s) a

        stencil2 :: StencilR sh a stencil1
                 -> StencilR sh b stencil2
                 -> TypeR c
                 -> Fun                                          aenv (stencil1 -> stencil2 -> c)
                 -> Boundary                                     aenv (Array sh a)
                 -> AST.DelayedOpenAcc     CompiledOpenAcc arch  aenv (Array sh a)
                 -> Boundary                                     aenv (Array sh b)
                 -> AST.DelayedOpenAcc     CompiledOpenAcc arch  aenv (Array sh b)
                 -> AST.PreOpenAccSkeleton CompiledOpenAcc arch  aenv (Array sh c)
        stencil2 s1 s2 tp _ _ a _ b = AST.Stencil2 tp (union shr h1 h2) a b
          where
            (shr, h1) = stencilHalo s1
            (_,   h2) = stencilHalo s2

        fusionError :: error
        fusionError = internalError ("unexpected fusible material: " % formatPreAccOp) pacc

        travA :: HasCallStack
              => DelayedOpenAcc aenv a
              -> LLVM arch (IntMap (Idx' aenv), CompiledOpenAcc arch aenv a)
        travA acc = pure <$> traverseAcc acc

        travD :: HasCallStack
              => DelayedOpenAcc aenv (Array sh e)
              -> LLVM arch ( IntMap (Idx' aenv)
                           , AST.DelayedOpenAcc CompiledOpenAcc arch aenv (Array sh e))
        travD acc =
          case acc of
            Delayed{..} -> liftA2 (const . (AST.Delayed reprD)) <$> travE extentD <*> travF indexD
            _           -> liftA  (AST.Manifest $ arraysR acc)  <$> travA acc

        travM :: HasCallStack
              => DelayedOpenAcc aenv (Array sh e)
              -> LLVM arch (IntMap (Idx' aenv), ArrayVar aenv (Array sh e))
        travM (Manifest (Avar v)) = return (freevar v, v)
        travM _                   = internalError "expected array variable"

        travME :: Maybe (OpenExp env aenv e) -> LLVM arch (IntMap (Idx' aenv), Bool)
        travME Nothing  = return (IntMap.empty, False)
        travME (Just e) = (True <$) <$> travE e

        travAF :: HasCallStack
               => DelayedOpenAfun aenv f
               -> LLVM arch (IntMap (Idx' aenv), CompiledOpenAfun arch aenv f)
        travAF afun = pure <$> compileOpenAfun afun

        travF :: HasCallStack
              => OpenFun env aenv t
              -> LLVM arch (IntMap (Idx' aenv), OpenFun env aenv t)
        travF (Body b)    = liftA Body <$> travE b
        travF (Lam lhs f) = liftA (Lam lhs) <$> travF f

        travB :: HasCallStack
              => Boundary aenv t
              -> LLVM arch (IntMap (Idx' aenv), Boundary aenv t)
        travB Clamp        = return $ pure Clamp
        travB Mirror       = return $ pure Mirror
        travB Wrap         = return $ pure Wrap
        travB (Constant c) = return $ pure (Constant c)
        travB (Function f) = liftA Function <$> travF f

        build :: (IntMap (Idx' aenv), AST.PreOpenAccSkeleton CompiledOpenAcc arch aenv arrs)
              -> LLVM arch (CompiledOpenAcc arch aenv arrs)
        build (aenv, eacc) = do
          let aval = makeGamma aenv
          kernel <- compileForTarget pacc aval
          return $! BuildAcc (arraysR eacc) aval kernel eacc

        plain :: (IntMap (Idx' aenv'), AST.PreOpenAccCommand CompiledOpenAcc arch aenv' arrs')
              -> LLVM arch (CompiledOpenAcc arch aenv' arrs')
        plain (_, eacc) = return (PlainAcc (arraysR eacc) eacc)

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
        alloc :: Fun aenv (sh -> e)
              -> Bool
        alloc f
          | Lam _ (Body (Undef _ _)) <- f = True
          | otherwise                     = False

        -- Unzips of manifest array data can be done in constant time without
        -- executing any array programs. We split them out here into a separate
        -- case so that the execution phase does not have to continually perform
        -- the below check.
        --
        unzip :: forall sh a b.
                 Fun aenv (a -> b)
              -> DelayedOpenAcc aenv (Array sh a)
              -> Maybe (AST.UnzipIdx a b, ArrayVar aenv (Array sh a))
        unzip f a
          | Lam lhs (Body b) <- f
          , Just vars <- extractExpVars b
          , Delayed _ _ sh index _              <- a
          , Shape _ u                           <- sh
          , Just v                              <- isIdentityIndexing index
          , Just Refl                           <- matchVar u v
          = Just (unzipIdx lhs vars, u)
        unzip _ _
          = Nothing

        unzipIdx :: forall env a b. ELeftHandSide a () env -> Vars ScalarType env b -> AST.UnzipIdx a b
        unzipIdx lhs = go
          where
            go :: Vars ScalarType env y -> AST.UnzipIdx a y
            go TupRunit                  = AST.UnzipUnit
            go (TupRpair v1 v2)          = AST.UnzipPair (go v1) (go v2)
            go (TupRsingle (Var _ _ ix)) = case lookupVar lhs ix of
              Right u -> u
              Left _  -> internalError "Left branch is unreachable as `Idx () y` is an empty type"

            lookupVar :: ELeftHandSide x env1 env2 -> Idx env2 y -> Either (Idx env1 y) (AST.UnzipIdx x y)
            lookupVar (LeftHandSideWildcard _) ix = Left ix
            lookupVar (LeftHandSideSingle _ _) ix = case ix of
              ZeroIdx     -> Right AST.UnzipId
              SuccIdx ix' -> Left ix'
            lookupVar (LeftHandSidePair l1 l2) ix = case lookupVar l2 ix of
              Right u -> Right $ AST.UnzipPrj PairIdxRight u
              Left ix' -> case lookupVar l1 ix' of
                Right u   -> Right $ AST.UnzipPrj PairIdxLeft u
                Left ix'' -> Left ix''

        -- Is there a foreign version available for this backend? If so, take
        -- the foreign function and drop the remaining terms. Otherwise, drop
        -- this term and continue walking down the list of alternate
        -- implementations.
        --
        foreignA :: (HasCallStack, A.Foreign asm)
                 => Ann
                 -> ArraysR b
                 -> asm         (a -> b)
                 -> DelayedAfun (a -> b)
                 -> DelayedOpenAcc aenv a
                 -> LLVM arch (CompiledOpenAcc arch aenv b)
        foreignA ann repr ff f a =
          case foreignAcc ff of
            Just asm -> plain =<< liftA (AST.Aforeign repr (A.strForeign ff) asm) <$> travA a
            Nothing  -> traverseAcc $ Manifest (Apply ann repr (weaken weakenEmpty f) a)

    -- Traverse a scalar expression
    --
    travE :: HasCallStack
          => OpenExp env aenv e
          -> LLVM arch (IntMap (Idx' aenv), OpenExp env aenv e)
    travE exp =
      case exp of
        Evar v                  -> return $ pure $ Evar v
        Const ann tp c          -> return $ pure $ Const ann tp c
        PrimConst ann c         -> return $ pure $ PrimConst ann c
        Undef ann tp            -> return $ pure $ Undef ann tp
        Foreign ann tp ff f x   -> foreignE ann tp ff f x
        --
        Let ann lhs a b         -> liftA2 (Let ann lhs)         <$> travE a  <*> travE b
        IndexSlice ann slix x s -> liftA2 (IndexSlice ann slix) <$> travE x  <*> travE s
        IndexFull ann slix x s  -> liftA2 (IndexFull ann slix)  <$> travE x  <*> travE s
        ToIndex ann shr s i     -> liftA2 (ToIndex   ann shr)   <$> travE s  <*> travE i
        FromIndex ann shr s i   -> liftA2 (FromIndex ann shr)   <$> travE s  <*> travE i
        Nil ann                 -> return . pure $ Nil ann
        Pair ann e1 e2          -> liftA2 (Pair ann)            <$> travE e1 <*> travE e2
        VecPack   ann vecr e    -> liftA  (VecPack   ann vecr)  <$> travE e
        VecUnpack ann vecr e    -> liftA  (VecUnpack ann vecr)  <$> travE e
        Case ann t xs x         -> liftA3 (Case ann)            <$> travE t  <*> travLE xs <*> travME x
        Cond ann p t e          -> liftA3 (Cond ann)            <$> travE p  <*> travE t   <*> travE e
        While ann p f x         -> liftA3 (While ann)           <$> travF p  <*> travF f   <*> travE x
        PrimApp ann f e         -> liftA  (PrimApp ann f)       <$> travE e
        Index ann a e           -> liftA2 (Index ann)           <$> travA a  <*> travE e
        LinearIndex ann a e     -> liftA2 (LinearIndex ann)     <$> travA a  <*> travE e
        Shape ann a             -> liftA  (Shape ann)           <$> travA a
        ShapeSize ann shr e     -> liftA  (ShapeSize ann shr)   <$> travE e
        Coerce ann t1 t2 x      -> liftA  (Coerce ann t1 t2)    <$> travE x

      where
        travA :: ArrayVar aenv (Array sh e)
              -> LLVM arch (IntMap (Idx' aenv), ArrayVar aenv (Array sh e))
        travA var = return (freevar var, var)

        travF :: HasCallStack
              => OpenFun env aenv t
              -> LLVM arch (IntMap (Idx' aenv), OpenFun env aenv t)
        travF (Body b)    = liftA Body      <$> travE b
        travF (Lam lhs f) = liftA (Lam lhs) <$> travF f

        travLE :: HasCallStack
               => [(TAG, OpenExp env aenv t)]
               -> LLVM arch (IntMap (Idx' aenv), [(TAG, OpenExp env aenv t)])
        travLE []     = return $ pure []
        travLE ((t,x):xs) = do
          (v,  y)  <- travE x
          (vs, ys) <- travLE xs
          return (IntMap.union v vs, (t,y):ys)

        travME :: HasCallStack
               => Maybe (OpenExp env aenv t)
               -> LLVM arch (IntMap (Idx' aenv), Maybe (OpenExp env aenv t))
        travME Nothing  = return $ pure Nothing
        travME (Just e) = fmap Just <$> travE e

        foreignE :: (HasCallStack, A.Foreign asm)
                 => Ann
                 -> TypeR b
                 -> asm           (a -> b)
                 -> Fun () (a -> b)
                 -> OpenExp env aenv a
                 -> LLVM arch (IntMap (Idx' aenv), OpenExp env aenv b)
        foreignE ann tp asm f x =
          case foreignExp @arch asm of
            Just{}                            -> liftA (Foreign ann tp asm err) <$> travE x
            Nothing
              | Lam lhs (Body b) <- f
              , Exists lhs' <- rebuildLHS lhs -> liftA2 (Let ann lhs')          <$> travE x <*> travE (weaken weakenEmpty $ weakenE (sinkWithLHS lhs lhs' weakenEmpty) b)
            _                                 -> error "the slow regard of silent things"
          where
            err :: Fun () (a -> b)
            err = internalError "attempt to use fallback in foreign expression"


-- Applicative
-- -----------
--
liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d g = f <$> a <*> b <*> c <*> d <*> g

instance HasArraysR (CompiledOpenAcc arch) where
  {-# INLINEABLE arraysR #-}
  arraysR (BuildAcc r _ _ _) = r
  arraysR (PlainAcc r     _) = r

