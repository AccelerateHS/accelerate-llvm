{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Link
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Link (

  Link(..),
  linkAcc, linkAfun,

  ExecOpenAcc(..), ExecOpenAfun,
  ExecAcc, ExecAfun,
  ExecExp, ExecOpenExp,
  ExecFun, ExecOpenFun

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                            hiding ( Foreign )
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State

import Control.Applicative                                          hiding ( Const )
import Control.DeepSeq
import Prelude                                                      hiding ( exp )


class Link arch where
  data ExecutableR arch

  -- | Link a target-specific object file into the current context to produce an
  -- executable functions.
  --
  linkForTarget
      :: ObjectR arch
      -> LLVM arch (ExecutableR arch)


-- | Annotate an open array expression with executable code loaded into the
-- target address space, suitable for execution.
--
data ExecOpenAcc arch aenv a where
  ExecAcc   :: Gamma aenv
            -> ExecutableR arch
            -> PreOpenAccSkeleton ExecOpenAcc arch aenv a
            -> ExecOpenAcc arch aenv a

  EvalAcc   :: PreOpenAccCommand  ExecOpenAcc arch aenv a
            -> ExecOpenAcc arch aenv a

instance HasArraysRepr (ExecOpenAcc arch) where
  arraysRepr (ExecAcc _ _ a) = arraysRepr a
  arraysRepr (EvalAcc a)     = arraysRepr a

-- An AST annotated with compiled and linked functions in the target address
-- space, suitable for execution.
--
type ExecOpenAfun arch  = PreOpenAfun (ExecOpenAcc arch)
type ExecOpenExp arch   = PreOpenExp (ExecOpenAcc arch)
type ExecOpenFun arch   = PreOpenFun (ExecOpenAcc arch)

type ExecAcc arch a     = ExecOpenAcc arch () a
type ExecAfun arch a    = ExecOpenAfun arch () a

type ExecExp arch       = ExecOpenExp arch ()
type ExecFun arch       = ExecOpenFun arch ()


-- | Link the compiled code for an array expression into the target address
-- space. Additionally, copy input array data into the target address space.
--
{-# INLINEABLE linkAcc #-}
linkAcc
    :: Link arch
    => CompiledAcc arch a
    -> LLVM arch (ExecAcc arch a)
linkAcc = linkOpenAcc

{-# INLINEABLE linkAfun #-}
linkAfun
    :: Link arch
    => CompiledAfun arch f
    -> LLVM arch (ExecAfun arch f)
linkAfun = linkOpenAfun


{-# INLINEABLE linkOpenAfun #-}
linkOpenAfun
    :: Link arch
    => CompiledOpenAfun arch aenv f
    -> LLVM arch (ExecOpenAfun arch aenv f)
linkOpenAfun (Alam lhs l) = Alam lhs <$> linkOpenAfun l
linkOpenAfun (Abody b)    = Abody    <$> linkOpenAcc b

{-# INLINEABLE linkOpenAcc #-}
linkOpenAcc
    :: forall arch _aenv _a. Link arch
    => CompiledOpenAcc arch _aenv _a
    -> LLVM arch (ExecOpenAcc arch _aenv _a)
linkOpenAcc = travA
  where
    travA :: forall aenv arrs. CompiledOpenAcc arch aenv arrs -> LLVM arch (ExecOpenAcc arch aenv arrs)
    travA (PlainAcc pacc) = EvalAcc <$>
      case pacc of
        Unzip tix ix            -> return (Unzip tix ix)
        Avar ix                 -> return (Avar ix)
        Use arr                 -> rnf arr `seq` return (Use arr)
        Unit e                  -> Unit         <$> travE e
        Alloc sh                -> Alloc        <$> travE sh
        Alet lhs a b            -> Alet lhs     <$> travA a  <*> travA b
        Apply f a               -> Apply        <$> travAF f <*> travA a
        Awhile p f a            -> Awhile       <$> travAF p <*> travAF f <*> travA a
        Acond p t e             -> Acond        <$> travE p  <*> travA t  <*> travA e
        Apair a1 a2             -> Apair        <$> travA a1 <*> travA a2
        Anil                    -> return Anil
        Reshape s ix            -> Reshape      <$> travE s <*> pure ix
        Aforeign s r f a        -> Aforeign s r f <$> travA a

    travA (BuildAcc aenv obj pacc) = ExecAcc aenv <$> linkForTarget obj <*>
      case pacc of
        Map a                   -> Map          <$> travA a
        Generate sh             -> Generate     <$> travE sh
        Transform sh a          -> Transform    <$> travE sh <*> travA a
        Backpermute sh a        -> Backpermute  <$> travE sh <*> travA a
        Fold a                  -> Fold         <$> travD a
        Fold1 a                 -> Fold1        <$> travD a
        FoldSeg a s             -> FoldSeg      <$> travD a <*> travD s
        Fold1Seg a s            -> Fold1Seg     <$> travD a <*> travD s
        Scanl a                 -> Scanl        <$> travD a
        Scanl1 a                -> Scanl1       <$> travD a
        Scanl' a                -> Scanl'       <$> travD a
        Scanr a                 -> Scanr        <$> travD a
        Scanr1 a                -> Scanr1       <$> travD a
        Scanr' a                -> Scanr'       <$> travD a
        Permute d a             -> Permute      <$> travA d <*> travD a
        Stencil1 h a            -> Stencil1 h   <$> travD a
        Stencil2 h a b          -> Stencil2 h   <$> travD a <*> travD b

    travAF :: CompiledOpenAfun arch aenv f
           -> LLVM arch (ExecOpenAfun arch aenv f)
    travAF = linkOpenAfun

    travD :: DelayedOpenAcc CompiledOpenAcc  arch aenv a
          -> LLVM arch (DelayedOpenAcc ExecOpenAcc arch aenv a)
    travD (Manifest a) = Manifest <$> travA a
    travD (Delayed sh) = Delayed  <$> travE sh

    travF :: CompiledOpenFun arch env aenv t
          -> LLVM arch (ExecOpenFun arch env aenv t)
    travF (Body b) = Body <$> travE b
    travF (Lam  f) = Lam  <$> travF f

    travE :: CompiledOpenExp arch env aenv t
          -> LLVM arch (ExecOpenExp arch env aenv t)
    travE exp =
      case exp of
        Var ix                  -> return (Var ix)
        Const c                 -> return (Const c)
        PrimConst c             -> return (PrimConst c)
        Undef                   -> return Undef
        IndexAny                -> return IndexAny
        IndexNil                -> return IndexNil
        Let a b                 -> Let                <$> travE a <*> travE b
        IndexCons t h           -> IndexCons          <$> travE t <*> travE h
        IndexHead h             -> IndexHead          <$> travE h
        IndexTail t             -> IndexTail          <$> travE t
        IndexSlice slix x s     -> (IndexSlice slix)  <$> travE x <*> travE s
        IndexFull slix x s      -> (IndexFull slix)   <$> travE x <*> travE s
        ToIndex s i             -> ToIndex            <$> travE s <*> travE i
        FromIndex s i           -> FromIndex          <$> travE s <*> travE i
        Tuple t                 -> Tuple              <$> travT t
        Prj ix e                -> (Prj ix)           <$> travE e
        Cond p t e              -> Cond               <$> travE p <*> travE t <*> travE e
        While p f x             -> While              <$> travF p <*> travF f <*> travE x
        PrimApp f e             -> (PrimApp f)        <$> travE e
        Index a e               -> Index              <$> travA a <*> travE e
        LinearIndex a e         -> LinearIndex        <$> travA a <*> travE e
        Shape a                 -> Shape              <$> travA a
        ShapeSize e             -> ShapeSize          <$> travE e
        Intersect x y           -> Intersect          <$> travE x <*> travE y
        Union x y               -> Union              <$> travE x <*> travE y
        Coerce x                -> Coerce             <$> travE x
        Foreign asm _ x         -> Foreign asm err    <$> travE x
          where err = $internalError "link" "attempt to use fallback foreign expression"

    travT :: Tuple (CompiledOpenExp arch env aenv) t
          -> LLVM arch (Tuple (ExecOpenExp arch env aenv) t)
    travT NilTup        = return NilTup
    travT (SnocTup t e) = SnocTup <$> travT t <*> travE e

