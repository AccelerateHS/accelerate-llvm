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
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State

import Control.Applicative                                          hiding ( Const )
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
  ExecAcc   :: ArraysR a
            -> Gamma aenv
            -> ExecutableR arch
            -> PreOpenAccSkeleton ExecOpenAcc arch aenv a
            -> ExecOpenAcc arch aenv a

  EvalAcc   :: ArraysR a
            -> PreOpenAccCommand  ExecOpenAcc arch aenv a
            -> ExecOpenAcc arch aenv a

instance HasArraysRepr (ExecOpenAcc arch) where
  arraysRepr (ExecAcc r _ _ _) = r
  arraysRepr (EvalAcc r _)     = r

-- An AST annotated with compiled and linked functions in the target address
-- space, suitable for execution.
--
type ExecOpenAfun arch  = PreOpenAfun (ExecOpenAcc arch)
type ExecOpenExp arch   = PreOpenExp ArrayVar
type ExecOpenFun arch   = PreOpenFun ArrayVar

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
    travA (PlainAcc repr' pacc) = EvalAcc repr' <$>
      case pacc of
        Unzip tix ix            -> return (Unzip tix ix)
        Avar ix                 -> return (Avar ix)
        Use repr arr            -> rnfArray repr arr `seq` return (Use repr arr)
        Unit tp e               -> Unit tp         <$> travE e
        Alloc repr sh           -> Alloc repr      <$> travE sh
        Alet lhs a b            -> Alet lhs        <$> travA a  <*> travA b
        Apply repr f a          -> Apply repr      <$> travAF f <*> travA a
        Awhile p f a            -> Awhile          <$> travAF p <*> travAF f <*> travA a
        Acond p t e             -> Acond           <$> travE p  <*> travA t  <*> travA e
        Apair a1 a2             -> Apair           <$> travA a1 <*> travA a2
        Anil                    -> return Anil
        Reshape shr s ix        -> Reshape shr     <$> travE s <*> pure ix
        Aforeign s r f a        -> Aforeign s r f  <$> travA a

    travA (BuildAcc repr' aenv obj pacc) = ExecAcc repr' aenv  <$> linkForTarget obj <*>
      case pacc of
        Map tp a                -> Map tp          <$> travA a
        Generate repr sh        -> Generate repr   <$> travE sh
        Transform repr sh a     -> Transform repr  <$> travE sh <*> travA a
        Backpermute shr sh a    -> Backpermute shr <$> travE sh <*> travA a
        Fold a                  -> Fold            <$> travD a
        Fold1 a                 -> Fold1           <$> travD a
        FoldSeg i a s           -> FoldSeg i       <$> travD a <*> travD s
        Fold1Seg i a s          -> Fold1Seg i      <$> travD a <*> travD s
        Scanl a                 -> Scanl           <$> travD a
        Scanl1 a                -> Scanl1          <$> travD a
        Scanl' a                -> Scanl'          <$> travD a
        Scanr a                 -> Scanr           <$> travD a
        Scanr1 a                -> Scanr1          <$> travD a
        Scanr' a                -> Scanr'          <$> travD a
        Permute d a             -> Permute         <$> travA d <*> travD a
        Stencil1 tp h a         -> Stencil1 tp h   <$> travD a
        Stencil2 tp h a b       -> Stencil2 tp h   <$> travD a <*> travD b

    travAF :: CompiledOpenAfun arch aenv f
           -> LLVM arch (ExecOpenAfun arch aenv f)
    travAF = linkOpenAfun

    travD :: DelayedOpenAcc CompiledOpenAcc  arch aenv a
          -> LLVM arch (DelayedOpenAcc ExecOpenAcc arch aenv a)
    travD (Manifest r a) = Manifest r <$> travA a
    travD (Delayed r sh) = Delayed  r <$> travE sh

    travF :: CompiledOpenFun arch env aenv t
          -> LLVM arch (ExecOpenFun arch env aenv t)
    travF (Body b)    = Body <$> travE b
    travF (Lam lhs f) = Lam lhs <$> travF f

    travE :: CompiledOpenExp arch env aenv t
          -> LLVM arch (ExecOpenExp arch env aenv t)
    travE exp =
      case exp of
        Evar ix                 -> return (Evar ix)
        Const tp c              -> return (Const tp c)
        PrimConst c             -> return (PrimConst c)
        Undef tp                -> return (Undef tp)
        Let lhs a b             -> Let lhs            <$> travE a <*> travE b
        IndexSlice slix x s     -> (IndexSlice slix)  <$> travE x <*> travE s
        IndexFull slix x s      -> (IndexFull slix)   <$> travE x <*> travE s
        ToIndex shr s i         -> ToIndex shr        <$> travE s <*> travE i
        FromIndex shr s i       -> FromIndex shr      <$> travE s <*> travE i
        Nil                     -> return Nil
        Pair e1 e2              -> Pair               <$> travE e1 <*> travE e2
        VecPack   vecr e        -> VecPack   vecr     <$> travE e
        VecUnpack vecr e        -> VecUnpack vecr     <$> travE e
        Cond p t e              -> Cond               <$> travE p <*> travE t <*> travE e
        While p f x             -> While              <$> travF p <*> travF f <*> travE x
        PrimApp f e             -> PrimApp f          <$> travE e
        Index var e             -> Index var          <$> travE e
        LinearIndex var e       -> LinearIndex var    <$> travE e
        Shape var               -> return $ Shape var
        ShapeSize shr e         -> ShapeSize shr      <$> travE e
        Coerce t1 t2 x          -> Coerce t1 t2       <$> travE x
        Foreign asm _ x         -> Foreign asm err    <$> travE x
          where err = $internalError "link" "attempt to use fallback foreign expression"

