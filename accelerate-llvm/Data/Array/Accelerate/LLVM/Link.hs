{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Link
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
            -> PreOpenAccSkeleton (ExecOpenAcc arch) aenv a
            -> ExecOpenAcc arch aenv a

  EvalAcc   :: Arrays a
            => PreOpenAccCommand (ExecOpenAcc arch) aenv a
            -> ExecOpenAcc arch aenv a

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
linkOpenAfun (Alam l)  = Alam  <$> linkOpenAfun l
linkOpenAfun (Abody b) = Abody <$> linkOpenAcc b

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
        Use arrs                -> rnfArrs (arrays (undefined::arrs)) arrs `seq` return (Use arrs)
        Unit e                  -> Unit         <$> travE e
        Alet a b                -> Alet         <$> travA a  <*> travA b
        Apply f a               -> Apply        <$> travAF f <*> travA a
        Awhile p f a            -> Awhile       <$> travAF p <*> travAF f <*> travA a
        Acond p t e             -> Acond        <$> travE p  <*> travA t  <*> travA e
        Atuple tup              -> Atuple       <$> travAtup tup
        Aprj ix tup             -> Aprj ix      <$> travA tup
        Reshape s ix            -> Reshape      <$> travE s <*> pure ix
        Aforeign asm a          -> Aforeign asm <$> travA a

    travA (BuildAcc aenv obj pacc) = ExecAcc aenv <$> linkForTarget obj <*>
      case pacc of
        Map sh                  -> Map          <$> travE sh
        Generate sh             -> Generate     <$> travE sh
        Transform sh            -> Transform    <$> travE sh
        Backpermute sh          -> Backpermute  <$> travE sh
        Fold sh                 -> Fold         <$> travE sh
        Fold1 sh                -> Fold1        <$> travE sh
        FoldSeg sa ss           -> FoldSeg      <$> travE sa <*> travE ss
        Fold1Seg sa ss          -> Fold1Seg     <$> travE sa <*> travE ss
        Scanl sh                -> Scanl        <$> travE sh
        Scanl1 sh               -> Scanl1       <$> travE sh
        Scanl' sh               -> Scanl'       <$> travE sh
        Scanr sh                -> Scanr        <$> travE sh
        Scanr1 sh               -> Scanr1       <$> travE sh
        Scanr' sh               -> Scanr'       <$> travE sh
        Permute sh d            -> Permute      <$> travE sh <*> travA d
        Stencil a               -> return (Stencil a)
        Stencil2 a b            -> return (Stencil2 a b)

    travAF :: CompiledOpenAfun arch aenv f
           -> LLVM arch (ExecOpenAfun arch aenv f)
    travAF = linkOpenAfun

    travAtup :: Atuple (CompiledOpenAcc arch aenv) a
             -> LLVM arch (Atuple (ExecOpenAcc arch aenv) a)
    travAtup NilAtup        = return NilAtup
    travAtup (SnocAtup t a) = SnocAtup <$> travAtup t <*> travA a

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
        Foreign asm _ x         -> Foreign asm err    <$> travE x
          where err = $internalError "link" "attempt to use fallback foreign expression"

    travT :: Tuple (CompiledOpenExp arch env aenv) t
          -> LLVM arch (Tuple (ExecOpenExp arch env aenv) t)
    travT NilTup        = return NilTup
    travT (SnocTup t e) = SnocTup <$> travT t <*> travE e

    rnfArrs :: ArraysR a -> a -> ()
    rnfArrs (ArraysRpair ar1 ar2) (a1, a2) = rnfArrs ar1 a1 `seq` rnfArrs ar2 a2
    rnfArrs ArraysRarray arr               = rnf arr
    rnfArrs ArraysRunit ()                 = ()

