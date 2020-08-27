{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Link
-- Copyright   : [2017..2020] The Accelerate Team
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

) where

-- accelerate
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.AST                                    ( PreOpenAfun(..), HasArraysR(..) )

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

instance HasArraysR (ExecOpenAcc arch) where
  {-# INLINEABLE arraysR #-}
  arraysR (ExecAcc r _ _ _) = r
  arraysR (EvalAcc r _)     = r

-- An AST annotated with compiled and linked functions in the target address
-- space, suitable for execution.
--
type ExecOpenAfun arch  = PreOpenAfun (ExecOpenAcc arch)

type ExecAcc arch a     = ExecOpenAcc arch () a
type ExecAfun arch a    = ExecOpenAfun arch () a


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
        Unit tp e               -> return $ Unit tp e
        Alloc repr sh           -> return $ Alloc repr sh
        Alet lhs a b            -> Alet lhs        <$> travA a  <*> travA b
        Apply repr f a          -> Apply repr      <$> travAF f <*> travA a
        Awhile p f a            -> Awhile          <$> travAF p <*> travAF f <*> travA a
        Acond p t e             -> Acond p         <$> travA t  <*> travA e
        Apair a1 a2             -> Apair           <$> travA a1 <*> travA a2
        Anil                    -> return Anil
        Reshape shr s ix        -> Reshape shr s   <$> pure ix
        Aforeign s r f a        -> Aforeign s r f  <$> travA a

    travA (BuildAcc repr' aenv obj pacc) = ExecAcc repr' aenv  <$> linkForTarget obj <*>
      case pacc of
        Map tp a                -> Map tp          <$> travA a
        Generate repr sh        -> return $ Generate repr sh
        Transform repr sh a     -> Transform repr sh <$> travA a
        Backpermute shr sh a    -> Backpermute shr sh <$> travA a
        Fold z a                -> Fold z          <$> travD a
        FoldSeg i z a s         -> FoldSeg i z     <$> travD a <*> travD s
        Scan d z a              -> Scan d z        <$> travD a
        Scan' d a               -> Scan' d         <$> travD a
        Permute d a             -> Permute         <$> travA d <*> travD a
        Stencil1 tp h a         -> Stencil1 tp h   <$> travD a
        Stencil2 tp h a b       -> Stencil2 tp h   <$> travD a <*> travD b

    travAF :: CompiledOpenAfun arch aenv f
           -> LLVM arch (ExecOpenAfun arch aenv f)
    travAF = linkOpenAfun

    travD :: DelayedOpenAcc CompiledOpenAcc  arch aenv a
          -> LLVM arch (DelayedOpenAcc ExecOpenAcc arch aenv a)
    travD (Manifest r a) = Manifest r <$> travA a
    travD (Delayed r sh) = return $ Delayed r sh

