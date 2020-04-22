{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen (

  Skeleton(..), Intrinsic(..), KernelMetadata,
  llvmOfPreOpenAcc,

) where

-- accelerate
import Data.Array.Accelerate.AST                                    hiding ( Val(..), prj )
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo

import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Intrinsic
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Permute
import Data.Array.Accelerate.LLVM.CodeGen.Skeleton
import Data.Array.Accelerate.LLVM.CodeGen.Stencil
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache
import Data.Array.Accelerate.LLVM.Foreign
import Data.Array.Accelerate.LLVM.State

-- standard library
import Prelude                                                      hiding ( map, scanl, scanl1, scanr, scanr1 )


-- | Generate code for a given target architecture.
--
{-# INLINEABLE llvmOfPreOpenAcc #-}
llvmOfPreOpenAcc
    :: forall arch aenv arrs. (Target arch, Skeleton arch, Intrinsic arch, Foreign arch)
    => UID
    -> PreOpenAcc DelayedOpenAcc aenv arrs
    -> Gamma aenv
    -> LLVM arch (Module arch aenv arrs)
llvmOfPreOpenAcc uid pacc aenv = evalCodeGen $
  case pacc of
    -- Producers
    Map tp f (arrayRepr -> repr)               -> map uid aenv repr tp (travF1 f)
    Generate repr _ f                          -> generate uid aenv repr (travF1 f)
    Transform repr2 _ p f (arrayRepr -> repr1) -> transform uid aenv repr1 repr2 (travF1 p) (travF1 f)
    Backpermute shr _ p (arrayRepr -> repr)    -> backpermute uid aenv repr shr (travF1 p)

    -- Consumers
    Fold f z a                                 -> fold uid aenv (reduceRank $ arrayRepr a) (travF2 f) (travE z) (travD a)
    Fold1 f a                                  -> fold1 uid aenv (reduceRank $ arrayRepr a) (travF2 f) (travD a)
    FoldSeg i f z a s                          -> foldSeg uid aenv (arrayRepr a) i (travF2 f) (travE z) (travD a) (travD s)
    Fold1Seg i f a s                           -> fold1Seg uid aenv (arrayRepr a) i (travF2 f) (travD a) (travD s)
    Scanl f z a                                -> scanl uid aenv (arrayRepr a) (travF2 f) (travE z) (travD a)
    Scanl' f z a                               -> scanl' uid aenv (arrayRepr a) (travF2 f) (travE z) (travD a)
    Scanl1 f a                                 -> scanl1 uid aenv (arrayRepr a) (travF2 f) (travD a)
    Scanr f z a                                -> scanr uid aenv (arrayRepr a) (travF2 f) (travE z) (travD a)
    Scanr' f z a                               -> scanr' uid aenv (arrayRepr a) (travF2 f) (travE z) (travD a)
    Scanr1 f a                                 -> scanr1 uid aenv (arrayRepr a) (travF2 f) (travD a)
    Permute f (arrayRepr -> ArrayR shr _) p a  -> permute uid aenv (arrayRepr a) shr (travPF f) (travF1 p) (travD a)
    Stencil s tp f b a                         -> stencil1 uid aenv s tp (travF1 f) (travB (stencilElt s) b) (travD a)
    Stencil2 s1 s2 tp f b1 a1 b2 a2            -> stencil2 uid aenv s1 s2 tp (travF2 f) (travB (stencilElt s1) b1) (travD a1) (travB (stencilElt s2) b2) (travD a2)

    -- Non-computation forms: sadness
    Alet{}      -> unexpectedError
    Avar{}      -> unexpectedError
    Apply{}     -> unexpectedError
    Acond{}     -> unexpectedError
    Awhile{}    -> unexpectedError
    Apair{}     -> unexpectedError
    Anil        -> unexpectedError
    Use{}       -> unexpectedError
    Unit{}      -> unexpectedError
    Aforeign{}  -> unexpectedError
    Reshape{}   -> unexpectedError

    Replicate{} -> fusionError
    Slice{}     -> fusionError
    ZipWith{}   -> fusionError

  where
    -- code generation for delayed arrays
    travD :: DelayedOpenAcc aenv (Array sh e) -> MIRDelayed arch aenv (Array sh e)
    travD Delayed{..} = IRDelayedJust $ IRDelayed reprD (travE extentD) (travF1 indexD) (travF1 linearIndexD)
    travD (Manifest acc) = IRDelayedNothing $ arrayRepr acc

    -- scalar code generation
    travF1 :: DelayedFun aenv (a -> b) -> IRFun1 arch aenv (a -> b)
    travF1 f = llvmOfFun1 f aenv

    travF2 :: DelayedFun aenv (a -> b -> c) -> IRFun2 arch aenv (a -> b -> c)
    travF2 f = llvmOfFun2 f aenv

    travPF :: DelayedFun aenv (e -> e -> e) -> IRPermuteFun arch aenv (e -> e -> e)
    travPF f = llvmOfPermuteFun f aenv

    travE :: DelayedExp aenv t -> IRExp arch aenv t
    travE e = llvmOfOpenExp e Empty aenv

    travB :: TupleType e
          -> PreBoundary DelayedOpenAcc aenv (Array sh e)
          -> IRBoundary arch aenv (Array sh e)
    travB _  Clamp        = IRClamp
    travB _  Mirror       = IRMirror
    travB _  Wrap         = IRWrap
    travB tp (Constant c) = IRConstant $ IR (constant tp c)
    travB _  (Function f) = IRFunction $ travF1 f

    reduceRank :: ArrayR (Array (sh, Int) e) -> ArrayR (Array sh e)
    reduceRank (ArrayR (ShapeRsnoc shr) tp) = ArrayR shr tp

    -- sadness
    fusionError, unexpectedError :: error
    fusionError      = $internalError "llvmOfPreOpenAcc" $ "unexpected fusible material: " ++ showPreAccOp pacc
    unexpectedError  = $internalError "llvmOfPreOpenAcc" $ "unexpected array primitive: "  ++ showPreAccOp pacc

