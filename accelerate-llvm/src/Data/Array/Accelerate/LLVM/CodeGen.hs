{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen
-- Copyright   : [2015..2020] The Accelerate Team
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

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Representation.Stencil
import Data.Array.Accelerate.Trafo.Delayed

import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
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

import Prelude                                                      hiding ( map, scanl, scanl1, scanr, scanr1 )


-- | Generate code for a given target architecture.
--
{-# INLINEABLE llvmOfPreOpenAcc #-}
llvmOfPreOpenAcc
    :: forall arch aenv arrs. (HasCallStack, Target arch, Skeleton arch, Intrinsic arch, Foreign arch)
    => UID
    -> PreOpenAcc DelayedOpenAcc aenv arrs
    -> Gamma aenv
    -> LLVM arch (Module arch aenv arrs)
llvmOfPreOpenAcc uid pacc aenv = evalCodeGen $
  case pacc of
    -- Producers
    Map tp f (arrayR -> repr)               -> map uid aenv repr tp (travF1 f)
    Generate repr _ f                       -> generate uid aenv repr (travF1 f)
    Transform repr2 _ p f (arrayR -> repr1) -> transform uid aenv repr1 repr2 (travF1 p) (travF1 f)
    Backpermute shr _ p (arrayR -> repr)    -> backpermute uid aenv repr shr (travF1 p)

    -- Consumers
    Fold f z a                              -> fold uid aenv (reduceRank $ arrayR a) (travF2 f) (travE <$> z) (travD a)
    FoldSeg i f z a s                       -> foldSeg uid aenv (arrayR a) i (travF2 f) (travE <$> z) (travD a) (travD s)
    Scan d f z a                            -> scan uid aenv (arrayR a) d (travF2 f) (travE <$> z) (travD a)
    Scan' d f z a                           -> scan' uid aenv (arrayR a) d (travF2 f) (travE z) (travD a)
    Permute f (arrayR -> ArrayR shr _) p a  -> permute uid aenv (arrayR a) shr (travPF f) (travF1 p) (travD a)
    Stencil s tp f b a                      -> stencil1 uid aenv s tp (travF1 f) (travB (stencilEltR s) b) (travD a)
    Stencil2 s1 s2 tp f b1 a1 b2 a2         -> stencil2 uid aenv s1 s2 tp (travF2 f) (travB (stencilEltR s1) b1) (travD a1) (travB (stencilEltR s2) b2) (travD a2)

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
    travD (Manifest acc) = IRDelayedNothing $ arrayR acc

    -- scalar code generation
    travF1 :: Fun aenv (a -> b) -> IRFun1 arch aenv (a -> b)
    travF1 f = llvmOfFun1 f aenv

    travF2 :: Fun aenv (a -> b -> c) -> IRFun2 arch aenv (a -> b -> c)
    travF2 f = llvmOfFun2 f aenv

    travPF :: Fun aenv (e -> e -> e) -> IRPermuteFun arch aenv (e -> e -> e)
    travPF f = llvmOfPermuteFun f aenv

    travE :: Exp aenv t -> IRExp arch aenv t
    travE e = llvmOfOpenExp e Empty aenv

    travB :: TypeR e
          -> Boundary aenv (Array sh e)
          -> IRBoundary arch aenv (Array sh e)
    travB _  Clamp        = IRClamp
    travB _  Mirror       = IRMirror
    travB _  Wrap         = IRWrap
    travB tp (Constant c) = IRConstant $ constant tp c
    travB _  (Function f) = IRFunction $ travF1 f

    -- sadness
    fusionError, unexpectedError :: error
    fusionError      = internalError $ "unexpected fusible material: " ++ showPreAccOp pacc
    unexpectedError  = internalError $ "unexpected array primitive: "  ++ showPreAccOp pacc

