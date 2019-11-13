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
import Data.Array.Accelerate.AST                                    hiding ( Val(..), prj, stencil )
import Data.Array.Accelerate.Array.Sugar                            hiding ( Foreign )
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
    Map f _                 -> map uid aenv (travF1 f)
    Generate _ f            -> generate uid aenv (travF1 f)
    Transform _ p f _       -> transform uid aenv (travF1 p) (travF1 f)
    Backpermute _ p _       -> backpermute uid aenv (travF1 p)

    -- Consumers
    Fold f z a              -> fold uid aenv (travF2 f) (travE z) (travD a)
    Fold1 f a               -> fold1 uid aenv (travF2 f) (travD a)
    FoldSeg f z a s         -> foldSeg uid aenv (travF2 f) (travE z) (travD a) (travD s)
    Fold1Seg f a s          -> fold1Seg uid aenv (travF2 f) (travD a) (travD s)
    Scanl f z a             -> scanl uid aenv (travF2 f) (travE z) (travD a)
    Scanl' f z a            -> scanl' uid aenv (travF2 f) (travE z) (travD a)
    Scanl1 f a              -> scanl1 uid aenv (travF2 f) (travD a)
    Scanr f z a             -> scanr uid aenv (travF2 f) (travE z) (travD a)
    Scanr' f z a            -> scanr' uid aenv (travF2 f) (travE z) (travD a)
    Scanr1 f a              -> scanr1 uid aenv (travF2 f) (travD a)
    Permute f _ p a         -> permute uid aenv (travPF f) (travF1 p) (travD a)
    Stencil f b a           -> stencil1 uid aenv (travF1 f) (travB b) (travD a)
    Stencil2 f b1 a1 b2 a2  -> stencil2 uid aenv (travF2 f) (travB b1) (travD a1) (travB b2) (travD a2)

    -- Non-computation forms: sadness
    Alet{}                  -> unexpectedError
    Avar{}                  -> unexpectedError
    Apply{}                 -> unexpectedError
    Acond{}                 -> unexpectedError
    Awhile{}                -> unexpectedError
    Apair{}                 -> unexpectedError
    Anil                    -> unexpectedError
    Use{}                   -> unexpectedError
    Unit{}                  -> unexpectedError
    Aforeign{}              -> unexpectedError
    Reshape{}               -> unexpectedError

    Replicate{}             -> fusionError
    Slice{}                 -> fusionError
    ZipWith{}               -> fusionError

  where
    -- code generation for delayed arrays
    travD :: DelayedOpenAcc aenv (Array sh e) -> MIRDelayed arch aenv (Array sh e)
    travD Delayed{..} = Just $ IRDelayed (travE extentD) (travF1 indexD) (travF1 linearIndexD)
    travD Manifest{}  = Nothing

    -- scalar code generation
    travF1 :: DelayedFun aenv (a -> b) -> IRFun1 arch aenv (a -> b)
    travF1 f = llvmOfFun1 f aenv

    travF2 :: DelayedFun aenv (a -> b -> c) -> IRFun2 arch aenv (a -> b -> c)
    travF2 f = llvmOfFun2 f aenv

    travPF :: DelayedFun aenv (e -> e -> e) -> IRPermuteFun arch aenv (e -> e -> e)
    travPF f = llvmOfPermuteFun f aenv

    travE :: DelayedExp aenv t -> IRExp arch aenv t
    travE e = llvmOfOpenExp e Empty aenv

    travB :: forall sh e.
             PreBoundary DelayedOpenAcc aenv (Array sh e)
          -> IRBoundary arch aenv (Array sh e)
    travB Clamp        = IRClamp
    travB Mirror       = IRMirror
    travB Wrap         = IRWrap
    travB (Constant c) = IRConstant $ IR (constant (eltType @e) c)
    travB (Function f) = IRFunction $ travF1 f

    -- sadness
    fusionError, unexpectedError :: error
    fusionError      = $internalError "llvmOfPreOpenAcc" $ "unexpected fusible material: " ++ showPreAccOp pacc
    unexpectedError  = $internalError "llvmOfPreOpenAcc" $ "unexpected array primitive: "  ++ showPreAccOp pacc

