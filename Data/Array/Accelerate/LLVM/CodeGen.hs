{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--


module Data.Array.Accelerate.LLVM.CodeGen
  where

-- accelerate
import Data.Array.Accelerate.AST                                hiding ( Val(..), prj )
import Data.Array.Accelerate.Array.Sugar                        ( Array )
import Data.Array.Accelerate.Trafo

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.CodeGen.Native.Map

#include "accelerate.h"


-- Code Generation
-- ===============

-- Array computations
-- ------------------

llvmOfAcc :: forall aenv arrs.
             Target
          -> DelayedOpenAcc aenv arrs
          -> Aval aenv
          -> Module aenv arrs
llvmOfAcc _      Delayed{}       _    = INTERNAL_ERROR(error) "llvmOfAcc" "expected manifest array"
llvmOfAcc target (Manifest pacc) aenv = runLLVM target $
  case pacc of
    -- Producers
    Map f a             -> mkMap aenv (travF1 f) (travD a)

    _                   -> error "silence!"
  where
    -- code generation for delayed arrays
    travD :: DelayedOpenAcc aenv (Array sh e) -> IRDelayed aenv (Array sh e)
    travD Manifest{}  = INTERNAL_ERROR(error) "llvmOfAcc" "expected delayed array"
    travD Delayed{..} = IRDelayed (travE extentD) (travF1 indexD) (travF1 linearIndexD)

    -- scalar code generation
    travF1 :: DelayedFun aenv (a -> b) -> IRFun1 aenv (a -> b)
    travF1 f = llvmOfFun1 f aenv

    travE :: DelayedExp aenv t -> IRExp aenv t
    travE e = llvmOfOpenExp e Empty aenv


-- Scalar expressions
-- ------------------

-- | Convert a closed function of one argument into a sequence of LLVM basic
-- blocks.
--
llvmOfFun1 :: DelayedFun aenv (a -> b) -> Aval aenv -> IRFun1 aenv (a -> b)
llvmOfFun1 (Lam (Body f)) aenv xs = llvmOfOpenExp f (Empty `Push` xs) aenv
llvmOfFun1 _              _    _  = error "dooo~ you knoooow~ what it's liiike?"


