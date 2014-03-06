{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen (

  Skeleton(..), llvmOfAcc,

) where

-- accelerate
import Data.Array.Accelerate.AST                                hiding ( Val(..), prj )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Trafo

import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad

-- standard library
import Prelude                                                  hiding ( map )

#include "accelerate.h"


-- | A class covering code generation for all of the primitive operations.
-- Client backends implement an instance of this class.
--
class Skeleton arch where
  map           :: (Shape sh, Elt a, Elt b)
                => arch
                -> Gamma aenv
                -> IRFun1    aenv (a -> b)
                -> IRDelayed aenv (Array sh a)
                -> CodeGen [Kernel arch aenv (Array sh b)]

  generate      :: (Shape sh, Elt e)
                => arch
                -> Gamma aenv
                -> IRFun1 aenv (sh -> e)
                -> CodeGen [Kernel arch aenv (Array sh e)]

  transform     :: (Shape sh, Shape sh', Elt a, Elt b)
                => arch
                -> Gamma aenv
                -> IRFun1    aenv (sh' -> sh)
                -> IRFun1    aenv (a -> b)
                -> IRDelayed aenv (Array sh a)
                -> CodeGen [Kernel arch aenv (Array sh' b)]

  fold          :: (Shape sh, Elt e)
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRExp     aenv e
                -> IRDelayed aenv (Array (sh:.Int) e)
                -> CodeGen [Kernel arch aenv (Array sh e)]

  fold1         :: (Shape sh, Elt e)
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRDelayed aenv (Array (sh:.Int) e)
                -> CodeGen [Kernel arch aenv (Array sh e)]


-- | Generate code for a given target architecture.
--
llvmOfAcc :: forall arch aenv arrs. (Target arch, Skeleton arch)
          => arch
          -> DelayedOpenAcc aenv arrs
          -> Gamma aenv
          -> Module arch aenv arrs
llvmOfAcc _    Delayed{}       _    = INTERNAL_ERROR(error) "llvmOfAcc" "expected manifest array"
llvmOfAcc arch (Manifest pacc) aenv = runLLVM $
  case pacc of
    -- Producers
    Map f a             -> map arch aenv (travF1 f) (travD a)
    Generate _ f        -> generate arch aenv (travF1 f)
    Transform _ p f a   -> transform arch aenv (travF1 p) (travF1 f) (travD a)
    Backpermute _ p a   -> transform arch aenv (travF1 p) (return)   (travD a)

    -- Consumers
    Fold f z a          -> fold arch aenv (travF2 f) (travE z) (travD a)
    Fold1 f a           -> fold1 arch aenv (travF2 f) (travD a)

    -- Non-computation forms: sadness
    Alet{}              -> unexpectedError pacc
    Avar{}              -> unexpectedError pacc
    Apply{}             -> unexpectedError pacc
    Acond{}             -> unexpectedError pacc
    Awhile{}            -> unexpectedError pacc
    Atuple{}            -> unexpectedError pacc
    Aprj{}              -> unexpectedError pacc
    Use{}               -> unexpectedError pacc
    Unit{}              -> unexpectedError pacc
    Aforeign{}          -> unexpectedError pacc
    Reshape{}           -> unexpectedError pacc

    Replicate{}         -> fusionError pacc
    Slice{}             -> fusionError pacc
    ZipWith{}           -> fusionError pacc

  where
    -- code generation for delayed arrays
    travD :: DelayedOpenAcc aenv (Array sh e) -> IRDelayed aenv (Array sh e)
    travD Manifest{}  = INTERNAL_ERROR(error) "llvmOfAcc" "expected delayed array"
    travD Delayed{..} = IRDelayed (travE extentD) (travF1 indexD) (travF1 linearIndexD)

    -- scalar code generation
    travF1 :: DelayedFun aenv (a -> b) -> IRFun1 aenv (a -> b)
    travF1 f = llvmOfFun1 f aenv

    travF2 :: DelayedFun aenv (a -> b -> c) -> IRFun2 aenv (a -> b -> c)
    travF2 f = llvmOfFun2 f aenv

    travE :: DelayedExp aenv t -> IRExp aenv t
    travE e = llvmOfOpenExp e Empty aenv

    -- sadness
    unexpectedError x   = INTERNAL_ERROR(error) "llvmOfAcc" $ "unexpected array primitive: " ++ showPreAccOp x
    fusionError x       = INTERNAL_ERROR(error) "llvmOfAcc" $ "unexpected fusible material: " ++ showPreAccOp x

