{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover
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
import Data.Array.Accelerate.AST                                hiding ( Val(..), prj, stencil )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Trafo

import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad

-- standard library
import Prelude                                                  hiding ( map, scanl, scanl1, scanr, scanr1 )
import qualified Prelude                                        as P

#include "accelerate.h"


-- | A class covering code generation for all of the primitive operations.
-- Client backends implement an instance of this class.
--
-- Minimal complete definition:
--   * generate
--   * fold, fold1, foldSeg, fold1Seg
--   * scanl, scanl', scanl1, scanr, scanr', scanr1
--   * permute
--   * stencil, stencil2
--
class Skeleton arch where
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
  transform = defaultTransform

  map           :: (Shape sh, Elt a, Elt b)
                => arch
                -> Gamma aenv
                -> IRFun1    aenv (a -> b)
                -> IRDelayed aenv (Array sh a)
                -> CodeGen [Kernel arch aenv (Array sh b)]
  map = defaultMap

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

  foldSeg       :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRExp     aenv e
                -> IRDelayed aenv (Array (sh:.Int) e)
                -> IRDelayed aenv (Segments i)
                -> CodeGen [Kernel arch aenv (Array (sh:.Int) e)]

  fold1Seg      :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRDelayed aenv (Array (sh:.Int) e)
                -> IRDelayed aenv (Segments i)
                -> CodeGen [Kernel arch aenv (Array (sh:.Int) e)]

  scanl         :: Elt e
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRExp     aenv e
                -> IRDelayed aenv (Vector e)
                -> CodeGen [Kernel arch aenv (Vector e)]

  scanl'        :: Elt e
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRExp     aenv e
                -> IRDelayed aenv (Vector e)
                -> CodeGen [Kernel arch aenv (Vector e, Scalar e)]

  scanl1        :: Elt e
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRDelayed aenv (Vector e)
                -> CodeGen [Kernel arch aenv (Vector e)]

  scanr         :: Elt e
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRExp     aenv e
                -> IRDelayed aenv (Vector e)
                -> CodeGen [Kernel arch aenv (Vector e)]

  scanr'        :: Elt e
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRExp     aenv e
                -> IRDelayed aenv (Vector e)
                -> CodeGen [Kernel arch aenv (Vector e, Scalar e)]

  scanr1        :: Elt e
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRDelayed aenv (Vector e)
                -> CodeGen [Kernel arch aenv (Vector e)]

  permute       :: (Shape sh, Shape sh', Elt e)
                => arch
                -> Gamma aenv
                -> IRFun2    aenv (e -> e -> e)
                -> IRFun1    aenv (sh -> sh')
                -> IRDelayed aenv (Array sh e)
                -> CodeGen [Kernel arch aenv (Array sh' e)]

  backpermute   :: (Shape sh, Shape sh', Elt e)
                => arch
                -> Gamma aenv
                -> IRFun1    aenv (sh' -> sh)
                -> IRDelayed aenv (Array sh e)
                -> CodeGen [Kernel arch aenv (Array sh' e)]
  backpermute = defaultBackpermute

  stencil       :: Elt b        -- (Elt a, Stencil sh a stencil)
                => arch
                -> Gamma aenv
                -> IRFun1 aenv (stencil -> b)
                -> Boundary (IRExp aenv a)
                -> CodeGen [Kernel arch aenv (Array sh b)]

  stencil2      :: Elt c        -- (Elt a, Elt b, Stencil sh a stencil1, Stencil sh b stencil2)
                => arch
                -> Gamma aenv
                -> IRFun2 aenv (stencil1 -> stencil2 -> c)
                -> Boundary (IRExp aenv a)
                -> Boundary (IRExp aenv b)
                -> CodeGen [Kernel arch aenv (Array sh c)]


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
    Map f a                 -> map arch aenv (travF1 f) (travD a)
    Generate _ f            -> generate arch aenv (travF1 f)
    Transform _ p f a       -> transform arch aenv (travF1 p) (travF1 f) (travD a)
    Backpermute _ p a       -> backpermute arch aenv (travF1 p) (travD a)

    -- Consumers
    Fold f z a              -> fold arch aenv (travF2 f) (travE z) (travD a)
    Fold1 f a               -> fold1 arch aenv (travF2 f) (travD a)
    FoldSeg f z a s         -> foldSeg arch aenv (travF2 f) (travE z) (travD a) (travD s)
    Fold1Seg f a s          -> fold1Seg arch aenv (travF2 f) (travD a) (travD s)
    Scanl f z a             -> scanl arch aenv (travF2 f) (travE z) (travD a)
    Scanl' f z a            -> scanl' arch aenv (travF2 f) (travE z) (travD a)
    Scanl1 f a              -> scanl1 arch aenv (travF2 f) (travD a)
    Scanr f z a             -> scanr arch aenv (travF2 f) (travE z) (travD a)
    Scanr' f z a            -> scanr' arch aenv (travF2 f) (travE z) (travD a)
    Scanr1 f a              -> scanr1 arch aenv (travF2 f) (travD a)
    Permute f _ p a         -> permute arch aenv (travF2 f) (travF1 p) (travD a)
    Stencil f b a           -> stencil arch aenv (travF1 f) (travB a b)
    Stencil2 f b1 a1 b2 a2  -> stencil2 arch aenv (travF2 f) (travB a1 b1) (travB a2 b2)

    -- Non-computation forms: sadness
    Alet{}                  -> unexpectedError pacc
    Avar{}                  -> unexpectedError pacc
    Apply{}                 -> unexpectedError pacc
    Acond{}                 -> unexpectedError pacc
    Awhile{}                -> unexpectedError pacc
    Atuple{}                -> unexpectedError pacc
    Aprj{}                  -> unexpectedError pacc
    Use{}                   -> unexpectedError pacc
    Unit{}                  -> unexpectedError pacc
    Aforeign{}              -> unexpectedError pacc
    Reshape{}               -> unexpectedError pacc

    Replicate{}             -> fusionError pacc
    Slice{}                 -> fusionError pacc
    ZipWith{}               -> fusionError pacc

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

    travB :: forall sh e. Elt e
          => DelayedOpenAcc aenv (Array sh e)
          -> Boundary (EltRepr e)
          -> Boundary (IRExp aenv e)
    travB _ Clamp        = Clamp
    travB _ Mirror       = Mirror
    travB _ Wrap         = Wrap
    travB _ (Constant c)
      = Constant $ return
      $ P.map constOp (constant (eltType (undefined::e)) c)

    -- sadness
    unexpectedError x   = INTERNAL_ERROR(error) "llvmOfAcc" $ "unexpected array primitive: " ++ showPreAccOp x
    fusionError x       = INTERNAL_ERROR(error) "llvmOfAcc" $ "unexpected fusible material: " ++ showPreAccOp x


-- Default producers
-- -----------------

defaultMap
    :: (Shape sh, Elt a, Elt b, Skeleton arch)
    => arch
    -> Gamma aenv
    -> IRFun1    aenv (a -> b)
    -> IRDelayed aenv (Array sh a)
    -> CodeGen [Kernel arch aenv (Array sh b)]
defaultMap arch aenv f a = transform arch aenv return f a

defaultBackpermute
    :: (Shape sh, Shape sh', Elt e, Skeleton arch)
    => arch
    -> Gamma aenv
    -> IRFun1    aenv (sh' -> sh)
    -> IRDelayed aenv (Array sh e)
    -> CodeGen [Kernel arch aenv (Array sh' e)]
defaultBackpermute arch aenv p a = transform arch aenv p return a

defaultTransform
    :: (Shape sh, Shape sh', Elt a, Elt b, Skeleton arch)
    => arch
    -> Gamma aenv
    -> IRFun1    aenv (sh' -> sh)
    -> IRFun1    aenv (a -> b)
    -> IRDelayed aenv (Array sh a)
    -> CodeGen [Kernel arch aenv (Array sh' b)]
defaultTransform arch aenv p f IRDelayed{..} =
  generate arch aenv $ \ix -> do
    ix' <- p ix
    a   <- delayedIndex ix'
    f a

