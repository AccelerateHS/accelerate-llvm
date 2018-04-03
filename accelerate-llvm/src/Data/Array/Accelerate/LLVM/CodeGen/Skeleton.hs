{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Skeleton
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Skeleton (

  Skeleton(..),

) where

import Prelude                                                  hiding ( id )

-- accelerate
import Data.Array.Accelerate.AST                                hiding ( Val(..), PreBoundary(..), prj, stencil )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Permute
import Data.Array.Accelerate.LLVM.CodeGen.Stencil
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache


-- | A class covering code generation for all of the primitive array operations.
-- Client backends implement an instance of this class.
--
-- Minimal complete definition:
--   * generate
--   * fold, fold1, foldSeg, fold1Seg
--   * scanl, scanl', scanl1, scanr, scanr', scanr1
--   * permute
--
class Skeleton arch where
  {-# MINIMAL generate, fold, fold1, foldSeg, fold1Seg, scanl, scanl', scanl1,
              scanr, scanr', scanr1, permute #-}

  generate      :: (Shape sh, Elt e)
                => arch
                -> UID
                -> Gamma       aenv
                -> IRFun1 arch aenv (sh -> e)
                -> CodeGen (IROpenAcc arch aenv (Array sh e))

  transform     :: (Shape sh, Shape sh', Elt a, Elt b)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun1    arch aenv (sh' -> sh)
                -> IRFun1    arch aenv (a -> b)
                -> IRDelayed arch aenv (Array sh a)
                -> CodeGen (IROpenAcc arch aenv (Array sh' b))

  map           :: (Shape sh, Elt a, Elt b)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun1    arch aenv (a -> b)
                -> IRDelayed arch aenv (Array sh a)
                -> CodeGen (IROpenAcc arch aenv (Array sh b))

  fold          :: (Shape sh, Elt e)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array sh e))

  fold1         :: (Shape sh, Elt e)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array sh e))

  foldSeg       :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> IRDelayed arch aenv (Segments i)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  fold1Seg      :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> IRDelayed arch aenv (Segments i)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanl         :: (Shape sh, Elt e)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanl'        :: (Shape sh, Elt e)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e, Array sh e))

  scanl1        :: (Shape sh, Elt e)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanr         :: (Shape sh, Elt e)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanr'        :: (Shape sh, Elt e)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e, Array sh e))

  scanr1        :: (Shape sh, Elt e)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  permute       :: (Shape sh, Shape sh', Elt e)
                => arch
                -> UID
                -> Gamma             aenv
                -> IRPermuteFun arch aenv (e -> e -> e)
                -> IRFun1       arch aenv (sh -> sh')
                -> IRDelayed    arch aenv (Array sh e)
                -> CodeGen (IROpenAcc arch aenv (Array sh' e))

  backpermute   :: (Shape sh, Shape sh', Elt e)
                => arch
                -> UID
                -> Gamma          aenv
                -> IRFun1    arch aenv (sh' -> sh)
                -> IRDelayed arch aenv (Array sh e)
                -> CodeGen (IROpenAcc arch aenv (Array sh' e))

  stencil       :: (Stencil sh a stencil, Elt b)
                => arch
                -> UID
                -> Gamma aenv
                -> IRFun1 arch aenv (stencil -> b)
                -> IRBoundary arch aenv (Array sh a)
                -> IRDelayed  arch aenv (Array sh a)
                -> CodeGen (IROpenAcc arch aenv (Array sh b))

  stencil2      :: (Stencil sh a stencil1, Stencil sh b stencil2, Elt c)
                => arch
                -> UID
                -> Gamma aenv
                -> IRFun2 arch aenv (stencil1 -> stencil2 -> c)
                -> IRBoundary arch aenv (Array sh a)
                -> IRDelayed  arch aenv (Array sh a)
                -> IRBoundary arch aenv (Array sh b)
                -> IRDelayed  arch aenv (Array sh b)
                -> CodeGen (IROpenAcc arch aenv (Array sh c))

  -- Default instances
  -- -----------------
  map           = defaultMap
  backpermute   = defaultBackpermute
  transform     = defaultTransform
  stencil       = defaultStencil1
  stencil2      = defaultStencil2


{-# INLINE id #-}
id :: forall arch aenv a. IRFun1 arch aenv (a -> a)
id = IRFun1 return

{-# INLINEABLE defaultMap #-}
defaultMap
    :: (Skeleton arch, Shape sh, Elt a, Elt b)
    => arch
    -> UID
    -> Gamma          aenv
    -> IRFun1    arch aenv (a -> b)
    -> IRDelayed arch aenv (Array sh a)
    -> CodeGen (IROpenAcc arch aenv (Array sh b))
defaultMap arch uid aenv f a
  = transform arch uid aenv id f a

{-# INLINEABLE defaultBackpermute #-}
defaultBackpermute
    :: (Skeleton arch, Shape sh, Shape sh', Elt e)
    => arch
    -> UID
    -> Gamma          aenv
    -> IRFun1    arch aenv (sh' -> sh)
    -> IRDelayed arch aenv (Array sh e)
    -> CodeGen (IROpenAcc arch aenv (Array sh' e))
defaultBackpermute arch uid aenv p a
  = transform arch uid aenv p id a

{-# INLINEABLE defaultTransform #-}
defaultTransform
    :: (Skeleton arch, Shape sh', Elt b)
    => arch
    -> UID
    -> Gamma          aenv
    -> IRFun1    arch aenv (sh' -> sh)
    -> IRFun1    arch aenv (a -> b)
    -> IRDelayed arch aenv (Array sh a)
    -> CodeGen (IROpenAcc arch aenv (Array sh' b))
defaultTransform arch uid aenv p f IRDelayed{..}
  = generate arch uid aenv . IRFun1 $ \ix -> do
      ix' <- app1 p ix
      a   <- app1 delayedIndex ix'
      app1 f a

{-# INLINEABLE defaultStencil1 #-}
defaultStencil1
    :: (Skeleton arch, Stencil sh a stencil, Elt b)
    => arch
    -> UID
    -> Gamma aenv
    -> IRFun1 arch aenv (stencil -> b)
    -> IRBoundary arch aenv (Array sh a)
    -> IRDelayed  arch aenv (Array sh a)
    -> CodeGen (IROpenAcc arch aenv (Array sh b))
defaultStencil1 arch uid aenv f boundary arr
  = generate arch uid aenv . IRFun1 $ \ix -> do
      sten <- stencilAccess boundary arr ix
      app1 f sten

{-# INLINEABLE defaultStencil2 #-}
defaultStencil2
    :: (Skeleton arch, Stencil sh a stencil1, Stencil sh b stencil2, Elt c)
    => arch
    -> UID
    -> Gamma aenv
    -> IRFun2 arch aenv (stencil1 -> stencil2 -> c)
    -> IRBoundary arch aenv (Array sh a)
    -> IRDelayed  arch aenv (Array sh a)
    -> IRBoundary arch aenv (Array sh b)
    -> IRDelayed  arch aenv (Array sh b)
    -> CodeGen (IROpenAcc arch aenv (Array sh c))
defaultStencil2 arch uid aenv f boundary1 arr1 boundary2 arr2
  = generate arch uid aenv . IRFun1 $ \ix -> do
      sten1 <- stencilAccess boundary1 arr1 ix
      sten2 <- stencilAccess boundary2 arr2 ix
      app2 f sten1 sten2

