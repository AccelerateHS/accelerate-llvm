{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Skeleton
-- Copyright   : [2015..2016] Trevor L. McDonell
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
import Data.Array.Accelerate.AST                                hiding ( Val(..), prj, stencil, stencilAccess )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Permute
import Data.Array.Accelerate.LLVM.CodeGen.Stencil
import Data.Array.Accelerate.LLVM.CodeGen.Sugar


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
                -> Gamma       aenv
                -> IRFun1 arch aenv (sh -> e)
                -> CodeGen (IROpenAcc arch aenv (Array sh e))

  transform     :: (Shape sh, Shape sh', Elt a, Elt b)
                => arch
                -> Gamma          aenv
                -> IRFun1    arch aenv (sh' -> sh)
                -> IRFun1    arch aenv (a -> b)
                -> IRDelayed arch aenv (Array sh a)
                -> CodeGen (IROpenAcc arch aenv (Array sh' b))

  map           :: (Shape sh, Elt a, Elt b)
                => arch
                -> Gamma          aenv
                -> IRFun1    arch aenv (a -> b)
                -> IRDelayed arch aenv (Array sh a)
                -> CodeGen (IROpenAcc arch aenv (Array sh b))

  fold          :: (Shape sh, Elt e)
                => arch
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array sh e))

  fold1         :: (Shape sh, Elt e)
                => arch
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array sh e))

  foldSeg       :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => arch
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> IRDelayed arch aenv (Segments i)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  fold1Seg      :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => arch
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> IRDelayed arch aenv (Segments i)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanl         :: (Shape sh, Elt e)
                => arch
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanl'        :: (Shape sh, Elt e)
                => arch
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e, Array sh e))

  scanl1        :: (Shape sh, Elt e)
                => arch
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanr         :: (Shape sh, Elt e)
                => arch
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanr'        :: (Shape sh, Elt e)
                => arch
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e, Array sh e))

  scanr1        :: (Shape sh, Elt e)
                => arch
                -> Gamma          aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  permute       :: (Shape sh, Shape sh', Elt e)
                => arch
                -> Gamma             aenv
                -> IRPermuteFun arch aenv (e -> e -> e)
                -> IRFun1       arch aenv (sh -> sh')
                -> IRDelayed    arch aenv (Array sh e)
                -> CodeGen (IROpenAcc arch aenv (Array sh' e))

  backpermute   :: (Shape sh, Shape sh', Elt e)
                => arch
                -> Gamma          aenv
                -> IRFun1    arch aenv (sh' -> sh)
                -> IRDelayed arch aenv (Array sh e)
                -> CodeGen (IROpenAcc arch aenv (Array sh' e))

  stencil       :: (Stencil sh a stencil, Elt b)
                => arch
                -> Gamma aenv
                -> IRFun1 arch aenv (stencil -> b)
                -> Boundary (IR a)
                -> IRManifest arch aenv (Array sh a)
                -> CodeGen (IROpenAcc arch aenv (Array sh b))

  stencil2      :: (Stencil sh a stencil1, Stencil sh b stencil2, Elt c)
                => arch
                -> Gamma aenv
                -> IRFun2 arch aenv (stencil1 -> stencil2 -> c)
                -> Boundary (IR a)
                -> IRManifest arch aenv (Array sh a)
                -> Boundary (IR b)
                -> IRManifest arch aenv (Array sh b)
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
    -> Gamma          aenv
    -> IRFun1    arch aenv (a -> b)
    -> IRDelayed arch aenv (Array sh a)
    -> CodeGen (IROpenAcc arch aenv (Array sh b))
defaultMap arch aenv f a
  = transform arch aenv id f a

{-# INLINEABLE defaultBackpermute #-}
defaultBackpermute
    :: (Skeleton arch, Shape sh, Shape sh', Elt e)
    => arch
    -> Gamma          aenv
    -> IRFun1    arch aenv (sh' -> sh)
    -> IRDelayed arch aenv (Array sh e)
    -> CodeGen (IROpenAcc arch aenv (Array sh' e))
defaultBackpermute arch aenv p a
  = transform arch aenv p id a

{-# INLINEABLE defaultTransform #-}
defaultTransform
    :: (Skeleton arch, Shape sh', Elt b)
    => arch
    -> Gamma          aenv
    -> IRFun1    arch aenv (sh' -> sh)
    -> IRFun1    arch aenv (a -> b)
    -> IRDelayed arch aenv (Array sh a)
    -> CodeGen (IROpenAcc arch aenv (Array sh' b))
defaultTransform arch aenv p f IRDelayed{..}
  = generate arch aenv . IRFun1 $ \ix -> do
      ix' <- app1 p ix
      a   <- app1 delayedIndex ix'
      app1 f a

{-# INLINEABLE defaultStencil1 #-}
defaultStencil1
    :: (Skeleton arch, Stencil sh a stencil, Elt b)
    => arch
    -> Gamma aenv
    -> IRFun1 arch aenv (stencil -> b)
    -> Boundary (IR a)
    -> IRManifest arch aenv (Array sh a)
    -> CodeGen (IROpenAcc arch aenv (Array sh b))
defaultStencil1 arch aenv f boundary (IRManifest v)
  = generate arch aenv . IRFun1 $ \ix -> do
      sten <- stencilAccess boundary (irArray (aprj v aenv)) ix
      app1 f sten

{-# INLINEABLE defaultStencil2 #-}
defaultStencil2
    :: (Skeleton arch, Stencil sh a stencil1, Stencil sh b stencil2, Elt c)
    => arch
    -> Gamma aenv
    -> IRFun2 arch aenv (stencil1 -> stencil2 -> c)
    -> Boundary (IR a)
    -> IRManifest arch aenv (Array sh a)
    -> Boundary (IR b)
    -> IRManifest arch aenv (Array sh b)
    -> CodeGen (IROpenAcc arch aenv (Array sh c))
defaultStencil2 arch aenv f boundary1 (IRManifest v1) boundary2 (IRManifest v2)
  = generate arch aenv . IRFun1 $ \ix -> do
      sten1 <- stencilAccess boundary1 (irArray (aprj v1 aenv)) ix
      sten2 <- stencilAccess boundary2 (irArray (aprj v2 aenv)) ix
      app2 f sten1 sten2

