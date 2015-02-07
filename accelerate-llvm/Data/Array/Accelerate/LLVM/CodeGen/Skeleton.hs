{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Skeleton
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Skeleton
  where

import Prelude                                                  hiding ( id )

-- accelerate
import Data.Array.Accelerate.AST                                hiding ( Val(..), prj, stencil )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad


-- | A class covering code generation for all of the primitive array operations.
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
  {-# MINIMAL generate, fold, fold1, foldSeg, fold1Seg, scanl, scanl', scanl1,
              scanr, scanr', scanr1, permute, stencil, stencil2 #-}

  generate      :: (Shape sh, Elt e)
                => arch
                -> Val aenv
                -> IRFun1 arch aenv (sh -> e)
                -> CodeGen (IROpenAcc arch aenv (Array sh e))

  transform     :: (Shape sh, Shape sh', Elt a, Elt b)
                => arch
                -> Val aenv
                -> IRFun1    arch aenv (sh' -> sh)
                -> IRFun1    arch aenv (a -> b)
                -> IRDelayed arch aenv (Array sh a)
                -> CodeGen (IROpenAcc arch aenv (Array sh' b))

  map           :: (Shape sh, Elt a, Elt b)
                => arch
                -> Val aenv
                -> IRFun1    arch aenv (a -> b)
                -> IRDelayed arch aenv (Array sh a)
                -> CodeGen (IROpenAcc arch aenv (Array sh b))

  fold          :: (Shape sh, Elt e)
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array sh e))

  fold1         :: (Shape sh, Elt e)
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen (IROpenAcc arch aenv (Array sh e))

  foldSeg       :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> IRDelayed arch aenv (Segments i)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  fold1Seg      :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Array (sh:.Int) e)
                -> IRDelayed arch aenv (Segments i)
                -> CodeGen (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanl         :: Elt e
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Vector e)
                -> CodeGen (IROpenAcc arch aenv (Vector e))

  scanl'        :: Elt e
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Vector e)
                -> CodeGen (IROpenAcc arch aenv (Vector e, Scalar e))

  scanl1        :: Elt e
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Vector e)
                -> CodeGen (IROpenAcc arch aenv (Vector e))

  scanr         :: Elt e
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Vector e)
                -> CodeGen (IROpenAcc arch aenv (Vector e))

  scanr'        :: Elt e
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRExp     arch aenv e
                -> IRDelayed arch aenv (Vector e)
                -> CodeGen (IROpenAcc arch aenv (Vector e, Scalar e))

  scanr1        :: Elt e
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRDelayed arch aenv (Vector e)
                -> CodeGen (IROpenAcc arch aenv (Vector e))

  permute       :: (Shape sh, Shape sh', Elt e)
                => arch
                -> Val aenv
                -> IRFun2    arch aenv (e -> e -> e)
                -> IRFun1    arch aenv (sh -> sh')
                -> IRDelayed arch aenv (Array sh e)
                -> CodeGen (IROpenAcc arch aenv (Array sh' e))

  backpermute   :: (Shape sh, Shape sh', Elt e)
                => arch
                -> Val aenv
                -> IRFun1    arch aenv (sh' -> sh)
                -> IRDelayed arch aenv (Array sh e)
                -> CodeGen (IROpenAcc arch aenv (Array sh' e))

  stencil       :: (Elt a, Elt b, Stencil sh a stencil)
                => arch
                -> Val aenv
                -> IRFun1 arch aenv (stencil -> b)
                -> Boundary (IRExp arch aenv a)
                -> IRManifest arch aenv (Array sh a)
                -> CodeGen (IROpenAcc arch aenv (Array sh b))

  stencil2      :: (Elt a, Elt b, Elt c, Stencil sh a stencil1, Stencil sh b stencil2)
                => arch
                -> Val aenv
                -> IRFun2 arch aenv (stencil1 -> stencil2 -> c)
                -> Boundary (IRExp arch aenv a)
                -> IRManifest arch aenv (Array sh a)
                -> Boundary (IRExp arch aenv b)
                -> IRManifest arch aenv (Array sh b)
                -> CodeGen (IROpenAcc arch aenv (Array sh c))

  -- Default instances
  -- -----------------
  map arch aenv f a             = transform arch aenv id f a
  backpermute arch aenv p a     = transform arch aenv p id a
  transform arch aenv p f IRDelayed{..} =
    generate arch aenv . IRFun1 $ \ix -> do
      ix' <- app1 p ix
      a   <- app1 delayedIndex ix'
      app1 f a

  stencil                       = $internalError "stencil1" "no default instance yet"
  stencil2                      = $internalError "stencil2" "no default instance yet"


id :: forall arch aenv a. IRFun1 arch aenv (a -> a)
id = IRFun1 return

