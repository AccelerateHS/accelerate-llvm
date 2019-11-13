{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Skeleton
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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
class Skeleton arch where
  {-# MINIMAL generate, transform
            , fold, fold1 , foldSeg, fold1Seg
            , scanl, scanl', scanl1, scanr, scanr', scanr1
            , permute
            , stencil1, stencil2 #-}

  generate      :: (Shape sh, Elt e)
                => UID
                -> Gamma        aenv
                -> IRFun1  arch aenv (sh -> e)
                -> CodeGen arch      (IROpenAcc arch aenv (Array sh e))

  transform     :: (Shape sh, Shape sh', Elt a, Elt b)
                => UID
                -> Gamma        aenv
                -> IRFun1  arch aenv (sh' -> sh)
                -> IRFun1  arch aenv (a -> b)
                -> CodeGen arch      (IROpenAcc arch aenv (Array sh' b))

  map           :: (Shape sh, Elt a, Elt b)
                => UID
                -> Gamma        aenv
                -> IRFun1  arch aenv (a -> b)
                -> CodeGen arch      (IROpenAcc arch aenv (Array sh b))

  fold          :: (Shape sh, Elt e)
                => UID
                -> Gamma           aenv
                -> IRFun2     arch aenv (e -> e -> e)
                -> IRExp      arch aenv e
                -> MIRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen    arch      (IROpenAcc arch aenv (Array sh e))

  fold1         :: (Shape sh, Elt e)
                => UID
                -> Gamma           aenv
                -> IRFun2     arch aenv (e -> e -> e)
                -> MIRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen    arch      (IROpenAcc arch aenv (Array sh e))

  foldSeg       :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => UID
                -> Gamma           aenv
                -> IRFun2     arch aenv (e -> e -> e)
                -> IRExp      arch aenv e
                -> MIRDelayed arch aenv (Array (sh:.Int) e)
                -> MIRDelayed arch aenv (Segments i)
                -> CodeGen    arch      (IROpenAcc arch aenv (Array (sh:.Int) e))

  fold1Seg      :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => UID
                -> Gamma           aenv
                -> IRFun2     arch aenv (e -> e -> e)
                -> MIRDelayed arch aenv (Array (sh:.Int) e)
                -> MIRDelayed arch aenv (Segments i)
                -> CodeGen    arch      (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanl         :: (Shape sh, Elt e)
                => UID
                -> Gamma           aenv
                -> IRFun2     arch aenv (e -> e -> e)
                -> IRExp      arch aenv e
                -> MIRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen    arch      (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanl'        :: (Shape sh, Elt e)
                => UID
                -> Gamma           aenv
                -> IRFun2     arch aenv (e -> e -> e)
                -> IRExp      arch aenv e
                -> MIRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen    arch      (IROpenAcc arch aenv (((), Array (sh:.Int) e), Array sh e))

  scanl1        :: (Shape sh, Elt e)
                => UID
                -> Gamma           aenv
                -> IRFun2     arch aenv (e -> e -> e)
                -> MIRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen    arch      (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanr         :: (Shape sh, Elt e)
                => UID
                -> Gamma           aenv
                -> IRFun2     arch aenv (e -> e -> e)
                -> IRExp      arch aenv e
                -> MIRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen    arch      (IROpenAcc arch aenv (Array (sh:.Int) e))

  scanr'        :: (Shape sh, Elt e)
                => UID
                -> Gamma           aenv
                -> IRFun2     arch aenv (e -> e -> e)
                -> IRExp      arch aenv e
                -> MIRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen    arch      (IROpenAcc arch aenv (((), Array (sh:.Int) e), Array sh e))

  scanr1        :: (Shape sh, Elt e)
                => UID
                -> Gamma           aenv
                -> IRFun2     arch aenv (e -> e -> e)
                -> MIRDelayed arch aenv (Array (sh:.Int) e)
                -> CodeGen    arch      (IROpenAcc arch aenv (Array (sh:.Int) e))

  permute       :: (Shape sh, Shape sh', Elt e)
                => UID
                -> Gamma             aenv
                -> IRPermuteFun arch aenv (e -> e -> e)
                -> IRFun1       arch aenv (sh -> sh')
                -> MIRDelayed   arch aenv (Array sh e)
                -> CodeGen      arch      (IROpenAcc arch aenv (Array sh' e))

  backpermute   :: (Shape sh, Shape sh', Elt e)
                => UID
                -> Gamma          aenv
                -> IRFun1    arch aenv (sh' -> sh)
                -> CodeGen   arch      (IROpenAcc arch aenv (Array sh' e))

  stencil1      :: (Stencil sh a stencil, Elt b)
                => UID
                -> Gamma aenv
                -> IRFun1     arch aenv (stencil -> b)
                -> IRBoundary arch aenv (Array sh a)
                -> MIRDelayed arch aenv (Array sh a)
                -> CodeGen    arch      (IROpenAcc arch aenv (Array sh b))

  stencil2      :: (Stencil sh a stencil1, Stencil sh b stencil2, Elt c)
                => UID
                -> Gamma aenv
                -> IRFun2 arch aenv (stencil1 -> stencil2 -> c)
                -> IRBoundary arch aenv (Array sh a)
                -> MIRDelayed arch aenv (Array sh a)
                -> IRBoundary arch aenv (Array sh b)
                -> MIRDelayed arch aenv (Array sh b)
                -> CodeGen    arch      (IROpenAcc arch aenv (Array sh c))

  -- Default instances
  -- -----------------
  map           = defaultMap
  backpermute   = defaultBackpermute


{-# INLINE id #-}
id :: forall arch aenv a. IRFun1 arch aenv (a -> a)
id = IRFun1 return

{-# INLINEABLE defaultMap #-}
defaultMap
    :: (Skeleton arch, Shape sh, Elt a, Elt b)
    => UID
    -> Gamma          aenv
    -> IRFun1    arch aenv (a -> b)
    -> CodeGen   arch      (IROpenAcc arch aenv (Array sh b))
defaultMap uid aenv f
  = transform uid aenv id f

{-# INLINEABLE defaultBackpermute #-}
defaultBackpermute
    :: (Skeleton arch, Shape sh, Shape sh', Elt e)
    => UID
    -> Gamma          aenv
    -> IRFun1    arch aenv (sh' -> sh)
    -> CodeGen   arch      (IROpenAcc arch aenv (Array sh' e))
defaultBackpermute uid aenv p
  = transform uid aenv p id

