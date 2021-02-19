{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.AST
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.AST (

  DelayedOpenAcc(..),
  PreOpenAccCommand(..),
  PreOpenAccSkeleton(..),
  UnzipIdx(..),
  HasInitialValue,

) where

import Data.Array.Accelerate.LLVM.Execute.Async

import Data.Array.Accelerate.AST                                    ( PreOpenAfun(..), HasArraysR(..), ArrayVar, ALeftHandSide, Exp, Direction, Message, PrimBool, arrayR )
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type


-- | Non-computational array program operations, parameterised over array
-- variables represented as de Bruijn indices.
--
data PreOpenAccCommand acc arch aenv a where

  Avar        :: ArrayVar                   aenv arrs
              -> PreOpenAccCommand acc arch aenv arrs

  Alet        :: ALeftHandSide bnd aenv aenv'
              -> acc                   arch aenv  bnd
              -> acc                   arch aenv' body
              -> PreOpenAccCommand acc arch aenv  body

  Alloc       :: ArrayR (Array sh e)
              -> Exp                        aenv sh
              -> PreOpenAccCommand acc arch aenv (Array sh e)

  Use         :: ArrayR (Array sh e)
              -> Array sh e
              -> PreOpenAccCommand acc arch aenv (Array sh e)

  Unit        :: TypeR e
              -> Exp                        aenv e
              -> PreOpenAccCommand acc arch aenv (Scalar e)

  Apair       :: acc                   arch aenv arrs1
              -> acc                   arch aenv arrs2
              -> PreOpenAccCommand acc arch aenv (arrs1, arrs2)

  Anil        :: PreOpenAccCommand acc arch aenv ()

  Atrace      :: Message                         arrs1
              -> acc                   arch aenv arrs1
              -> acc                   arch aenv arrs2
              -> PreOpenAccCommand acc arch aenv arrs2

  Apply       :: ArraysR bs
              -> PreOpenAfun      (acc arch) aenv (as -> bs)
              -> acc                   arch  aenv as
              -> PreOpenAccCommand acc arch  aenv bs

  Aforeign    :: ArraysR bs
              -> String
              -> (as -> Par arch (FutureR arch bs))
              -> acc                   arch aenv as
              -> PreOpenAccCommand acc arch aenv bs

  Acond       :: Exp                         aenv PrimBool
              -> acc                   arch  aenv arrs
              -> acc                   arch  aenv arrs
              -> PreOpenAccCommand acc arch  aenv arrs

  Awhile      :: PreOpenAfun      (acc arch) aenv (arrs -> Scalar PrimBool)
              -> PreOpenAfun      (acc arch) aenv (arrs -> arrs)
              -> acc                   arch  aenv arrs
              -> PreOpenAccCommand acc arch  aenv arrs

  Reshape     :: ShapeR  sh
              -> Exp                         aenv sh
              -> ArrayVar                    aenv (Array sh' e)
              -> PreOpenAccCommand acc arch  aenv (Array sh  e)

  Unzip       :: UnzipIdx tup e
              -> ArrayVar                   aenv (Array sh tup)
              -> PreOpenAccCommand acc arch aenv (Array sh e)


-- | Collective array computations parameterised over array variables
-- represented as de Bruijn indices.
--
data PreOpenAccSkeleton acc arch aenv a where

  -- Producers. The only way these terms can appear in the AST is if they
  -- are applied to a manifest array.
  --
  Map         :: TypeR b
              -> acc                    arch aenv (Array sh a)
              -> PreOpenAccSkeleton acc arch aenv (Array sh b)

  Generate    :: ArrayR (Array sh e)
              -> Exp                         aenv sh
              -> PreOpenAccSkeleton acc arch aenv (Array sh e)

  Transform   :: ArrayR (Array sh' b)
              -> Exp                         aenv sh'
              -> acc                    arch aenv (Array sh  a)
              -> PreOpenAccSkeleton acc arch aenv (Array sh' b)

  Backpermute :: ShapeR sh'
              -> Exp                         aenv sh'
              -> acc                    arch aenv (Array sh  e)
              -> PreOpenAccSkeleton acc arch aenv (Array sh' e)

  -- Consumers. These may have been applied to either manifest or delayed
  -- array data.
  --
  Fold        :: HasInitialValue
              -> DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array sh e)

  FoldSeg     :: IntegralType i
              -> HasInitialValue
              -> DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> DelayedOpenAcc     acc arch aenv (Segments i)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e)

  Scan        :: Direction
              -> HasInitialValue
              -> DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e)

  Scan'       :: Direction
              -> DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e, Array sh e)

  Permute     :: acc                    arch aenv (Array sh' e)     -- target array (default values)
              -> DelayedOpenAcc     acc arch aenv (Array sh  e)     -- source values
              -> PreOpenAccSkeleton acc arch aenv (Array sh' e)

  Stencil1    :: TypeR b
              -> sh                                                 -- stencil offset/halo size
              -> DelayedOpenAcc     acc arch aenv (Array sh a)
              -> PreOpenAccSkeleton acc arch aenv (Array sh b)

  Stencil2    :: TypeR c
              -> sh                                                 -- stencil offset/halo size
              -> DelayedOpenAcc     acc arch aenv (Array sh a)
              -> DelayedOpenAcc     acc arch aenv (Array sh b)
              -> PreOpenAccSkeleton acc arch aenv (Array sh c)

data UnzipIdx a b where
  UnzipId   ::                                   UnzipIdx a a
  UnzipPrj  :: PairIdx a b   -> UnzipIdx b c ->  UnzipIdx a c
  UnzipUnit ::                                   UnzipIdx a ()
  UnzipPair :: UnzipIdx a b1 -> UnzipIdx a b2 -> UnzipIdx a (b1, b2)

-- Denotes whether the fold or scan has an initial value.
-- When False, this is a fold1 or scan1.
--
type HasInitialValue = Bool

-- | Representation for array arguments.
--
-- If the argument is a delayed array (that is, it was fused into its
-- consumer) we only need to keep track of the extent of the argument. If
-- the argument is a manifest array, we recurse into the subterm.
--
data DelayedOpenAcc acc arch aenv a where
  Delayed     :: ArrayR (Array sh e)
              -> Exp aenv sh
              -> DelayedOpenAcc acc arch aenv (Array sh e)

  Manifest    :: ArraysR (Array sh e)
              -> acc arch aenv (Array sh e)
              -> DelayedOpenAcc acc arch aenv (Array sh e)

instance HasArraysR (acc arch) => HasArraysR (PreOpenAccCommand acc arch) where
  {-# INLINEABLE arraysR #-}
  arraysR (Avar (Var repr _))                   = TupRsingle repr
  arraysR (Alet _ _ a)                          = arraysR a
  arraysR (Alloc repr _)                        = TupRsingle repr
  arraysR (Use repr _)                          = TupRsingle repr
  arraysR (Unit tp _)                           = TupRsingle $ ArrayR ShapeRz tp
  arraysR (Apair a1 a2)                         = arraysR a1 `TupRpair` arraysR a2
  arraysR Anil                                  = TupRunit
  arraysR (Atrace _ _ a2)                       = arraysR a2
  arraysR (Apply repr _ _)                      = repr
  arraysR (Aforeign repr _ _ _)                 = repr
  arraysR (Acond _ a1 _)                        = arraysR a1
  arraysR (Awhile _ _ a)                        = arraysR a
  arraysR (Reshape shr _ (Var (ArrayR _ tp) _)) = TupRsingle $ ArrayR shr tp
  arraysR (Unzip idx (Var (ArrayR shr tp) _))   = TupRsingle $ ArrayR shr $ go idx tp
    where
      go :: UnzipIdx a b -> TypeR a -> TypeR b
      go UnzipId                    t              = t
      go (UnzipPrj PairIdxLeft ix)  (TupRpair t _) = go ix t
      go (UnzipPrj PairIdxRight ix) (TupRpair _ t) = go ix t
      go UnzipUnit                  _              = TupRunit
      go (UnzipPair ix1 ix2)        t              = go ix1 t `TupRpair` go ix2 t
      go _                          _              = error "Time enough for life to unfold all the precious things life has in store."

instance HasArraysR (acc arch) => HasArraysR (PreOpenAccSkeleton acc arch) where
  {-# INLINEABLE arraysR #-}
  arraysR (Map tp a)               = let ArrayR shr _ = arrayR a
                                     in  TupRsingle $ ArrayR shr tp
  arraysR (Generate repr _)        = TupRsingle repr
  arraysR (Transform repr _ _)     = TupRsingle repr
  arraysR (Backpermute shr _ a)    = TupRsingle $ ArrayR shr $ arrayRtype $ arrayR a
  arraysR (Fold _ a)               = let ArrayR (ShapeRsnoc shr) tp = arrayR a
                                     in  TupRsingle $ ArrayR shr tp
  arraysR (FoldSeg _ _ a _)        = arraysR a
  arraysR (Scan _ _ a)             = arraysR a
  arraysR (Scan' _ a)              = let ArrayR (ShapeRsnoc shr) tp = arrayR a
                                     in  TupRsingle (ArrayR (ShapeRsnoc shr) tp) `TupRpair` TupRsingle (ArrayR shr tp)
  arraysR (Permute a _)            = arraysR a
  arraysR (Stencil1 tp _ a)        = let ArrayR shr _ = arrayR a
                                     in  TupRsingle $ ArrayR shr tp
  arraysR (Stencil2 tp _ a _)      = let ArrayR shr _ = arrayR a
                                     in  TupRsingle $ ArrayR shr tp

instance HasArraysR (acc arch) => HasArraysR (DelayedOpenAcc acc arch) where
  {-# INLINEABLE arraysR #-}
  arraysR (Delayed  repr _) = TupRsingle repr
  arraysR (Manifest repr _) = repr

