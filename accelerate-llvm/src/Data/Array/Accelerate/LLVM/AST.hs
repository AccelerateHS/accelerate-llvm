{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.AST
-- Copyright   : [2017..2019] The Accelerate Team
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

  PreAfun, PreOpenAfun(..),
  Fun, OpenFun(..),
  Exp, OpenExp(..),
  PairIdx(..),
  UnzipIdx(..),
  Idx(..), Var(..), ArrayVar, LeftHandSide(..), ALeftHandSide, ELeftHandSide, ShapeR(..),
  TupR(..), ArrayR(..), HasArraysRepr(..), arrayRepr,

) where

import Data.Array.Accelerate.LLVM.Execute.Async

import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.AST
    ( PreOpenAfun(..), OpenExp(..), OpenFun(..), Idx(..), PreAfun, Fun, Exp, Var(..), ArrayVar,
      LeftHandSide(..), ALeftHandSide, ELeftHandSide, HasArraysRepr(..), arrayRepr, ShapeR(..), PairIdx(..) )


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

  Unit        :: TupleType e
              -> Exp                        aenv e
              -> PreOpenAccCommand acc arch aenv (Scalar e)

  Apair       :: acc                   arch aenv arrs1
              -> acc                   arch aenv arrs2
              -> PreOpenAccCommand acc arch aenv (arrs1, arrs2)

  Anil        :: PreOpenAccCommand acc arch aenv ()

  Apply       :: ArraysR bs
              -> PreOpenAfun      (acc arch) aenv (as -> bs)
              -> acc                   arch  aenv as
              -> PreOpenAccCommand acc arch  aenv bs

  Aforeign    :: ArraysR bs
              -> String
              -> (as -> Par arch (FutureR arch bs))
              -> acc                   arch aenv as
              -> PreOpenAccCommand acc arch aenv bs

  Acond       :: Exp                         aenv Bool
              -> acc                   arch  aenv arrs
              -> acc                   arch  aenv arrs
              -> PreOpenAccCommand acc arch  aenv arrs

  Awhile      :: PreOpenAfun      (acc arch) aenv (arrs -> Scalar Bool)
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
  Map         :: TupleType b
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
  Fold        :: DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array sh e)

  Fold1       :: DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array sh e)

  FoldSeg     :: IntegralType i
              -> DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> DelayedOpenAcc     acc arch aenv (Segments i)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e)

  Fold1Seg    :: IntegralType i
              -> DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> DelayedOpenAcc     acc arch aenv (Segments i)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e)

  Scanl       :: DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e)

  Scanl1      :: DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e)

  Scanl'      :: DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e, Array sh e)

  Scanr       :: DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e)

  Scanr1      :: DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e)

  Scanr'      :: DelayedOpenAcc     acc arch aenv (Array (sh, Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh, Int) e, Array sh e)

  Permute     :: acc                    arch aenv (Array sh' e)     -- target array (default values)
              -> DelayedOpenAcc     acc arch aenv (Array sh  e)     -- source values
              -> PreOpenAccSkeleton acc arch aenv (Array sh' e)

  Stencil1    :: TupleType b
              -> sh                                                 -- stencil offset/halo size
              -> DelayedOpenAcc     acc arch aenv (Array sh a)
              -> PreOpenAccSkeleton acc arch aenv (Array sh b)

  Stencil2    :: TupleType c
              -> sh                                                 -- stencil offset/halo size
              -> DelayedOpenAcc     acc arch aenv (Array sh a)
              -> DelayedOpenAcc     acc arch aenv (Array sh b)
              -> PreOpenAccSkeleton acc arch aenv (Array sh c)

data UnzipIdx a b where
  UnzipId   ::                                   UnzipIdx a a
  UnzipPrj  :: PairIdx a b   -> UnzipIdx b c ->  UnzipIdx a c
  UnzipUnit ::                                   UnzipIdx a ()
  UnzipPair :: UnzipIdx a b1 -> UnzipIdx a b2 -> UnzipIdx a (b1, b2)

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

instance HasArraysRepr (acc arch) => HasArraysRepr (PreOpenAccCommand acc arch) where
  arraysRepr (Avar (Var repr _))                   = TupRsingle repr
  arraysRepr (Alet _ _ a)                          = arraysRepr a
  arraysRepr (Alloc repr _)                        = TupRsingle repr
  arraysRepr (Use repr _)                          = TupRsingle repr
  arraysRepr (Unit tp _)                           = TupRsingle $ ArrayR ShapeRz tp
  arraysRepr (Apair a1 a2)                         = arraysRepr a1 `TupRpair` arraysRepr a2
  arraysRepr Anil                                  = TupRunit
  arraysRepr (Apply repr _ _)                      = repr
  arraysRepr (Aforeign repr _ _ _)                 = repr
  arraysRepr (Acond _ a1 _)                        = arraysRepr a1
  arraysRepr (Awhile _ _ a)                        = arraysRepr a
  arraysRepr (Reshape shr _ (Var (ArrayR _ tp) _)) = TupRsingle $ ArrayR shr tp
  arraysRepr (Unzip idx (Var (ArrayR shr tp) _))   = TupRsingle $ ArrayR shr $ go idx tp
    where
      go :: UnzipIdx a b -> TupleType a -> TupleType b
      go UnzipId                    t              = t
      go (UnzipPrj PairIdxLeft ix)  (TupRpair t _) = go ix t
      go (UnzipPrj PairIdxRight ix) (TupRpair _ t) = go ix t
      go UnzipUnit                  _              = TupRunit
      go (UnzipPair ix1 ix2)        t              = go ix1 t `TupRpair` go ix2 t
      go _                          _              = error "Time enough for life to unfold all the precious things life has in store."

instance HasArraysRepr (acc arch) => HasArraysRepr (PreOpenAccSkeleton acc arch) where
  arraysRepr (Map tp a)               = let ArrayR shr _ = arrayRepr a
                                        in  TupRsingle $ ArrayR shr tp
  arraysRepr (Generate repr _)        = TupRsingle repr
  arraysRepr (Transform repr _ _)     = TupRsingle repr
  arraysRepr (Backpermute shr _ a)    = TupRsingle $ ArrayR shr $ arrayRtype $ arrayRepr a
  arraysRepr (Fold a)                 = let ArrayR (ShapeRsnoc shr) tp = arrayRepr a
                                        in  TupRsingle $ ArrayR shr tp
  arraysRepr (Fold1 a)                = let ArrayR (ShapeRsnoc shr) tp = arrayRepr a
                                        in  TupRsingle $ ArrayR shr tp
  arraysRepr (FoldSeg _ a _)          = arraysRepr a
  arraysRepr (Fold1Seg _ a _)         = arraysRepr a
  arraysRepr (Scanl a)                = arraysRepr a
  arraysRepr (Scanl1 a)               = arraysRepr a
  arraysRepr (Scanl' a)               = let ArrayR (ShapeRsnoc shr) tp = arrayRepr a
                                        in  TupRsingle (ArrayR (ShapeRsnoc shr) tp) `TupRpair` TupRsingle (ArrayR shr tp)
  arraysRepr (Scanr a)                = arraysRepr a
  arraysRepr (Scanr1 a)               = arraysRepr a
  arraysRepr (Scanr' a)               = let ArrayR (ShapeRsnoc shr) tp = arrayRepr a
                                        in  TupRsingle (ArrayR (ShapeRsnoc shr) tp) `TupRpair` TupRsingle (ArrayR shr tp)
  arraysRepr (Permute a _)            = arraysRepr a
  arraysRepr (Stencil1 tp _ a)        = let ArrayR shr _ = arrayRepr a
                                        in  TupRsingle $ ArrayR shr tp
  arraysRepr (Stencil2 tp _ a _)      = let ArrayR shr _ = arrayRepr a
                                        in  TupRsingle $ ArrayR shr tp

instance HasArraysRepr (acc arch) => HasArraysRepr (DelayedOpenAcc acc arch) where
  arraysRepr (Delayed  repr _) = TupRsingle repr
  arraysRepr (Manifest repr _) = repr
