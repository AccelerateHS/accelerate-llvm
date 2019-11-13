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
  PreFun,  PreOpenFun(..),
  PreExp,  PreOpenExp(..),
  Idx(..), ArrayVar(..), LeftHandSide(..),
  HasArraysRepr(..),

) where

import Data.Array.Accelerate.LLVM.Execute.Async

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.AST
    ( PreOpenAfun(..), PreOpenExp(..), PreOpenFun(..), Idx(..), PreAfun, PreFun, PreExp, ArrayVar(..), LeftHandSide(..), HasArraysRepr(..) )


-- | Non-computational array program operations, parameterised over array
-- variables represented as de Bruijn indices.
--
data PreOpenAccCommand acc arch aenv a where

  Avar        :: ArrayVar                   aenv arrs
              -> PreOpenAccCommand acc arch aenv arrs

  Alet        :: LeftHandSide bnd aenv aenv'
              -> acc                   arch aenv  bnd
              -> acc                   arch aenv' body
              -> PreOpenAccCommand acc arch aenv  body

  Alloc       :: (Shape sh, Elt e)
              => PreExp           (acc arch) aenv sh
              -> PreOpenAccCommand acc arch  aenv (Array sh e)

  Use         :: (Shape sh, Elt e)
              => Array sh e
              -> PreOpenAccCommand acc arch aenv (Array sh e)

  Unit        :: Elt e
              => PreExp           (acc arch) aenv e
              -> PreOpenAccCommand acc arch  aenv (Scalar e)

  Apair       :: acc                   arch aenv arrs1
              -> acc                   arch aenv arrs2
              -> PreOpenAccCommand acc arch aenv (arrs1, arrs2)

  Anil        :: PreOpenAccCommand acc arch aenv ()

  Apply       :: PreOpenAfun      (acc arch) aenv (as -> bs)
              -> acc                   arch  aenv as
              -> PreOpenAccCommand acc arch  aenv bs

  Aforeign    :: String
              -> ArraysR bs
              -> (as -> Par arch (FutureR arch bs))
              -> acc                   arch aenv as
              -> PreOpenAccCommand acc arch aenv bs

  Acond       :: PreExp           (acc arch) aenv Bool
              -> acc                   arch  aenv arrs
              -> acc                   arch  aenv arrs
              -> PreOpenAccCommand acc arch  aenv arrs

  Awhile      :: PreOpenAfun      (acc arch) aenv (arrs -> Scalar Bool)
              -> PreOpenAfun      (acc arch) aenv (arrs -> arrs)
              -> acc                   arch  aenv arrs
              -> PreOpenAccCommand acc arch  aenv arrs

  Reshape     :: (Shape sh, Shape sh', Elt e)
              => PreExp           (acc arch) aenv sh
              -> ArrayVar                    aenv (Array sh' e)
              -> PreOpenAccCommand acc arch  aenv (Array sh  e)

  Unzip       :: (Elt tup, Elt e)
              => TupleIdx (TupleRepr tup) e
              -> ArrayVar                   aenv (Array sh tup)
              -> PreOpenAccCommand acc arch aenv (Array sh e)


-- | Collective array computations parameterised over array variables
-- represented as de Bruijn indices.
--
data PreOpenAccSkeleton acc arch aenv a where

  -- Producers. The only way these terms can appear in the AST is if they
  -- are applied to a manifest array.
  --
  Map         :: (Shape sh, Elt a, Elt b)
              => acc                    arch aenv (Array sh a)
              -> PreOpenAccSkeleton acc arch aenv (Array sh b)

  Generate    :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv sh
              -> PreOpenAccSkeleton acc arch  aenv (Array sh e)

  Transform   :: (Shape sh, Shape sh', Elt a, Elt  b)
              => PreExp            (acc arch) aenv sh'
              -> acc                    arch  aenv (Array sh  a)
              -> PreOpenAccSkeleton acc arch  aenv (Array sh' b)

  Backpermute :: (Shape sh, Shape sh', Elt e)
              => PreExp            (acc arch) aenv sh'
              -> acc                    arch  aenv (Array sh  e)
              -> PreOpenAccSkeleton acc arch  aenv (Array sh' e)

  -- Consumers. These may have been applied to either manifest or delayed
  -- array data.
  --
  Fold        :: (Shape sh, Elt e)
              => DelayedOpenAcc     acc arch aenv (Array (sh :. Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array sh e)

  Fold1       :: (Shape sh, Elt e)
              => DelayedOpenAcc     acc arch aenv (Array (sh :. Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array sh e)

  FoldSeg     :: (Shape sh, Elt e, Elt i, IsIntegral i)
              => DelayedOpenAcc     acc arch aenv (Array (sh :. Int) e)
              -> DelayedOpenAcc     acc arch aenv (Segments i)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh :. Int) e)

  Fold1Seg    :: (Shape sh, Elt e, Elt i, IsIntegral i)
              => DelayedOpenAcc     acc arch aenv (Array (sh :. Int) e)
              -> DelayedOpenAcc     acc arch aenv (Segments i)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh :. Int) e)

  Scanl       :: (Shape sh, Elt e)
              => DelayedOpenAcc     acc arch aenv (Array (sh :. Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh :. Int) e)

  Scanl1      :: (Shape sh, Elt e)
              => DelayedOpenAcc     acc arch aenv (Array (sh :. Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh :. Int) e)

  Scanl'      :: (Shape sh, Elt e)
              => DelayedOpenAcc     acc arch aenv (Array (sh :. Int) e)
              -> PreOpenAccSkeleton acc arch aenv (ArrRepr (Array (sh :. Int) e, Array sh e))

  Scanr       :: (Shape sh, Elt e)
              => DelayedOpenAcc     acc arch aenv (Array (sh :. Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh :. Int) e)

  Scanr1      :: (Shape sh, Elt e)
              => DelayedOpenAcc     acc arch aenv (Array (sh :. Int) e)
              -> PreOpenAccSkeleton acc arch aenv (Array (sh :. Int) e)

  Scanr'      :: (Shape sh, Elt e)
              => DelayedOpenAcc     acc arch aenv (Array (sh :. Int) e)
              -> PreOpenAccSkeleton acc arch aenv (ArrRepr (Array (sh :. Int) e, Array sh e))

  Permute     :: (Shape sh, Shape sh', Elt e)
              => acc                    arch aenv (Array sh' e)     -- target array (default values)
              -> DelayedOpenAcc     acc arch aenv (Array sh  e)     -- source values
              -> PreOpenAccSkeleton acc arch aenv (Array sh' e)

  Stencil1    :: (Shape sh, Elt a, Elt b)
              => sh                                                 -- stencil offset/halo size
              -> DelayedOpenAcc     acc arch aenv (Array sh a)
              -> PreOpenAccSkeleton acc arch aenv (Array sh b)

  Stencil2    :: (Shape sh, Elt a, Elt b, Elt c)
              => sh                                                 -- stencil offset/halo size
              -> DelayedOpenAcc     acc arch aenv (Array sh a)
              -> DelayedOpenAcc     acc arch aenv (Array sh b)
              -> PreOpenAccSkeleton acc arch aenv (Array sh c)

-- | Representation for array arguments.
--
-- If the argument is a delayed array (that is, it was fused into its
-- consumer) we only need to keep track of the extent of the argument. If
-- the argument is a manifest array, we recurse into the subterm.
--
data DelayedOpenAcc acc arch aenv a where
  Delayed     :: PreExp (acc arch) aenv sh
              -> DelayedOpenAcc acc arch aenv (Array sh e)

  Manifest    :: acc arch aenv (Array sh e)
              -> DelayedOpenAcc acc arch aenv (Array sh e)

instance HasArraysRepr (acc arch) => HasArraysRepr (PreOpenAccCommand acc arch) where
  arraysRepr (Avar (ArrayVar _))    = ArraysRarray
  arraysRepr (Alet _ _ a)           = arraysRepr a
  arraysRepr Alloc{}                = ArraysRarray
  arraysRepr (Use _)                = ArraysRarray
  arraysRepr Unit{}                 = ArraysRarray
  arraysRepr (Apair a1 a2)          = arraysRepr a1 `ArraysRpair` arraysRepr a2
  arraysRepr Anil                   = ArraysRunit
  arraysRepr (Apply fn _)           = case fn of
    Alam _ (Abody a) -> arraysRepr a
    _                -> error "Where do you think you're going?" 
  arraysRepr (Aforeign _ repr _ _ ) = repr
  arraysRepr (Acond _ a1 _)         = arraysRepr a1
  arraysRepr (Awhile _ _ a)         = arraysRepr a
  arraysRepr Reshape{}              = ArraysRarray
  arraysRepr (Unzip _ (ArrayVar _)) = ArraysRarray

instance HasArraysRepr (acc arch) => HasArraysRepr (PreOpenAccSkeleton acc arch) where
  arraysRepr Map{}                  = ArraysRarray
  arraysRepr Generate{}             = ArraysRarray
  arraysRepr Transform{}            = ArraysRarray
  arraysRepr Backpermute{}          = ArraysRarray
  arraysRepr Fold{}                 = ArraysRarray
  arraysRepr Fold1{}                = ArraysRarray
  arraysRepr FoldSeg{}              = ArraysRarray
  arraysRepr Fold1Seg{}             = ArraysRarray
  arraysRepr Scanl{}                = ArraysRarray
  arraysRepr Scanl1{}               = ArraysRarray
  arraysRepr Scanl'{}               = arraysRtuple2
  arraysRepr Scanr{}                = ArraysRarray
  arraysRepr Scanr1{}               = ArraysRarray
  arraysRepr Scanr'{}               = arraysRtuple2
  arraysRepr Permute{}              = ArraysRarray
  arraysRepr Stencil1{}             = ArraysRarray
  arraysRepr Stencil2{}             = ArraysRarray
