{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.AST
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.AST (

  PreOpenAccCommand(..),
  PreOpenAccSkeleton(..),

  PreAfun, PreOpenAfun(..),
  PreFun,  PreOpenFun(..),
  PreExp,  PreOpenExp(..),
  Idx(..), Val(..), prj,

) where

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Execute.Async

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.AST
    ( PreOpenAfun(..), PreOpenExp(..), PreOpenFun(..), Idx(..), Val(..), PreAfun, PreFun, PreExp, prj )


-- | Non-computational array program operations, parameterised over array
-- variables represented as de Bruijn indices.
--
data PreOpenAccCommand acc arch aenv a where

  Avar        :: Arrays arrs
              => Idx                        aenv arrs
              -> PreOpenAccCommand acc arch aenv arrs

  Alet        :: (Arrays bnd, Arrays body)
              => acc                   arch aenv        bnd
              -> acc                   arch (aenv, bnd) body
              -> PreOpenAccCommand acc arch aenv        body

  Use         :: Arrays arrs
              => ArrRepr arrs
              -> PreOpenAccCommand acc arch aenv arrs

  Unit        :: Elt e
              => PreExp           (acc arch) aenv e
              -> PreOpenAccCommand acc arch  aenv (Scalar e)

  Atuple      :: (Arrays arrs, IsAtuple arrs)
              => Atuple           (acc arch aenv) (TupleRepr arrs)
              -> PreOpenAccCommand acc arch aenv  arrs

  Aprj        :: (Arrays arrs, IsAtuple arrs, Arrays a)
              => TupleIdx (TupleRepr arrs) a
              -> acc                   arch aenv arrs
              -> PreOpenAccCommand acc arch aenv a

  Apply       :: (Arrays as, Arrays bs)
              => PreOpenAfun      (acc arch) aenv (as -> bs)
              -> acc                   arch  aenv as
              -> PreOpenAccCommand acc arch  aenv bs

  Aforeign    :: (Arrays as, Arrays bs)
              => String
              -> (StreamR arch -> as -> LLVM arch bs)
              -> acc                   arch aenv as
              -> PreOpenAccCommand acc arch aenv bs

  Acond       :: Arrays arrs
              => PreExp           (acc arch) aenv Bool
              -> acc                   arch  aenv arrs
              -> acc                   arch  aenv arrs
              -> PreOpenAccCommand acc arch  aenv arrs

  Awhile      :: Arrays arrs
              => PreOpenAfun      (acc arch) aenv (arrs -> Scalar Bool)
              -> PreOpenAfun      (acc arch) aenv (arrs -> arrs)
              -> acc                   arch  aenv arrs
              -> PreOpenAccCommand acc arch  aenv arrs

  Reshape     :: (Shape sh, Shape sh', Elt e)
              => PreExp           (acc arch) aenv sh
              -> Idx                         aenv (Array sh' e)
              -> PreOpenAccCommand acc arch  aenv (Array sh  e)

  Unzip       :: (Elt tup, Elt e)
              => TupleIdx (TupleRepr tup) e
              -> Idx                        aenv (Array sh tup)
              -> PreOpenAccCommand acc arch aenv (Array sh e)


-- | Collective array computations parameterised over array variables
-- represented as de Bruijn indices.
--
data PreOpenAccSkeleton acc arch aenv a where

  Map         :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv sh
              -> PreOpenAccSkeleton acc arch  aenv (Array sh e)

  Generate    :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv sh
              -> PreOpenAccSkeleton acc arch  aenv (Array sh e)

  Transform   :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv sh
              -> PreOpenAccSkeleton acc arch  aenv (Array sh e)

  Backpermute :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv sh
              -> PreOpenAccSkeleton acc arch  aenv (Array sh e)

  Fold        :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv (sh :. Int)
              -> PreOpenAccSkeleton acc arch  aenv (Array sh e)

  Fold1       :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv (sh :. Int)
              -> PreOpenAccSkeleton acc arch  aenv (Array sh e)

  FoldSeg     :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv (sh :. Int)
              -> PreExp            (acc arch) aenv (Z  :. Int)
              -> PreOpenAccSkeleton acc arch  aenv (Array (sh:.Int) e)

  Fold1Seg    :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv (sh :. Int)
              -> PreExp            (acc arch) aenv (Z  :. Int)
              -> PreOpenAccSkeleton acc arch  aenv (Array (sh:.Int) e)

  Scanl       :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv (sh :. Int)
              -> PreOpenAccSkeleton acc arch  aenv (Array (sh:.Int) e)

  Scanl1      :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv (sh :. Int)
              -> PreOpenAccSkeleton acc arch  aenv (Array (sh:.Int) e)

  Scanl'      :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv (sh :. Int)
              -> PreOpenAccSkeleton acc arch  aenv (Array (sh:.Int) e, Array sh e)

  Scanr       :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv (sh :. Int)
              -> PreOpenAccSkeleton acc arch  aenv (Array (sh:.Int) e)

  Scanr1      :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv (sh :. Int)
              -> PreOpenAccSkeleton acc arch  aenv (Array (sh:.Int) e)

  Scanr'      :: (Shape sh, Elt e)
              => PreExp            (acc arch) aenv (sh :. Int)
              -> PreOpenAccSkeleton acc arch  aenv (Array (sh:.Int) e, Array sh e)

  Permute     :: (Shape sh, Shape sh', Elt e)
              => PreExp            (acc arch) aenv sh               -- source
              -> acc                    arch  aenv (Array sh' e)    -- default values
              -> PreOpenAccSkeleton acc arch  aenv (Array sh' e)

  Stencil     :: (Shape sh, Elt a, Elt b)
              => Idx                         aenv (Array sh a)
              -> PreOpenAccSkeleton acc arch aenv (Array sh b)

  Stencil2    :: (Shape sh, Elt a, Elt b, Elt c)
              => Idx                         aenv (Array sh a)
              -> Idx                         aenv (Array sh b)
              -> PreOpenAccSkeleton acc arch aenv (Array sh c)

