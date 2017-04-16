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

  PreOpenAfun(..),
  PreOpenExp(..),
  PreOpenFun(..),
  Idx(..), Val(..), prj,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.AST
    ( PreOpenAfun(..), PreOpenExp(..), PreOpenFun(..), Idx(..), Val(..), PreExp, prj )


-- | Non-computational array program operations, parameterised over array
-- variables represented as de Bruijn indices.
--
data PreOpenAccCommand acc aenv a where

  Avar        :: Arrays arrs
              => Idx                   aenv arrs
              -> PreOpenAccCommand acc aenv arrs

  Alet        :: (Arrays bnd, Arrays body)
              => acc                   aenv        bnd
              -> acc                   (aenv, bnd) body
              -> PreOpenAccCommand acc aenv        body

  Use         :: Arrays arrs
              => ArrRepr arrs
              -> PreOpenAccCommand acc aenv arrs

  Unit        :: Elt e
              => PreExp            acc aenv e
              -> PreOpenAccCommand acc aenv (Scalar e)

  Atuple      :: (Arrays arrs, IsAtuple arrs)
              => Atuple           (acc aenv) (TupleRepr arrs)
              -> PreOpenAccCommand acc aenv  arrs

  Aprj        :: (Arrays arrs, IsAtuple arrs, Arrays a)
              => TupleIdx (TupleRepr arrs) a
              -> acc                   aenv arrs
              -> PreOpenAccCommand acc aenv a

  Apply       :: (Arrays as, Arrays bs)
              => PreOpenAfun       acc aenv (as -> bs)
              -> acc                   aenv as
              -> PreOpenAccCommand acc aenv bs

  Aforeign    :: (Arrays as, Arrays bs, Foreign asm)
              => asm                       (as -> bs)
              -> acc                   aenv as
              -> PreOpenAccCommand acc aenv bs

  Acond       :: Arrays arrs
              => PreExp            acc aenv Bool
              -> acc                   aenv arrs
              -> acc                   aenv arrs
              -> PreOpenAccCommand acc aenv arrs

  Awhile      :: Arrays arrs
              => PreOpenAfun       acc aenv (arrs -> Scalar Bool)
              -> PreOpenAfun       acc aenv (arrs -> arrs)
              -> acc                   aenv arrs
              -> PreOpenAccCommand acc aenv arrs

  Reshape     :: (Shape sh, Shape sh', Elt e)
              => PreExp            acc aenv sh
              -> Idx                   aenv (Array sh' e)
              -> PreOpenAccCommand acc aenv (Array sh  e)

  Unzip       :: (Elt tup, Elt e)
              => TupleIdx (TupleRepr tup) e
              -> Idx                   aenv (Array sh tup)
              -> PreOpenAccCommand acc aenv (Array sh e)


-- | Collective array computations parameterised over array variables
-- represented as de Bruijn indices.
--
data PreOpenAccSkeleton acc aenv a where

  Map         :: (Shape sh, Elt e)
              => PreExp             acc aenv sh
              -> PreOpenAccSkeleton acc aenv (Array sh e)

  Generate    :: (Shape sh, Elt e)
              => PreExp             acc aenv sh
              -> PreOpenAccSkeleton acc aenv (Array sh e)

  Transform   :: (Shape sh, Elt e)
              => PreExp             acc aenv sh
              -> PreOpenAccSkeleton acc aenv (Array sh e)

  Backpermute :: (Shape sh, Elt e)
              => PreExp             acc aenv sh
              -> PreOpenAccSkeleton acc aenv (Array sh e)

  Fold        :: (Shape sh, Elt e)
              => PreExp             acc aenv (sh :. Int)
              -> PreOpenAccSkeleton acc aenv (Array sh e)

  Fold1       :: (Shape sh, Elt e)
              => PreExp             acc aenv (sh :. Int)
              -> PreOpenAccSkeleton acc aenv (Array sh e)

  FoldSeg     :: (Shape sh, Elt e)
              => PreExp             acc aenv (sh :. Int)
              -> PreExp             acc aenv (Z  :. Int)
              -> PreOpenAccSkeleton acc aenv (Array (sh:.Int) e)

  Fold1Seg    :: (Shape sh, Elt e)
              => PreExp             acc aenv (sh :. Int)
              -> PreExp             acc aenv (Z  :. Int)
              -> PreOpenAccSkeleton acc aenv (Array (sh:.Int) e)

  Scanl       :: (Shape sh, Elt e)
              => PreExp             acc aenv (sh :. Int)
              -> PreOpenAccSkeleton acc aenv (Array (sh:.Int) e)

  Scanl1      :: (Shape sh, Elt e)
              => PreExp             acc aenv (sh :. Int)
              -> PreOpenAccSkeleton acc aenv (Array (sh:.Int) e)

  Scanl'      :: (Shape sh, Elt e)
              => PreExp             acc aenv (sh :. Int)
              -> PreOpenAccSkeleton acc aenv (Array (sh:.Int) e, Array sh e)

  Scanr       :: (Shape sh, Elt e)
              => PreExp             acc aenv (sh :. Int)
              -> PreOpenAccSkeleton acc aenv (Array (sh:.Int) e)

  Scanr1      :: (Shape sh, Elt e)
              => PreExp             acc aenv (sh :. Int)
              -> PreOpenAccSkeleton acc aenv (Array (sh:.Int) e)

  Scanr'      :: (Shape sh, Elt e)
              => PreExp             acc aenv (sh :. Int)
              -> PreOpenAccSkeleton acc aenv (Array (sh:.Int) e, Array sh e)

  Permute     :: (Shape sh, Shape sh', Elt e)
              => PreExp             acc aenv sh               -- source
              -> acc                    aenv (Array sh' e)    -- default values
              -> PreOpenAccSkeleton acc aenv (Array sh' e)

  Stencil     :: (Shape sh, Elt a, Elt b)
              => Idx                    aenv (Array sh a)
              -> PreOpenAccSkeleton acc aenv (Array sh b)

  Stencil2    :: (Shape sh, Elt a, Elt b, Elt c)
              => Idx                    aenv (Array sh a)
              -> Idx                    aenv (Array sh b)
              -> PreOpenAccSkeleton acc aenv (Array sh c)

