{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Marshal
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Marshal
  where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma, Idx'(..) )
import Data.Array.Accelerate.LLVM.Execute.Environment
import Data.Array.Accelerate.LLVM.Execute.Async

import Data.DList                                               ( DList )
import qualified Data.DList                                     as DL
import qualified Data.IntMap                                    as IM


-- Marshalling arguments
-- ---------------------
class Async arch => Marshal arch where
  -- | A type family that is used to specify a concrete kernel argument and
  -- stream/context type for a given backend target.
  --
  type ArgR arch

  -- | Used to pass shapes as arguments to kernels.
  marshalInt :: Int -> ArgR arch

  -- | Pass arrays to kernels
  marshalScalarData' :: SingleType e -> ScalarArrayData e -> Par arch (DList (ArgR arch))

-- | Convert function arguments into stream a form suitable for function calls
-- The functions ending in a prime return a DList, other functions return lists.
--
marshalArrays :: forall arch arrs. Marshal arch => ArraysR arrs -> arrs -> Par arch [ArgR arch]
marshalArrays repr arrs = DL.toList <$> marshalArrays' @arch repr arrs

marshalArrays' :: forall arch arrs. Marshal arch => ArraysR arrs -> arrs -> Par arch (DList (ArgR arch))
marshalArrays' = marshalTupR' @arch (marshalArray' @arch)

marshalArray' :: forall arch a. Marshal arch => ArrayR a -> a -> Par arch (DList (ArgR arch))
marshalArray' (ArrayR shr tp) (Array sh a) = do
  arg1 <- marshalArrayData' @arch tp a
  let arg2 = marshalShape' @arch shr sh
  return $ arg1 `DL.append` arg2

marshalArrayData' :: forall arch t. Marshal arch => TypeR t -> ArrayData t -> Par arch (DList (ArgR arch))
marshalArrayData' TupRunit ()               = return DL.empty
marshalArrayData' (TupRpair t1 t2) (a1, a2) = do
  l1 <- marshalArrayData' t1 a1
  l2 <- marshalArrayData' t2 a2
  return $ l1 `DL.append` l2
marshalArrayData' (TupRsingle t) ad
  | ScalarArrayDict _ s <- scalarArrayDict t
  = marshalScalarData' @arch s ad

marshalEnv :: forall arch aenv. Marshal arch => Gamma aenv -> ValR arch aenv -> Par arch [ArgR arch]
marshalEnv g a = DL.toList <$> marshalEnv' g a

marshalEnv' :: forall arch aenv. Marshal arch => Gamma aenv -> ValR arch aenv -> Par arch (DList (ArgR arch))
marshalEnv' gamma aenv
    = fmap DL.concat
    $ mapM (\(_, Idx' repr idx) -> marshalArray' @arch repr =<< get (prj idx aenv)) (IM.elems gamma)

marshalShape :: forall arch sh. Marshal arch => ShapeR sh -> sh -> [ArgR arch]
marshalShape shr sh = DL.toList $ marshalShape' @arch shr sh

marshalShape' :: forall arch sh. Marshal arch => ShapeR sh -> sh -> DList (ArgR arch)
marshalShape' ShapeRz () = DL.empty
marshalShape' (ShapeRsnoc shr) (sh, n) = marshalShape' @arch shr sh `DL.snoc` marshalInt @arch n

type ParamsR arch = TupR (ParamR arch)

data ParamR arch a where
  ParamRarray  :: ArrayR (Array sh e) -> ParamR arch (Array sh e)
  ParamRmaybe  :: ParamR arch a       -> ParamR arch (Maybe a)
  ParamRfuture :: ParamR arch a       -> ParamR arch (FutureR arch a)
  ParamRenv    :: Gamma aenv          -> ParamR arch (ValR arch aenv)
  ParamRint    ::                        ParamR arch Int
  ParamRshape  :: ShapeR sh           -> ParamR arch sh
  ParamRargs   ::                        ParamR arch (DList (ArgR arch))

marshalParam' :: forall arch a. Marshal arch => ParamR arch a -> a -> Par arch (DList (ArgR arch))
marshalParam' (ParamRarray repr)  a        = marshalArray' repr a
marshalParam' (ParamRmaybe _   )  Nothing  = return $ DL.empty
marshalParam' (ParamRmaybe repr)  (Just a) = marshalParam' repr a
marshalParam' (ParamRfuture repr) future   = marshalParam' repr =<< get future
marshalParam' (ParamRenv gamma)   aenv     = marshalEnv'   gamma aenv
marshalParam'  ParamRint          x        = return $ DL.singleton $ marshalInt @arch x
marshalParam' (ParamRshape shr)   sh       = return $ marshalShape' @arch shr sh
marshalParam'  ParamRargs         args     = return args

marshalParams' :: forall arch a. Marshal arch => ParamsR arch a -> a -> Par arch (DList (ArgR arch))
marshalParams' = marshalTupR' @arch (marshalParam' @arch)

{-# INLINE marshalTupR' #-}
marshalTupR' :: forall arch s a. Marshal arch => (forall b. s b -> b -> Par arch (DList (ArgR arch))) -> TupR s a -> a -> Par arch (DList (ArgR arch))
marshalTupR' _ TupRunit         ()       = return $ DL.empty
marshalTupR' f (TupRsingle t)   x        = f t x
marshalTupR' f (TupRpair t1 t2) (x1, x2) = DL.append <$> marshalTupR' @arch f t1 x1 <*> marshalTupR' @arch f t2 x2

