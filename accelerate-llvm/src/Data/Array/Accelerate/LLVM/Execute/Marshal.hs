{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Marshal
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Marshal
  where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma, Idx'(..) )
import Data.Array.Accelerate.LLVM.Execute.Environment
import Data.Array.Accelerate.LLVM.Execute.Async

-- libraries
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
  marshalScalarData' :: ScalarData e' -> Par arch (DList (ArgR arch))

-- | Convert function arguments into stream a form suitable for function calls
-- The functions ending in a prime return a DList, other functions return lists.
--
marshalArrays :: forall arch arrs. Marshal arch => ArraysR arrs -> arrs -> Par arch [ArgR arch]
marshalArrays repr arrs = DL.toList <$> marshalArrays' @arch repr arrs

marshalArrays' :: Marshal arch => ArraysR arrs -> arrs -> Par arch (DList (ArgR arch))
marshalArrays' = undefined

marshalArray' :: forall arch sh tp. Marshal arch => ArrayR (Array sh tp) -> Array sh tp -> Par arch (DList (ArgR arch))
marshalArray' (ArrayR _ tp) (Array _ a) = marshalArrayData' @arch tp a

marshalArrayData' :: forall arch tp. Marshal arch => TupleType tp -> ArrayData tp -> Par arch (DList (ArgR arch))
marshalArrayData' TupRunit () = return DL.empty
marshalArrayData' (TupRpair t1 t2) (a1, a2) = do
  l1 <- marshalArrayData' t1 a1
  l2 <- marshalArrayData' t2 a2
  return $ l1 `DL.append` l2
marshalArrayData' (TupRsingle t) ad
  | (_, ScalarDict) <- scalarDict t = marshalScalarData' @arch @tp ad

marshalEnv :: forall arch aenv. Marshal arch => (Gamma aenv, ValR arch aenv) -> Par arch [ArgR arch]
marshalEnv ga = DL.toList <$> marshalEnv' ga

marshalEnv' :: forall arch aenv. Marshal arch => (Gamma aenv, ValR arch aenv) -> Par arch (DList (ArgR arch))
marshalEnv' (gamma, aenv)
    = fmap DL.concat
    $ mapM (\(_, Idx' repr idx) -> marshalArray' @arch repr =<< get (prj idx aenv)) (IM.elems gamma)

marshalShape :: forall arch sh. Marshal arch => ShapeR sh -> sh -> [ArgR arch]
marshalShape shr sh = DL.toList $ marshalShape' @arch shr sh

marshalShape' :: forall arch sh. Marshal arch => ShapeR sh -> sh -> DList (ArgR arch)
marshalShape' ShapeRz () = DL.empty
marshalShape' (ShapeRsnoc shr) (sh, n) = marshalShape' @arch shr sh `DL.snoc` marshalInt @arch n
