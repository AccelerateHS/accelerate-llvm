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
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type

-- libraries
import Data.DList                                               ( DList )
import qualified Data.DList                                     as DL


-- Marshalling arguments
-- ---------------------

-- | Convert function arguments into stream a form suitable for function calls
--
marshal :: forall arch m args. Marshalable arch m args => args -> m [ArgR arch]
marshal args = DL.toList `fmap` marshal' @arch args


-- | A type family that is used to specify a concrete kernel argument and
-- stream/context type for a given backend target.
--
type family ArgR arch


-- | Data which can be marshalled as function arguments to kernels.
--
-- These are just the basic definitions that don't require backend specific
-- knowledge. To complete the definition, a backend must provide instances for:
--
--   * Int                      -- for shapes
--   * ArrayData e              -- for array data
--   * (Gamma aenv, Aval aenv)  -- for free array variables
--
class Monad m => Marshalable arch m a where
  marshal' :: a -> m (DList (ArgR arch))

instance Monad m => Marshalable arch m () where
  marshal' () = return DL.empty

instance (Marshalable arch m a, Marshalable arch m b) => Marshalable arch m (a, b) where
  marshal' (a, b) =
    DL.concat `fmap` sequence [marshal' @arch a, marshal' @arch b]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c) => Marshalable arch m (a, b, c) where
  marshal' (a, b, c) =
    DL.concat `fmap` sequence [marshal' @arch a, marshal' @arch b, marshal' @arch c]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c, Marshalable arch m d) => Marshalable arch m (a, b, c, d) where
  marshal' (a, b, c, d) =
    DL.concat `fmap` sequence [marshal' @arch a, marshal' @arch b, marshal' @arch c, marshal' @arch d]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c, Marshalable arch m d, Marshalable arch m e)
    => Marshalable arch m (a, b, c, d, e) where
  marshal' (a, b, c, d, e) =
    DL.concat `fmap` sequence [marshal' @arch a, marshal' @arch b, marshal' @arch c, marshal' @arch d, marshal' @arch e]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c, Marshalable arch m d, Marshalable arch m e, Marshalable arch m f)
    => Marshalable arch m (a, b, c, d, e, f) where
  marshal' (a, b, c, d, e, f) =
    DL.concat `fmap` sequence [marshal' @arch a, marshal' @arch b, marshal' @arch c, marshal' @arch d, marshal' @arch e, marshal' @arch f]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c, Marshalable arch m d, Marshalable arch m e, Marshalable arch m f, Marshalable arch m g)
    => Marshalable arch m (a, b, c, d, e, f, g) where
  marshal' (a, b, c, d, e, f, g) =
    DL.concat `fmap` sequence [marshal' @arch a, marshal' @arch b, marshal' @arch c, marshal' @arch d, marshal' @arch e, marshal' @arch f, marshal' @arch g]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c, Marshalable arch m d, Marshalable arch m e, Marshalable arch m f, Marshalable arch m g, Marshalable arch m h)
    => Marshalable arch m (a, b, c, d, e, f, g, h) where
  marshal' (a, b, c, d, e, f, g, h) =
    DL.concat `fmap` sequence [marshal' @arch a, marshal' @arch b, marshal' @arch c, marshal' @arch d, marshal' @arch e, marshal' @arch f, marshal' @arch g, marshal' @arch h]

instance Marshalable arch m a => Marshalable arch m [a] where
  marshal' = fmap DL.concat . mapM (marshal' @arch)

instance Marshalable arch m a => Marshalable arch m (Maybe a) where
  marshal' = \case
    Nothing -> return DL.empty
    Just a  -> marshal' @arch a

-- instance Monad m => Marshalable arch m (DList (ArgR arch)) where
--   marshal' = return

-- instance (Async arch, Marshalable arch (Par arch) a) => Marshalable arch (Par arch) (FutureR arch a) where
--   marshal' future = marshal' @arch =<< get future

instance (Shape sh, Marshalable arch m Int, Marshalable arch m (ArrayData (EltRepr e)))
    => Marshalable arch m (Array sh e) where
  marshal' (Array sh adata) =
    DL.concat `fmap` sequence [marshal' @arch adata, go (eltType @sh) sh]
    where
      go :: TupleType a -> a -> m (DList (ArgR arch))
      go TypeRunit         ()       = return DL.empty
      go (TypeRpair ta tb) (sa, sb) = DL.concat `fmap` sequence [go ta sa, go tb sb]
      go (TypeRscalar t)   i
        | SingleScalarType (NumSingleType (IntegralNumType TypeInt{})) <- t = marshal' @arch i
        | otherwise                                                         = $internalError "marshal" "expected Int argument"

instance {-# INCOHERENT #-} (Shape sh, Monad m, Marshalable arch m Int)
    => Marshalable arch m sh where
  marshal' sh = go (eltType @sh) (fromElt sh)
    where
      go :: TupleType a -> a -> m (DList (ArgR arch))
      go TypeRunit         ()       = return DL.empty
      go (TypeRpair ta tb) (sa, sb) = DL.concat `fmap` sequence [go ta sa, go tb sb]
      go (TypeRscalar t)   i
        | SingleScalarType (NumSingleType (IntegralNumType TypeInt{})) <- t = marshal' @arch i
        | otherwise                                                         = $internalError "marshal" "expected Int argument"

-- instance Monad m => Marshalable arch m Z where
--   marshal' Z = return DL.empty

-- instance (Shape sh, Marshalable arch m sh, Marshalable arch m Int)
--     => Marshalable arch m (sh :. Int) where
--   marshal' (sh :. sz) =
--     DL.concat `fmap` sequence [marshal' @arch sh, marshal' @arch sz]

-- instance Marshalable arch (Gamma aenv, ValR arch aenv) where
--   marshal' (gamma, aenv)
--     = fmap DL.concat
--     $ mapM (\(_, Idx' idx) -> marshal' =<< get (prj idx aenv)) (IM.elems gamma)

