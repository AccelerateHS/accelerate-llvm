{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Marshal
-- Copyright   : [2014..2018] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Marshal
  where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

-- libraries
import Data.Proxy                                               ( Proxy )
import Data.DList                                               ( DList )
import qualified Data.DList                                     as DL


-- Marshalling arguments
-- ---------------------

-- | Convert function arguments into stream a form suitable for function calls
--
marshal :: Marshalable arch m args => Proxy arch -> args -> m [ArgR arch]
marshal proxy args = DL.toList `fmap` marshal' proxy args


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
  marshal' :: Proxy arch -> a -> m (DList (ArgR arch))

instance Monad m => Marshalable arch m () where
  marshal' _ () = return DL.empty

instance (Marshalable arch m a, Marshalable arch m b) => Marshalable arch m (a, b) where
  marshal' proxy (a, b) =
    DL.concat `fmap` sequence [marshal' proxy a, marshal' proxy b]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c) => Marshalable arch m (a, b, c) where
  marshal' proxy (a, b, c) =
    DL.concat `fmap` sequence [marshal' proxy a, marshal' proxy b, marshal' proxy c]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c, Marshalable arch m d) => Marshalable arch m (a, b, c, d) where
  marshal' proxy (a, b, c, d) =
    DL.concat `fmap` sequence [marshal' proxy a, marshal' proxy b, marshal' proxy c, marshal' proxy d]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c, Marshalable arch m d, Marshalable arch m e)
    => Marshalable arch m (a, b, c, d, e) where
  marshal' proxy (a, b, c, d, e) =
    DL.concat `fmap` sequence [marshal' proxy a, marshal' proxy b, marshal' proxy c, marshal' proxy d, marshal' proxy e]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c, Marshalable arch m d, Marshalable arch m e, Marshalable arch m f)
    => Marshalable arch m (a, b, c, d, e, f) where
  marshal' proxy (a, b, c, d, e, f) =
    DL.concat `fmap` sequence [marshal' proxy a, marshal' proxy b, marshal' proxy c, marshal' proxy d, marshal' proxy e, marshal' proxy f]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c, Marshalable arch m d, Marshalable arch m e, Marshalable arch m f, Marshalable arch m g)
    => Marshalable arch m (a, b, c, d, e, f, g) where
  marshal' proxy (a, b, c, d, e, f, g) =
    DL.concat `fmap` sequence [marshal' proxy a, marshal' proxy b, marshal' proxy c, marshal' proxy d, marshal' proxy e, marshal' proxy f, marshal' proxy g]

instance (Marshalable arch m a, Marshalable arch m b, Marshalable arch m c, Marshalable arch m d, Marshalable arch m e, Marshalable arch m f, Marshalable arch m g, Marshalable arch m h)
    => Marshalable arch m (a, b, c, d, e, f, g, h) where
  marshal' proxy (a, b, c, d, e, f, g, h) =
    DL.concat `fmap` sequence [marshal' proxy a, marshal' proxy b, marshal' proxy c, marshal' proxy d, marshal' proxy e, marshal' proxy f, marshal' proxy g, marshal' proxy h]

instance Marshalable arch m a => Marshalable arch m [a] where
  marshal' proxy = fmap DL.concat . mapM (marshal' proxy)

-- instance Monad m => Marshalable arch m (DList (ArgR arch)) where
--   marshal' _ = return

-- instance (Async arch, Marshalable arch (Par arch) a) => Marshalable arch (Par arch) (FutureR arch a) where
--   marshal' proxy future = marshal' proxy =<< get future

instance (Shape sh, Marshalable arch m Int, Marshalable arch m (ArrayData (EltRepr e)))
    => Marshalable arch m (Array sh e) where
  marshal' proxy (Array sh adata) =
    marshal' proxy (adata, reverse (R.shapeToList sh))

instance Monad m => Marshalable arch m Z where
  marshal' _ Z = return DL.empty

instance (Shape sh, Marshalable arch m sh, Marshalable arch m Int)
    => Marshalable arch m (sh :. Int) where
  marshal' proxy (sh :. sz) =
    DL.concat `fmap` sequence [marshal' proxy sh, marshal' proxy sz]

-- instance Marshalable arch (Gamma aenv, ValR arch aenv) where
--   marshal' (gamma, aenv)
--     = fmap DL.concat
--     $ mapM (\(_, Idx' idx) -> marshal' =<< get (prj idx aenv)) (IM.elems gamma)

