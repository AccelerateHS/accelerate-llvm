{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Marshal
-- Copyright   : [2014..2017] Trevor L. McDonell
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

import Data.Array.Accelerate.LLVM.Execute.Async

-- libraries
import Data.DList                                               ( DList )
import qualified Data.DList                                     as DL


-- Marshalling arguments
-- ---------------------

-- | Convert function arguments into stream a form suitable for CUDA function calls
--
marshal :: Marshalable t args => t -> StreamR t -> args -> IO [ArgR t]
marshal target stream args = DL.toList `fmap` marshal' target stream args


-- | A type family that is used to specify a concrete kernel argument and
-- stream/context type for a given backend target.
--
type family ArgR target


-- | Data which can be marshalled as function arguments to kernels.
--
-- These are just the basic definitions that don't require backend specific
-- knowledge. To complete the definition, a backend must provide instances for:
--
--   * Int                      -- for shapes
--   * ArrayData e              -- for array data
--   * (Gamma aenv, Aval aenv)  -- for free array variables
--
class Marshalable t a where
  marshal' :: t -> StreamR t -> a -> IO (DList (ArgR t))

instance Marshalable t () where
  marshal' _ _ () = return DL.empty

instance (Marshalable t a, Marshalable t b) => Marshalable t (a, b) where
  marshal' t s (a, b) =
    DL.concat `fmap` sequence [marshal' t s a, marshal' t s b]

instance (Marshalable t a, Marshalable t b, Marshalable t c) => Marshalable t (a, b, c) where
  marshal' t s (a, b, c) =
    DL.concat `fmap` sequence [marshal' t s a, marshal' t s b, marshal' t s c]

instance (Marshalable t a, Marshalable t b, Marshalable t c, Marshalable t d) => Marshalable t (a, b, c, d) where
  marshal' t s (a, b, c, d) =
    DL.concat `fmap` sequence [marshal' t s a, marshal' t s b, marshal' t s c, marshal' t s d]

instance (Marshalable t a, Marshalable t b, Marshalable t c, Marshalable t d, Marshalable t e)
    => Marshalable t (a, b, c, d, e) where
  marshal' t s (a, b, c, d, e) =
    DL.concat `fmap` sequence [marshal' t s a, marshal' t s b, marshal' t s c, marshal' t s d, marshal' t s e]

instance (Marshalable t a, Marshalable t b, Marshalable t c, Marshalable t d, Marshalable t e, Marshalable t f)
    => Marshalable t (a, b, c, d, e, f) where
  marshal' t s (a, b, c, d, e, f) =
    DL.concat `fmap` sequence [marshal' t s a, marshal' t s b, marshal' t s c, marshal' t s d, marshal' t s e, marshal' t s f]

instance (Marshalable t a, Marshalable t b, Marshalable t c, Marshalable t d, Marshalable t e, Marshalable t f, Marshalable t g)
    => Marshalable t (a, b, c, d, e, f, g) where
  marshal' t s (a, b, c, d, e, f, g) =
    DL.concat `fmap` sequence [marshal' t s a, marshal' t s b, marshal' t s c, marshal' t s d, marshal' t s e, marshal' t s f, marshal' t s g]

instance (Marshalable t a, Marshalable t b, Marshalable t c, Marshalable t d, Marshalable t e, Marshalable t f, Marshalable t g, Marshalable t h)
    => Marshalable t (a, b, c, d, e, f, g, h) where
  marshal' t s (a, b, c, d, e, f, g, h) =
    DL.concat `fmap` sequence [marshal' t s a, marshal' t s b, marshal' t s c, marshal' t s d, marshal' t s e, marshal' t s f, marshal' t s g, marshal' t s h]

instance Marshalable t a => Marshalable t [a] where
  marshal' t s = fmap DL.concat . mapM (marshal' t s)

instance (Shape sh, Marshalable t Int, Marshalable t (ArrayData (EltRepr e)))
    => Marshalable t (Array sh e) where
  marshal' t s (Array sh adata) =
    marshal' t s (adata, reverse (R.shapeToList sh))

