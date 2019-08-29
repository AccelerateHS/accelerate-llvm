{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances  #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Marshal
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Marshal (

  Marshalable,
  M.marshal, M.marshal',

) where

-- accelerate
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma, Idx'(..) )
import qualified Data.Array.Accelerate.LLVM.Execute.Marshal     as M

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import Data.Array.Accelerate.LLVM.PTX.Execute.Environment       ( Val, prj )
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim      as Prim

-- cuda
import qualified Foreign.CUDA.Driver                            as CUDA

-- libraries
import Control.Monad
import Data.DList                                               ( DList )
import Data.Int
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable                                         ( Storable )
import qualified Data.DList                                     as DL
import qualified Data.IntMap                                    as IM


-- Instances for handling concrete types in the PTX backend
--
type Marshalable m args   = M.Marshalable PTX m args
type instance M.ArgR PTX  = CUDA.FunParam


instance Monad m => M.Marshalable PTX m (DList CUDA.FunParam) where
  marshal' = return

instance Monad m => M.Marshalable PTX m Int where
  marshal' x = return $ DL.singleton (CUDA.VArg x)

instance Monad m => M.Marshalable PTX m Int32 where
  marshal' x = return $ DL.singleton (CUDA.VArg x)

instance {-# OVERLAPS #-} M.Marshalable PTX (Par PTX) (Gamma aenv, Val aenv) where
  marshal' (gamma, aenv)
    = fmap DL.concat
    $ mapM (\(_, Idx' idx) -> liftPar . M.marshal' @PTX =<< get (prj idx aenv)) (IM.elems gamma)

instance (M.Marshalable PTX (Par PTX) a) => M.Marshalable PTX (Par PTX) (Future a) where
  marshal' future = M.marshal' @PTX =<< get future

instance ArrayElt e => M.Marshalable PTX (Par PTX) (ArrayData e) where
  marshal' adata = liftPar (M.marshal' @PTX adata)

instance ArrayElt e => M.Marshalable PTX (LLVM PTX) (ArrayData e) where
  marshal' adata = go arrayElt adata
    where
      wrap :: forall e' a. (ArrayElt e', ArrayPtrs e' ~ Ptr a, Typeable e', Typeable a, Storable a)
           => ArrayData e'
           -> LLVM PTX (DList CUDA.FunParam)
      wrap ad = fmap (DL.singleton . CUDA.VArg) (unsafeGetDevicePtr ad)

      go :: ArrayEltR e' -> ArrayData e' -> LLVM PTX (DList CUDA.FunParam)
      go ArrayEltRunit    !_  = return DL.empty
      go ArrayEltRint     !ad = wrap ad
      go ArrayEltRint8    !ad = wrap ad
      go ArrayEltRint16   !ad = wrap ad
      go ArrayEltRint32   !ad = wrap ad
      go ArrayEltRint64   !ad = wrap ad
      go ArrayEltRword    !ad = wrap ad
      go ArrayEltRword8   !ad = wrap ad
      go ArrayEltRword16  !ad = wrap ad
      go ArrayEltRword32  !ad = wrap ad
      go ArrayEltRword64  !ad = wrap ad
      go ArrayEltRhalf    !ad = wrap ad
      go ArrayEltRfloat   !ad = wrap ad
      go ArrayEltRdouble  !ad = wrap ad
      go ArrayEltRchar    !ad = wrap ad
      go ArrayEltRbool    !ad = wrap ad
      --
      go (ArrayEltRvec  !aeR)        (AD_Vec _ !ad)      = go aeR ad
      go (ArrayEltRpair !aeR1 !aeR2) (AD_Pair !ad1 !ad2) = return DL.append `ap` go aeR1 ad1 `ap` go aeR2 ad2


-- TODO FIXME !!!
--
-- We will probably need to change marshal to be a bracketed function, so that
-- the garbage collector does not try to evict the array in the middle of
-- a computation.
--
unsafeGetDevicePtr
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a, Storable a)
    => ArrayData e
    -> LLVM PTX (CUDA.DevicePtr a)
unsafeGetDevicePtr !ad =
  Prim.withDevicePtr ad (\p -> return (Nothing,p))

