{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances  #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Marshal
-- Copyright   : [2014..2018] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Marshal (

  Marshalable,
  M.marshal', M.marshal,

) where

-- accelerate
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma, Idx'(..) )
import qualified Data.Array.Accelerate.LLVM.Execute.Marshal     as M

import Data.Array.Accelerate.LLVM.Native.Array.Data
import Data.Array.Accelerate.LLVM.Native.Execute.Async
import Data.Array.Accelerate.LLVM.Native.Execute.Environment
import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.State

-- libraries
import Data.DList                                               ( DList )
import qualified Data.DList                                     as DL
import qualified Data.IntMap                                    as IM
import qualified Foreign.LibFFI                                 as FFI


-- Instances for handling concrete types in the Native backend
--
type Marshalable m args     = M.Marshalable Native m args
type instance M.ArgR Native = FFI.Arg


instance Monad m => M.Marshalable Native m (DList FFI.Arg) where
  marshal' _ = return

instance Monad m => M.Marshalable Native m Int where
  marshal' _ x = return $ DL.singleton (FFI.argInt x)

instance {-# OVERLAPS #-} M.Marshalable Native (Par Native) (Gamma aenv, Val aenv) where
  marshal' proxy (gamma, aenv)
    = fmap DL.concat
    $ mapM (\(_, Idx' idx) -> liftPar . M.marshal' proxy =<< get (prj idx aenv)) (IM.elems gamma)

-- instance M.Marshalable Native (Gamma aenv, Val aenv) where
--   marshal' t s (gamma, aenv)
--     = fmap DL.concat
--     $ mapM (\(_, Idx' idx) -> M.marshal' t s (sync (aprj idx aenv))) (IM.elems gamma)
--     where
--       sync (AsyncR () a) = a

instance ArrayElt e => M.Marshalable Native (Par Native) (ArrayData e) where
  marshal' proxy adata = liftPar (M.marshal' proxy adata)

instance ArrayElt e => M.Marshalable Native (LLVM Native) (ArrayData e) where
  marshal' _ adata = return $ marshalR arrayElt adata
    where
      marshalR :: ArrayEltR e' -> ArrayData e' -> DList FFI.Arg
      marshalR ArrayEltRunit             _  = DL.empty
      marshalR (ArrayEltRpair aeR1 aeR2) ad =
        marshalR aeR1 (fstArrayData ad) `DL.append`
        marshalR aeR2 (sndArrayData ad)
      --
      marshalR (ArrayEltRvec2 ae)  (AD_V2 ad)  = marshalR ae ad
      marshalR (ArrayEltRvec3 ae)  (AD_V3 ad)  = marshalR ae ad
      marshalR (ArrayEltRvec4 ae)  (AD_V4 ad)  = marshalR ae ad
      marshalR (ArrayEltRvec8 ae)  (AD_V8 ad)  = marshalR ae ad
      marshalR (ArrayEltRvec16 ae) (AD_V16 ad) = marshalR ae ad
      --
      marshalR ArrayEltRint     ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint8    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint16   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint32   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint64   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword8   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword16  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword32  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword64  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRhalf    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRfloat   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRdouble  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRchar    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcshort  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcushort ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcint    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcuint   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRclong   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRculong  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcllong  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcullong ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcchar   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcschar  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcuchar  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcfloat  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcdouble ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRbool    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)

