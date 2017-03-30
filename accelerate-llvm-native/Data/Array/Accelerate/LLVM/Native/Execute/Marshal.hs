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
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Marshal (

  Marshalable, M.marshal

) where

-- accelerate
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma, Idx'(..) )
import qualified Data.Array.Accelerate.LLVM.Execute.Marshal     as M

import Data.Array.Accelerate.LLVM.Native.Array.Data
import Data.Array.Accelerate.LLVM.Native.Execute.Async
import Data.Array.Accelerate.LLVM.Native.Execute.Environment
import Data.Array.Accelerate.LLVM.Native.Target

-- libraries
import Data.DList                                               ( DList )
import qualified Data.DList                                     as DL
import qualified Data.IntMap                                    as IM
import qualified Foreign.LibFFI                                 as FFI


-- Instances for the Native backend
--
type Marshalable args       = M.Marshalable Native args
type instance M.ArgR Native = FFI.Arg


-- Instances for handling concrete types in this backend, namely shapes and
-- array data.
--
instance M.Marshalable Native Int where
  marshal' _ _ x = return $ DL.singleton (FFI.argInt x)

instance {-# OVERLAPS #-} M.Marshalable Native (Gamma aenv, Aval aenv) where
  marshal' t s (gamma, aenv)
    = fmap DL.concat
    $ mapM (\(_, Idx' idx) -> M.marshal' t s (sync (aprj idx aenv))) (IM.elems gamma)
    where
      sync (AsyncR () a) = a

instance ArrayElt e => M.Marshalable Native (ArrayData e) where
  marshal' _ _ adata = return $ marshalR arrayElt adata
    where
      marshalR :: ArrayEltR e' -> ArrayData e' -> DList FFI.Arg
      marshalR ArrayEltRunit             _  = DL.empty
      marshalR (ArrayEltRpair aeR1 aeR2) ad =
        marshalR aeR1 (fstArrayData ad) `DL.append`
        marshalR aeR2 (sndArrayData ad)
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

