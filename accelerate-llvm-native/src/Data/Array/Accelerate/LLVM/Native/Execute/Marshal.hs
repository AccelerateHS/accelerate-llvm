{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances  #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Marshal
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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
  marshal' = return

instance Monad m => M.Marshalable Native m Int where
  marshal' x = return $ DL.singleton (FFI.argInt x)

instance {-# OVERLAPS #-} M.Marshalable Native (Par Native) (Gamma aenv, Val aenv) where
  marshal' (gamma, aenv)
    = fmap DL.concat
    $ mapM (\(_, Idx' idx) -> liftPar . M.marshal' @Native =<< get (prj idx aenv)) (IM.elems gamma)

-- instance M.Marshalable Native (Gamma aenv, Val aenv) where
--   marshal' t s (gamma, aenv)
--     = fmap DL.concat
--     $ mapM (\(_, Idx' idx) -> M.marshal' t s (sync (aprj idx aenv))) (IM.elems gamma)
--     where
--       sync (AsyncR () a) = a

instance ArrayElt e => M.Marshalable Native (Par Native) (ArrayData e) where
  marshal' adata = liftPar (M.marshal' @Native adata)

instance ArrayElt e => M.Marshalable Native (LLVM Native) (ArrayData e) where
  marshal' adata = return $ marshalR arrayElt adata
    where
      marshalR :: ArrayEltR e' -> ArrayData e' -> DList FFI.Arg
      marshalR ArrayEltRunit    !_  = DL.empty
      marshalR ArrayEltRint     !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint8    !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint16   !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint32   !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint64   !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword    !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword8   !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword16  !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword32  !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword64  !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRhalf    !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRfloat   !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRdouble  !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRchar    !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRbool    !ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      --
      marshalR (ArrayEltRvec  !ae)         (AD_Vec _ !ad)      = marshalR ae ad
      marshalR (ArrayEltRpair !aeR1 !aeR2) (AD_Pair !ad1 !ad2) = marshalR aeR1 ad1 `DL.append` marshalR aeR2 ad2

