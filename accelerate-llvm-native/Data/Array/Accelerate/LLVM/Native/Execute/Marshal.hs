{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Marshal
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Marshal (

  Marshalable, marshal

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma, Idx'(..) )

import Data.Array.Accelerate.LLVM.Native.Array.Data
import Data.Array.Accelerate.LLVM.Native.Execute.Environment

-- libraries
import Data.DList                                               ( DList )
import qualified Data.DList                                     as DL
import qualified Data.IntMap                                    as IM
import qualified Foreign.LibFFI                                 as FFI


-- Marshalling data
-- ----------------

-- | Convert function arguments into a form suitable for libFFI function calls.
--
marshal :: Marshalable args => args -> [FFI.Arg]
marshal = DL.toList . marshal'


-- Data which can be marshalled as function arguments to a kernel invocation.
--
class Marshalable a where
  marshal' :: a -> DList FFI.Arg

instance Marshalable () where
  marshal' () = DL.empty

instance ArrayElt e => Marshalable (ArrayData e) where
  marshal' adata = marshalR arrayElt adata
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

-- instance Shape sh => Marshalable sh where
--   marshal' sh = map FFI.argInt (reverse (shapeToList sh))

instance Marshalable (Gamma aenv, Aval aenv) where              -- overlaps with instance (a,b)
  marshal' (gamma, aenv)
    = DL.concat
    $ map (\(_, Idx' idx) -> marshal' (aprj idx aenv)) (IM.elems gamma)

instance (Shape sh, Elt e) => Marshalable (Array sh e) where
  marshal' (Array sh adata) = marshal' adata `DL.append`
                              marshal' (reverse (R.shapeToList sh))

instance (Marshalable a, Marshalable b) => Marshalable (a, b) where
  marshal' (a, b) = marshal' a `DL.append` marshal' b

instance (Marshalable a, Marshalable b, Marshalable c) => Marshalable (a, b, c) where
  marshal' (a, b, c)
    = DL.concat [marshal' a, marshal' b, marshal' c]

instance (Marshalable a, Marshalable b, Marshalable c, Marshalable d) => Marshalable (a, b, c, d) where
  marshal' (a, b, c, d)
    = DL.concat [marshal' a, marshal' b, marshal' c, marshal' d]

instance (Marshalable a, Marshalable b, Marshalable c, Marshalable d, Marshalable e)
    => Marshalable (a, b, c, d, e) where
  marshal' (a, b, c, d, e)
    = DL.concat [marshal' a, marshal' b, marshal' c, marshal' d, marshal' e]

instance (Marshalable a, Marshalable b, Marshalable c, Marshalable d, Marshalable e, Marshalable f)
    => Marshalable (a, b, c, d, e, f) where
  marshal' (a, b, c, d, e, f)
    = DL.concat [marshal' a, marshal' b, marshal' c, marshal' d, marshal' e, marshal' f]

instance Marshalable Int where
  marshal' x = DL.singleton (FFI.argInt x)

instance Marshalable a => Marshalable [a] where
  marshal' = DL.concat . map marshal'

