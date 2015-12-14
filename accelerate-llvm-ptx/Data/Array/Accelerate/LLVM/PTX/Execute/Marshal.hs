{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Marshal
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Marshal (

  Marshalable, M.marshal

) where

-- accelerate
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma, Idx'(..) )
import qualified Data.Array.Accelerate.LLVM.Execute.Marshal     as M

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Array.Data
import Data.Array.Accelerate.LLVM.PTX.Execute.Async             ( Async(..) )
import Data.Array.Accelerate.LLVM.PTX.Execute.Event             ( after )
import Data.Array.Accelerate.LLVM.PTX.Execute.Environment
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim      as Prim

-- cuda
import qualified Foreign.CUDA.Driver                            as CUDA

-- libraries
import Control.Monad
import Data.Int
import Data.DList                                               ( DList )
import Data.Typeable
import Foreign.Ptr
import qualified Data.DList                                     as DL
import qualified Data.IntMap                                    as IM


-- Instances for the PTX backend
--
type Marshalable args       = M.Marshalable PTX args
type instance M.ArgR PTX    = CUDA.FunParam


-- Instances for handling concrete types in this backend, namely shapes and
-- array data.
--
instance M.Marshalable PTX Int where
  marshal' _ _ x = return $ DL.singleton (CUDA.VArg x)

instance M.Marshalable PTX Int32 where
  marshal' _ _ x = return $ DL.singleton (CUDA.VArg x)

instance M.Marshalable PTX (Gamma aenv, Aval aenv) where        -- overlaps with instance (a,b)
  marshal' ptx stream (gamma, aenv)
    = fmap DL.concat
    $ mapM (\(_, Idx' idx) -> M.marshal' ptx stream =<< sync (aprj idx aenv)) (IM.elems gamma)
    where
      -- HAXORZ~ D:
      --
      -- The 'Async' class functions need to run in the LLVM monad because the
      -- continuations of the 'streaming' function run under the full LLVM
      -- monad. But the marshalling functions must run in IO because they will
      -- be executed in the lower-level scheduling code.
      --
      -- We hack around this impedance mismatch by calling the 'after'
      -- implementation directly.
      --
      sync :: Async a -> IO a
      sync (Async event arr) = after event stream >> return arr

instance ArrayElt e => M.Marshalable PTX (ArrayData e) where
  marshal' PTX{..} _ adata = do
    let marshalP :: forall e' a. (ArrayElt e', ArrayPtrs e' ~ Ptr a, Typeable a)
                 => ArrayData e'
                 -> IO (DList CUDA.FunParam)
        marshalP ad =
          fmap (DL.singleton . CUDA.VArg)
               (Prim.devicePtr ptxMemoryTable ad :: IO (CUDA.DevicePtr a))

        marshalR :: ArrayEltR e' -> ArrayData e' -> IO (DList CUDA.FunParam)
        marshalR ArrayEltRunit             _  = return DL.empty
        marshalR (ArrayEltRpair aeR1 aeR2) ad =
          return DL.append `ap` marshalR aeR1 (fstArrayData ad)
                           `ap` marshalR aeR2 (sndArrayData ad)
        marshalR ArrayEltRint     ad = marshalP ad
        marshalR ArrayEltRint8    ad = marshalP ad
        marshalR ArrayEltRint16   ad = marshalP ad
        marshalR ArrayEltRint32   ad = marshalP ad
        marshalR ArrayEltRint64   ad = marshalP ad
        marshalR ArrayEltRword    ad = marshalP ad
        marshalR ArrayEltRword8   ad = marshalP ad
        marshalR ArrayEltRword16  ad = marshalP ad
        marshalR ArrayEltRword32  ad = marshalP ad
        marshalR ArrayEltRword64  ad = marshalP ad
        marshalR ArrayEltRfloat   ad = marshalP ad
        marshalR ArrayEltRdouble  ad = marshalP ad
        marshalR ArrayEltRchar    ad = marshalP ad
        marshalR ArrayEltRcshort  ad = marshalP ad
        marshalR ArrayEltRcushort ad = marshalP ad
        marshalR ArrayEltRcint    ad = marshalP ad
        marshalR ArrayEltRcuint   ad = marshalP ad
        marshalR ArrayEltRclong   ad = marshalP ad
        marshalR ArrayEltRculong  ad = marshalP ad
        marshalR ArrayEltRcllong  ad = marshalP ad
        marshalR ArrayEltRcullong ad = marshalP ad
        marshalR ArrayEltRcchar   ad = marshalP ad
        marshalR ArrayEltRcschar  ad = marshalP ad
        marshalR ArrayEltRcuchar  ad = marshalP ad
        marshalR ArrayEltRcfloat  ad = marshalP ad
        marshalR ArrayEltRcdouble ad = marshalP ad
        marshalR ArrayEltRbool    ad = marshalP ad

    marshalR arrayElt adata

