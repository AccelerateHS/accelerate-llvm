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

module Data.Array.Accelerate.LLVM.PTX.Execute.Marshal ( module M ) where

-- accelerate
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Execute.Marshal               as M

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim      as Prim

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data

-- cuda
import qualified Foreign.CUDA.Driver                            as CUDA

-- libraries
import qualified Data.DList                                     as DL


instance Marshal PTX where
  type ArgR PTX = CUDA.FunParam

  marshalInt = CUDA.VArg
  marshalScalarData' tp
    | (ScalarDict, _, _) <- singleDict tp
    = liftPar . fmap (DL.singleton . CUDA.VArg) . unsafeGetDevicePtr tp

-- TODO FIXME !!!
--
-- We will probably need to change marshal to be a bracketed function, so that
-- the garbage collector does not try to evict the array in the middle of
-- a computation.
--
unsafeGetDevicePtr
    :: SingleType e
    -> ArrayData e
    -> LLVM PTX (CUDA.DevicePtr (ScalarDataRepr e))
unsafeGetDevicePtr !tp !ad =
  Prim.withDevicePtr tp ad (\p -> return (Nothing, p))

