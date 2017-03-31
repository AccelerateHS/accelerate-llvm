-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Stream-boot
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Stream (

  Stream,
  streaming,

) where

import Data.Array.Accelerate.Lifetime                               ( Lifetime )
import Data.Array.Accelerate.LLVM.State                             ( LLVM )
import Data.Array.Accelerate.LLVM.PTX.Target                        ( PTX )
import {-# SOURCE #-} Data.Array.Accelerate.LLVM.PTX.Execute.Event

import qualified Foreign.CUDA.Driver.Stream                         as Stream


type Stream = Lifetime Stream.Stream

streaming
  :: (Stream -> LLVM PTX a)
  -> (Event -> a -> LLVM PTX b)
  -> LLVM PTX b

