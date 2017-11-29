-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Event-boot
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Event (

  Event,
  query, block

) where

import Data.Array.Accelerate.Lifetime
import qualified Foreign.CUDA.Driver.Event                          as Event


type Event = Lifetime Event.Event

query :: Event -> IO Bool
block :: Event -> IO ()

