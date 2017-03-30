{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Async
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Async (

  Async, Stream, Event,
  module Data.Array.Accelerate.LLVM.Execute.Async,

) where

-- accelerate
import Data.Array.Accelerate.LLVM.Execute.Async                     hiding ( Async )
import qualified Data.Array.Accelerate.LLVM.Execute.Async           as A

import Data.Array.Accelerate.LLVM.Native.Target


type Async a = A.AsyncR  Native a
type Stream  = A.StreamR Native
type Event   = A.EventR  Native

-- The native backend does everything synchronously.
--
instance A.Async Native where
  type StreamR Native = ()
  type EventR  Native = ()

  {-# INLINE fork #-}
  fork = return ()

  {-# INLINE join #-}
  join () = return ()

  {-# INLINE checkpoint #-}
  checkpoint () = return ()

  {-# INLINE after #-}
  after () () = return ()

  {-# INLINE block #-}
  block () = return ()

