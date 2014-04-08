{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Async
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Async (

  Async, Stream,
  A.wait, A.after, A.streaming, A.async,

) where

-- accelerate
import qualified Data.Array.Accelerate.LLVM.Execute.Async       as A
import Data.Array.Accelerate.LLVM.Native.Target


type Async a = A.AsyncR Native a
type Stream  = A.StreamR Native

-- The native backend does everything synchronously.
--
instance A.Async Native where
  type AsyncR Native a = a
  type StreamR Native  = ()

  {-# INLINE wait #-}
  wait a = return a

  {-# INLINE after #-}
  after () a = return a

  {-# INLINE streaming #-}
  streaming f g = g =<< f ()

  {-# INLINE async #-}
  async () a = a

