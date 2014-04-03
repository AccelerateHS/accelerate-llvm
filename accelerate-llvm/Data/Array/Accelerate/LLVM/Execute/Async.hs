{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Async
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Async
  where

import Data.Array.Accelerate.LLVM.State


-- Asynchronous operations
-- -----------------------

class Async arch where
  type AsyncR arch a    -- ^ An asynchronous array
  type StreamR arch     -- ^ Streams can execute concurrently with other streams

  -- | Wait for an asynchronous operation to complete
  --
  wait      :: AsyncR arch a -> LLVM arch a

  -- | Mark the given array as being available to the execution stream only
  -- after all preceding operations have completed.
  --
  after     :: StreamR arch -> AsyncR arch a -> LLVM arch a

  -- | Execute a computation in a new stream and make that available
  -- asynchronously in a consumer operation.
  --
  streaming :: (StreamR arch  -> LLVM arch a)
            -> (AsyncR arch a -> LLVM arch b)
            -> LLVM arch b

  -- | Execute the given operation asynchronously
  --
  async     :: StreamR arch -> LLVM arch a -> LLVM arch (AsyncR arch a)

