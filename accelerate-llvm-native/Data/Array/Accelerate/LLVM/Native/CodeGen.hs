{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen
  where

-- accelerate
import Data.Array.Accelerate.LLVM.CodeGen

import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Native.CodeGen.Fold
import Data.Array.Accelerate.LLVM.Native.CodeGen.Generate
import Data.Array.Accelerate.LLVM.Native.CodeGen.Map
import Data.Array.Accelerate.LLVM.Native.CodeGen.Transform


instance Skeleton Native where
  map _         = mkMap
  generate _    = mkGenerate
  transform _   = mkTransform
  fold _        = mkFold
  fold1 _       = mkFold1

