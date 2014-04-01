{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen
  where

-- accelerate
import Data.Array.Accelerate.LLVM.CodeGen

import Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Map
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Transform
import Data.Array.Accelerate.LLVM.PTX.Target


instance Skeleton PTX where
  map           = mkMap
  generate      = mkGenerate
  transform     = mkTransform
  fold          = mkFold
  fold1         = mkFold1

