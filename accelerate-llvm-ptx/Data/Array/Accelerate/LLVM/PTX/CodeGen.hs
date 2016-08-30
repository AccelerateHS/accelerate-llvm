{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen (

  KernelMetadata(..),

) where

-- accelerate
import Data.Array.Accelerate.LLVM.CodeGen

import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Fold
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Scan
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Map
-- import Data.Array.Accelerate.LLVM.PTX.CodeGen.Transform
import Data.Array.Accelerate.LLVM.PTX.Target


instance Skeleton PTX where
  map           = mkMap
  generate      = mkGenerate
--  transform     = mkTransform
  fold          = mkFold
  fold1         = mkFold1
  scanl         = mkScan

instance Expression PTX

