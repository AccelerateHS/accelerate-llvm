{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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
import Data.Array.Accelerate.LLVM.PTX.CodeGen.FoldSeg
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Intrinsic ()
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Map
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Permute
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Scan
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Stencil
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Transform
import Data.Array.Accelerate.LLVM.PTX.Target


instance Skeleton PTX where
  map _       = mkMap
  generate _  = mkGenerate
  transform _ = mkTransform
  fold _      = mkFold
  fold1 _     = mkFold1
  foldSeg _   = mkFoldSeg
  fold1Seg _  = mkFold1Seg
  scanl _     = mkScanl
  scanl1 _    = mkScanl1
  scanl' _    = mkScanl'
  scanr _     = mkScanr
  scanr1 _    = mkScanr1
  scanr' _    = mkScanr'
  permute _   = mkPermute
  stencil1 _  = mkStencil1
  stencil2 _  = mkStencil2

