{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen (

  KernelMetadata(..),

) where

-- accelerate
import Data.Array.Accelerate.LLVM.CodeGen

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Fold
import Data.Array.Accelerate.LLVM.Native.CodeGen.FoldSeg
import Data.Array.Accelerate.LLVM.Native.CodeGen.Generate
import Data.Array.Accelerate.LLVM.Native.CodeGen.Map
import Data.Array.Accelerate.LLVM.Native.CodeGen.Permute
import Data.Array.Accelerate.LLVM.Native.CodeGen.Scan
import Data.Array.Accelerate.LLVM.Native.CodeGen.Stencil
import Data.Array.Accelerate.LLVM.Native.CodeGen.Transform
import Data.Array.Accelerate.LLVM.Native.Target


instance Skeleton Native where
  map         = mkMap
  generate    = mkGenerate
  transform   = mkTransform
  fold        = mkFold
  fold1       = mkFold1
  foldSeg     = mkFoldSeg
  fold1Seg    = mkFold1Seg
  scanl       = mkScanl
  scanl1      = mkScanl1
  scanl'      = mkScanl'
  scanr       = mkScanr
  scanr1      = mkScanr1
  scanr'      = mkScanr'
  permute     = mkPermute
  stencil1    = mkStencil1
  stencil2    = mkStencil2

