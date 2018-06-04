{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen
-- Copyright   : [2014..2017] Trevor L. McDonell
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
import Data.Array.Accelerate.LLVM.PTX.CodeGen.FoldSeg
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Intrinsic ()
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Map
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Permute
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Scan
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Stencil
import Data.Array.Accelerate.LLVM.PTX.Target


instance Skeleton PTX where
  map ptx _       = mkMap ptx
  generate ptx _  = mkGenerate ptx
  fold ptx _      = mkFold ptx
  fold1 ptx _     = mkFold1 ptx
  foldSeg ptx _   = mkFoldSeg ptx
  fold1Seg ptx _  = mkFold1Seg ptx
  scanl ptx _     = mkScanl ptx
  scanl1 ptx _    = mkScanl1 ptx
  scanl' ptx _    = mkScanl' ptx
  scanr ptx _     = mkScanr ptx
  scanr1 ptx _    = mkScanr1 ptx
  scanr' ptx _    = mkScanr' ptx
  permute ptx _   = mkPermute ptx
  stencil1 ptx _  = mkStencil1 ptx
  stencil2 ptx _  = mkStencil2 ptx

