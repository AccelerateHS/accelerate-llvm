-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.Execute.Environment
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Multi.Execute.Environment (

  Aval, aprj

) where

import Data.Array.Accelerate.LLVM.Multi.Target
import Data.Array.Accelerate.LLVM.Execute.Environment

type Aval = AvalR Multi

