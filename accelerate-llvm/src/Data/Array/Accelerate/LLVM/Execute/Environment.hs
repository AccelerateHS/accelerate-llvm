{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Environment
-- Copyright   : [2014..2018] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Environment (

  module Data.Array.Accelerate.LLVM.Environment,
  module Data.Array.Accelerate.LLVM.Execute.Environment,

) where

import Data.Array.Accelerate.LLVM.Execute.Async
import Data.Array.Accelerate.LLVM.Environment                       hiding ( ValR )
import qualified Data.Array.Accelerate.LLVM.Environment             as E


-- Environments
-- ------------

-- Valuation for an environment of futures
--
type ValR arch = E.ValR (FutureR arch)

