-- |
-- Module      : nofib-llvm-ptx
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import Data.Array.Accelerate.Test.NoFib
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.Debug

main :: IO ()
main = do
  beginMonitoring
  nofib runN

