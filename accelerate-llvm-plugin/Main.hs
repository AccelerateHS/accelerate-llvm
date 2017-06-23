-- |
-- Module      : Main
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import GHC.Paths
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  let
      args' = [ "--interactive"
              , "-plugin-package", "accelerate-llvm-plugin"
              , "-fplugin=Data.Array.Accelerate.LLVM.Plugin"
              , "-fplugin-opt=Data.Array.Accelerate.LLVM.Plugin:interactive"
              ]
           ++ args
  --
  callProcess ghc args'

