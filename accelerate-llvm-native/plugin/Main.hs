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

import Data.List
import Data.Maybe
import GHC.Paths
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  let
      interactive = isJust $ find (`elem` ["-i", "--interactive"]) args

      args' = [ "-plugin-package", "accelerate-llvm-native"
              , "-fplugin=Data.Array.Accelerate.LLVM.Native.Plugin"
              ]
           ++ when interactive
              [ "-fplugin-opt=Data.Array.Accelerate.LLVM.Native.Plugin:interactive"
              ]
           ++ args
  --
  callProcess ghc args'

when :: Bool -> [a] -> [a]
when True  x = x
when False _ = []

