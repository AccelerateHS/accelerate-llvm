{- |
Module      : Text.LLVM.Triple
Description : AST, parsing, and printing of LLVM target triples.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental
-}

module Text.LLVM.Triple
  ( module Text.LLVM.Triple.AST
  , module Text.LLVM.Triple.Parse
  , module Text.LLVM.Triple.Print
  ) where

import Text.LLVM.Triple.AST
import Text.LLVM.Triple.Parse
import Text.LLVM.Triple.Print
