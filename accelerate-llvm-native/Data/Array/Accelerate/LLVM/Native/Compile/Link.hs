{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile.Link
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile.Link
  where

-- llvm-hs
import LLVM.AST
import LLVM.AST.Global
import LLVM.ExecutionEngine

-- accelerate
import Data.Array.Accelerate.Error

-- standard library
import Data.Maybe


-- | Return function pointers to all of the global function definitions in the
-- given executable module.
--
getGlobalFunctions
    :: ExecutionEngine e f
    => Module
    -> ExecutableModule e
    -> IO [(String, f)]
getGlobalFunctions ast exe
  = mapM (\f -> (f,) `fmap` link f)
  $ globalFunctions (moduleDefinitions ast)
  where
    link f = fromMaybe ($internalError "link" "function not found") `fmap` getFunction exe (Name f)


-- | Extract the names of the function definitions from a module
--
-- TLM: move this somewhere it can be shared between Native/NVVM backend
--
globalFunctions :: [Definition] -> [String]
globalFunctions defs =
  [ n | GlobalDefinition Function{..} <- defs
      , not (null basicBlocks)
      , let Name n = name
      ]

