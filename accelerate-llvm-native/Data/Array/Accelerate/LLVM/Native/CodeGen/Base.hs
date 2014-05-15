{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Base
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Base
  where

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.Global

-- accelerate
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type


-- | Generate function parameters that will specify the first and last (linear)
-- index of the array this thread should evaluate.
--
gangParam :: (Operand, Operand, [Parameter])
gangParam =
  let t         = typeOf (scalarType :: ScalarType Int)
      start     = "ix.start"
      end       = "ix.end"
  in
  (local start, local end, [ Parameter t start [], Parameter t end [] ] )

-- | The thread ID of a gang worker
--
gangId :: (Operand, [Parameter])
gangId =
  let t         = typeOf (scalarType :: ScalarType Int)
      thread    = "ix.tid"
  in
  (local thread, [Parameter t thread []] )


-- | Make a complete kernel function using a quasi quoter
--
makeKernelQ :: Name -> CodeGen Global -> CodeGen [Kernel t aenv a]
makeKernelQ n qq = do
  fun <- qq
  return [ Kernel fun { name = n } ]


-- | Create a complete kernel function by running the code generation process
-- specified in the final parameter.
--
makeKernel :: Name -> [Parameter] -> CodeGen () -> CodeGen [Kernel t aenv a]
makeKernel kernel param body = do
  code <- body >> createBlocks
  return [ Kernel $ functionDefaults
             { returnType  = VoidType
             , name        = kernel
             , parameters  = (param, False)
             , basicBlocks = code
             } ]

