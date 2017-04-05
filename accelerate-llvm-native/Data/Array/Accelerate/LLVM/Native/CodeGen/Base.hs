{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Base
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Base
  where

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Downcast
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )

import LLVM.AST.Type.Name
import qualified LLVM.AST.Global                                    as LLVM
import qualified LLVM.AST.Type                                      as LLVM


-- | Generate function parameters that will specify the first and last (linear)
-- index of the array this thread should evaluate.
--
gangParam :: (IR Int, IR Int, [LLVM.Parameter])
gangParam =
  let t         = scalarType
      start     = "ix.start"
      end       = "ix.end"
  in
  (local t start, local t end, [ scalarParameter t start, scalarParameter t end ] )


-- | The thread ID of a gang worker
--
gangId :: (IR Int, [LLVM.Parameter])
gangId =
  let t         = scalarType
      tid       = "ix.tid"
  in
  (local t tid, [ scalarParameter t tid ] )


-- Global function definitions
-- ---------------------------

data instance KernelMetadata Native = KM_Native ()

-- | Combine kernels into a single program
--
(+++) :: IROpenAcc Native aenv a -> IROpenAcc Native aenv a -> IROpenAcc Native aenv a
IROpenAcc k1 +++ IROpenAcc k2 = IROpenAcc (k1 ++ k2)

-- | Create a single kernel program
--
makeOpenAcc :: Label -> [LLVM.Parameter] -> CodeGen () -> CodeGen (IROpenAcc Native aenv a)
makeOpenAcc name param kernel = do
  body  <- makeKernel name param kernel
  return $ IROpenAcc [body]

-- | Create a complete kernel function by running the code generation process
-- specified in the final parameter.
--
makeKernel :: Label -> [LLVM.Parameter] -> CodeGen () -> CodeGen (Kernel Native aenv a)
makeKernel name param kernel = do
  _    <- kernel
  code <- createBlocks
  return $ Kernel
    { kernelMetadata = KM_Native ()
    , unKernel       = LLVM.functionDefaults
                     { LLVM.returnType  = LLVM.VoidType
                     , LLVM.name        = downcast name
                     , LLVM.parameters  = (param, False)
                     , LLVM.basicBlocks = code
                     }
    }

