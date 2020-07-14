{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Base
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Base
  where

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache
import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Name
import qualified LLVM.AST.Global                                    as LLVM
import qualified LLVM.AST.Type                                      as LLVM

import Control.Monad
import Data.Monoid
import Data.String
import Text.Printf
import Prelude                                                      as P


-- | Generate function parameters that will specify the first and last (linear)
-- index of the array this thread should evaluate.
--
gangParam :: ShapeR sh -> (Operands sh, Operands sh, [LLVM.Parameter])
gangParam shr =
  let start = "ix.start"
      end   = "ix.end"
      tp    = shapeType shr
  in
  (local tp start, local tp end, parameter tp start ++ parameter tp end)


-- | The worker ID of the calling thread
--
gangId :: (Operands Int, [LLVM.Parameter])
gangId =
  let tid = "ix.tid"
  in (local (TupRsingle scalarTypeInt) tid, [ scalarParameter scalarType tid ] )


-- Global function definitions
-- ---------------------------

data instance KernelMetadata Native = KM_Native ()

-- | Combine kernels into a single program
--
(+++) :: IROpenAcc Native aenv a -> IROpenAcc Native aenv a -> IROpenAcc Native aenv a
IROpenAcc k1 +++ IROpenAcc k2 = IROpenAcc (k1 ++ k2)

-- | Create a single kernel program
--
makeOpenAcc :: UID -> Label -> [LLVM.Parameter] -> CodeGen Native () -> CodeGen Native (IROpenAcc Native aenv a)
makeOpenAcc uid name param kernel = do
  body  <- makeKernel (name <> fromString (printf "_%s" (show uid))) param kernel
  return $ IROpenAcc [body]

-- | Create a complete kernel function by running the code generation process
-- specified in the final parameter.
--
makeKernel :: Label -> [LLVM.Parameter] -> CodeGen Native () -> CodeGen Native (Kernel Native aenv a)
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

