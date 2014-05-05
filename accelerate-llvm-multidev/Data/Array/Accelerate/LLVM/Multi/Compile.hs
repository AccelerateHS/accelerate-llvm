{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.Compile
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Multi.Compile (

  module Data.Array.Accelerate.LLVM.Compile

) where

-- accelerate
import Data.Array.Accelerate.Trafo                              ( DelayedOpenAcc )
import Data.Array.Accelerate.LLVM.State                         ( LLVM )
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.Compile

import Data.Array.Accelerate.LLVM.Target
import Data.Array.Accelerate.LLVM.Multi.Target
import Data.Array.Accelerate.LLVM.Native                        ()
import Data.Array.Accelerate.LLVM.PTX                           ()

-- standard library
import Control.Applicative


instance Compile Multi where
  compileForTarget = compileForMulti


-- | Compile a given Accelerate array computation for all target backends
--
compileForMulti
    :: DelayedOpenAcc aenv a
    -> Gamma aenv
    -> LLVM Multi (ExecutableR Multi)
compileForMulti acc aenv =
  MultiR <$> compileForTarget acc aenv `with` ptxTarget
         <*> compileForTarget acc aenv `with` nativeTarget

