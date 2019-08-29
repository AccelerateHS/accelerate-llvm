{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.Compile
-- Copyright   : [2014..2015] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Multi.Compile (

  ExecutableR(..),
  module Data.Array.Accelerate.LLVM.Compile,

) where

-- accelerate
import Data.Array.Accelerate.Trafo                              ( DelayedOpenAcc )
import Data.Array.Accelerate.LLVM.State                         ( LLVM )
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.Compile

import Data.Array.Accelerate.LLVM.Multi.Target
import Data.Array.Accelerate.LLVM.PTX.Internal
import Data.Array.Accelerate.LLVM.Native.Internal


-- standard library
import Control.Applicative


instance Compile Multi where
  data ExecutableR Multi = MultiR {
          ptxExecutable    :: ExecutableR PTX
        , nativeExecutable :: ExecutableR Native
        }
  --
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

