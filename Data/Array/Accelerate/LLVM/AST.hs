{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.AST
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.AST
  where

-- llvm-general
import LLVM.General.Module                              ( Module )

-- accelerate
import Data.Array.Accelerate.AST                        ( PreOpenAcc, PreAfun, PreExp, PreOpenExp, PreOpenFun )
import Data.Array.Accelerate.Array.Sugar                ( Array, Shape, Elt )

import Data.Array.Accelerate.LLVM.CodeGen.Environment


-- | Annotate an open array expression with the information necessary to execute
-- each node directly.
--
data ExecOpenAcc aenv a where
  ExecAcc  :: Module
           -> Aval aenv
           -> PreOpenAcc ExecOpenAcc aenv a
           -> ExecOpenAcc aenv a

  EmbedAcc :: (Shape sh, Elt e)
           => PreExp ExecOpenAcc aenv sh
           -> ExecOpenAcc aenv (Array sh e)


-- An annotated AST suitable for execution
--
type ExecAcc a   = ExecOpenAcc () a
type ExecAfun a  = PreAfun ExecOpenAcc a

type ExecOpenExp = PreOpenExp ExecOpenAcc
type ExecOpenFun = PreOpenFun ExecOpenAcc

type ExecExp     = ExecOpenExp ()
type ExecFun     = ExecOpenFun ()

