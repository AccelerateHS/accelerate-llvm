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

-- accelerate
import Data.Array.Accelerate.AST                        ( PreOpenAcc, PreAfun, PreExp, PreOpenExp, PreOpenFun )
import Data.Array.Accelerate.Array.Sugar                ( Array, Shape, Elt )

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.Target


-- | Annotate an open array expression with the information necessary to execute
-- each node directly.
--
data ExecOpenAcc arch aenv a where
  ExecAcc  :: Target arch
           => ExecutableR arch
           -> Gamma aenv
           -> PreOpenAcc (ExecOpenAcc arch) aenv a
           -> ExecOpenAcc arch aenv a

  EmbedAcc :: (Shape sh, Elt e)
           => PreExp (ExecOpenAcc arch) aenv sh
           -> ExecOpenAcc arch aenv (Array sh e)


-- An annotated AST suitable for execution
--
type ExecAcc arch a     = ExecOpenAcc arch () a
type ExecAfun arch a    = PreAfun (ExecOpenAcc arch) a

type ExecOpenExp arch   = PreOpenExp (ExecOpenAcc arch)
type ExecOpenFun arch   = PreOpenFun (ExecOpenAcc arch)

type ExecExp arch       = ExecOpenExp arch ()
type ExecFun arch       = ExecOpenFun arch ()

