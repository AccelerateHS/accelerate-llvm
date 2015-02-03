{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Environment
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Environment
  where

import Data.Array.Accelerate.AST                        ( Idx(..) )
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.CodeGen.IR


-- | An environment for local scalar expression bindings, encoded at the value
-- level as a heterogenous snoc list, and on the type level as nested tuples.
--
data Val env where
  Empty ::                    Val ()
  Push  :: Val env -> IR t -> Val (env, t)

-- | Projection of a value from the valuation environment using a de Bruijn
-- index.
--
prj :: Idx env t -> Val env -> IR t
prj ZeroIdx      (Push _   v) = v
prj (SuccIdx ix) (Push val _) = prj ix val
prj _            _            = $internalError "prj" "inconsistent valuation"

