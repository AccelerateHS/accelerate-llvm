{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Environment
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Environment
  where

import Data.IntMap                                              ( IntMap )
import Data.String
import Text.Printf
import qualified Data.IntMap                                    as IM

import Data.Array.Accelerate.AST                                ( ArrayVar )
import Data.Array.Accelerate.AST.Idx                            ( Idx(..), idxToInt )
import Data.Array.Accelerate.AST.Var                            ( Var(..) )
import Data.Array.Accelerate.Error                              ( internalError )
import Data.Array.Accelerate.Representation.Array               ( Array, ArrayR(..) )

import Data.Array.Accelerate.LLVM.CodeGen.IR

import LLVM.AST.Type.Name

import GHC.Stack


-- Scalar environment
-- ==================

-- | An environment for local scalar expression bindings, encoded at the value
-- level as a heterogenous snoc list, and on the type level as nested tuples.
--
data Val env where
  Empty ::                          Val ()
  Push  :: Val env -> Operands t -> Val (env, t)

-- | Projection of a value from the valuation environment using a de Bruijn
-- index.
--
prj :: Idx env t -> Val env -> Operands t
prj ZeroIdx      (Push _   v) = v
prj (SuccIdx ix) (Push val _) = prj ix val


-- Array environment
-- =================

-- | A mapping between the environment index of a free array variable and the
-- Name of that array to be used in the generated code.
--
-- This simply compresses the array indices into a continuous range, rather than
-- directly using the integer equivalent of the de Bruijn index. Thus, the
-- result is still sensitive to the order of let bindings, but not of any
-- intermediate (unused) free array variables.
--
type Gamma aenv = IntMap (Label, Idx' aenv)

data Idx' aenv where
  Idx' :: ArrayR (Array sh e) -> Idx aenv (Array sh e) -> Idx' aenv

-- Projection of a value from the array environment using a de Bruijn index.
-- This returns a pair of operands to access the shape and array data
-- respectively.
--
aprj :: HasCallStack => Idx aenv t -> Gamma aenv -> Name t
aprj ix aenv =
  case IM.lookup (idxToInt ix) aenv of
    Nothing             -> internalError "free variable not registered"
    Just (Label n,_)    -> Name n


-- | Construct the array environment index, will be used by code generation to
-- map free array variable indices to names in the generated code.
--
makeGamma :: IntMap (Idx' aenv) -> Gamma aenv
makeGamma = snd . IM.mapAccum (\n ix -> (n+1, toAval n ix)) 0
  where
    toAval :: Int -> Idx' aenv -> (Label, Idx' aenv)
    toAval n ix = (fromString (printf "fv%d" n), ix)

-- | A free variable
--
freevar :: ArrayVar aenv a -> IntMap (Idx' aenv)
freevar (Var repr@ArrayR{} ix) = IM.singleton (idxToInt ix) (Idx' repr ix)

