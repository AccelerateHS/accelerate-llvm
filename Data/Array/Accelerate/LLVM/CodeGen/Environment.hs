{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Environment
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Environment
  where

-- llvm-general
import LLVM.General.AST

-- accelerate
import Data.Array.Accelerate.AST                hiding ( Val(..), prj )

-- standard library
import Data.IntMap                              ( IntMap )
import qualified  Data.IntMap                   as Map

#include "accelerate.h"


-- Environments
-- ============

-- | A mapping between the environment index of a free array variable and the
-- Name of that array to be used in the generated code.
--
-- This simply compresses the array indices into a continuous range, rather than
-- directly using the integer equivalent of the de Bruijn index. Thus, the
-- result is still sensitive to the order of let bindings, but not of any
-- intermediate (unused) free array variables.
--
type Aval aenv = IntMap Name

-- Projection of a value from the array environment using a de Bruijn index.
-- This returns a pair of operands to access the shape and array data
-- respectively.
--
aprj :: Idx aenv t -> Aval aenv -> Name
aprj ix aenv =
  case Map.lookup (idxToInt ix) aenv of
    Just n  -> n
    Nothing -> INTERNAL_ERROR(error) "aprj" "inconsistent valuation"


-- | An environment for local scalar expression bindings, encoded at the value
-- level as a heterogenous snoc list, and on the type level as nested tuples.
--
data Val env where
  Empty ::                         Val ()
  Push  :: Val env -> [Operand] -> Val (env, t)
                        -- ^ Idx env t ~ IR env aenv t

-- Projection of a value from the valuation environment using a de Bruijn index.
--
prj :: Idx env t -> Val env -> [Operand]
prj ZeroIdx      (Push _   v) = v
prj (SuccIdx ix) (Push val _) = prj ix val
prj _            _            = INTERNAL_ERROR(error) "prj" "inconsistent valuation"

