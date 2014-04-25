{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Environment
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
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
import Data.Array.Accelerate.AST                                hiding ( Val(..), prj )
import Data.Array.Accelerate.Error                              ( internalError )
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )

-- standard library
import Data.IntMap                                              ( IntMap )
import qualified Data.IntMap                                    as IM


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
type Gamma aenv = IntMap (Name, Idx' aenv)

-- Projection of a value from the array environment using a de Bruijn index.
-- This returns a pair of operands to access the shape and array data
-- respectively.
--
aprj :: Idx aenv t -> Gamma aenv -> Name
aprj ix aenv =
  case IM.lookup (idxToInt ix) aenv of
    Just (n,_)  -> n
    Nothing     -> $internalError "aprj" "inconsistent valuation"


data Idx' aenv where
  Idx' :: (Shape sh, Elt e) => Idx aenv (Array sh e) -> Idx' aenv


-- | Construct the array environment index, will be used by code generation to
-- map free array variable indices to names in the generated code.
--
makeGamma :: IntMap (Idx' aenv) -> Gamma aenv
makeGamma = snd . IM.mapAccum (\n ix -> (n+1, toAval n ix)) 0
  where
    toAval :: Int -> Idx' aenv -> (Name, Idx' aenv)
    toAval n ix = (Name ("fv" ++ show n), ix)

freevar :: (Shape sh, Elt e) => Idx aenv (Array sh e) -> IntMap (Idx' aenv)
freevar ix = IM.singleton (idxToInt ix) (Idx' ix)


-- Scalar environment
-- ==================

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
prj _            _            = $internalError "prj" "inconsistent valuation"

