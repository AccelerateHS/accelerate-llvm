{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Environment
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Environment
  where

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.Execute.Async


-- Array environments
-- ------------------

-- Valuation for an environment of array computations
--
data AvalR arch env where
  Aempty :: AvalR arch ()
  Apush  :: AvalR arch env -> AsyncR arch t -> AvalR arch (env, t)


-- Projection of a value from a valuation using a de Bruijn index.
--
aprj :: Idx env t -> AvalR arch env -> AsyncR arch t
aprj ZeroIdx       (Apush _   x) = x
aprj (SuccIdx idx) (Apush val _) = aprj idx val
aprj _             _             = $internalError "aprj" "inconsistent valuation"

