{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Environment
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Environment
  where

-- accelerate
import Data.Array.Accelerate.AST
#if __GLASGOW_HASKELL__ < 800
import Data.Array.Accelerate.Error
#endif

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
#if __GLASGOW_HASKELL__ < 800
aprj _             _             = $internalError "aprj" "inconsistent valuation"
#endif

