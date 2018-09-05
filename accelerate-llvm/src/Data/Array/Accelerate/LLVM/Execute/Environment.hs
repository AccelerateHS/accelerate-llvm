{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Environment
-- Copyright   : [2014..2018] Trevor L. McDonell
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
import Data.Array.Accelerate.AST                                    ( Idx(..) )
#if __GLASGOW_HASKELL__ < 800
import Data.Array.Accelerate.Error
#endif

import Data.Array.Accelerate.LLVM.Execute.Async


-- Environments
-- ------------

-- Valuation for an environment of futures
--
data ValR arch env where
  Empty :: ValR arch ()
  Push  :: ValR arch env -> FutureR arch t -> ValR arch (env, t)


-- Projection of a value from a valuation using a de Bruijn index.
--
prj :: Idx env t -> ValR arch env -> FutureR arch t
prj ZeroIdx       (Push _   x) = x
prj (SuccIdx idx) (Push val _) = prj idx val
#if __GLASGOW_HASKELL__ < 800
prj _             _            = $internalError "prj" "inconsistent valuation"
#endif

