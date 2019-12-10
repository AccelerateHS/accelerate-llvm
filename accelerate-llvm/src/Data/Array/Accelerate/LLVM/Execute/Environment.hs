{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Environment
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Environment
  where

-- accelerate
import Data.Array.Accelerate.AST                                    ( Idx(..), LeftHandSide(..) )
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

push :: ValR arch env -> (LeftHandSide t env env', FutureArraysR arch t) -> ValR arch env'
push env (LeftHandSideWildcard _, _       ) = env
push env (LeftHandSideArray     , a       ) = env `Push` a
push env (LeftHandSidePair l1 l2, (a1, a2)) = push env (l1, a1) `push` (l2, a2)

-- Projection of a value from a valuation using a de Bruijn index.
--
prj :: Idx env t -> ValR arch env -> FutureR arch t
prj ZeroIdx       (Push _   x) = x
prj (SuccIdx idx) (Push val _) = prj idx val
#if __GLASGOW_HASKELL__ < 800
prj _             _            = $internalError "prj" "inconsistent valuation"
#endif

