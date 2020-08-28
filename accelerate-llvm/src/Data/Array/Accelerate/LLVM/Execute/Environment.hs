{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Environment
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Environment
  where

import Data.Array.Accelerate.AST                                    ( ALeftHandSide, ELeftHandSide )
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.Representation.Array

import Data.Array.Accelerate.LLVM.Execute.Async


-- Environments
-- ------------

-- Valuation for an environment of futures
--
data ValR arch env where
  Empty :: ValR arch ()
  Push  :: ValR arch env -> FutureR arch t -> ValR arch (env, t)

push :: ValR arch env -> (ALeftHandSide t env env', FutureArraysR arch t) -> ValR arch env'
push env (LeftHandSideWildcard _     , _       ) = env
push env (LeftHandSideSingle ArrayR{}, a       ) = env `Push` a
push env (LeftHandSidePair l1 l2     , (a1, a2)) = push env (l1, a1) `push` (l2, a2)

pushE :: Async arch => ValR arch env -> (ELeftHandSide t env env', FutureR arch t) -> Par arch (ValR arch env')
pushE env (LeftHandSideSingle _  , e) = return $ env `Push` e
pushE env (LeftHandSideWildcard _, _) = return env
pushE env (LeftHandSidePair l1 l2, e) = do
  -- TODO: This code creates many intermediate Futures, in case of deeply nested pairs.
  -- We could improve this to only construct Futures for the values actually stored
  -- in the environment and not have any intermediate ones. We can do that in a similar
  -- way as done in Data.Array.Accelerate.LLVM.Execute.split
  --
  e1 <- new
  e2 <- new
  fork $ do
    (v1, v2) <- get e
    put e1 v1
    put e2 v2
  env' <- pushE env (l1, e1)
  pushE env' (l2, e2)

-- Projection of a value from a valuation using a de Bruijn index.
--
prj :: Idx env t -> ValR arch env -> FutureR arch t
prj ZeroIdx       (Push _   x) = x
prj (SuccIdx idx) (Push val _) = prj idx val

