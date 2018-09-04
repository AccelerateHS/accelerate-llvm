{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Environment
-- Copyright   : [2018] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Environment
  where

import Data.Array.Accelerate.AST                                ( Idx(..) )


-- | An environment for local bindings, encoded at the value level as
-- a heterogeneous snoc list, and on the type level as nested tuples.
--
data ValR f env where
  Empty ::                      ValR f ()
  Push  :: ValR f env -> f t -> ValR f (env, t)

-- | Projection of a value from the valuation environment using a de Bruijn
-- index.
--
{-# INLINEABLE prj #-}
prj :: Idx env t -> ValR f env -> f t
prj ZeroIdx      (Push _   v) = v
prj (SuccIdx ix) (Push val _) = prj ix val
#if __GLASGOW_HASKELL__ < 800
prj _            _            = $internalError "prj" "inconsistent valuation"
#endif

