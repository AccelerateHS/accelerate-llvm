{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Global
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Global
  where

import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Function
import LLVM.AST.Type.Name

import qualified LLVM.AST.Global                                    as LLVM
import qualified LLVM.AST.Name                                      as LLVM
import qualified LLVM.AST.Type                                      as LLVM


-- | A global function definition.
--
type GlobalFunction args t = Function Label args t

instance Downcast (GlobalFunction args t) LLVM.Global where
  downcast f = LLVM.functionDefaults { LLVM.name       = nm
                                     , LLVM.returnType = res
                                     , LLVM.parameters = (params, False)
                                     }
    where
      trav :: GlobalFunction args t -> ([LLVM.Type], LLVM.Type, LLVM.Name)
      trav (Body t _ n) = ([], downcast t, downcast n)
      trav (Lam a _ l)  = let (as, r, n) = trav l
                          in  (downcast a : as, r, n)
      --
      (args, res, nm)  = trav f
      params           = [ LLVM.Parameter t (LLVM.UnName i) [] | t <- args | i <- [0..] ]

