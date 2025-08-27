{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty     as LLVM


-- | An external global function definition.
--
type GlobalFunction args t = Function Label args t

instance Downcast (GlobalFunction args t) LLVM.Declare where
  downcast f = LLVM.Declare
    { LLVM.decLinkage = Just LLVM.External
    , LLVM.decVisibility = Nothing
    , LLVM.decRetType = res
    , LLVM.decName = nm
    , LLVM.decArgs = args
    , LLVM.decVarArgs = False
    , LLVM.decAttrs = []
    , LLVM.decComdat = Nothing }
    where
      trav :: GlobalFunction args t -> ([LLVM.Type], LLVM.Type, LLVM.Symbol)
      trav (Body t _ n) = ([], downcast t, labelToPrettyS n)
      trav (Lam a _ l)  = let (as, r, n) = trav l
                          in  (downcast a : as, r, n)
      --
      (args, res, nm)  = trav f

