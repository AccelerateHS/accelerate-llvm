{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RoleAnnotations    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Name
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Name
  where

import Data.Data
import Data.Word
import Data.String
import Prelude


-- | Objects of various sorts in LLVM IR are identified by address in the LLVM
-- C++ API, and may be given a string name. When printed to (resp. read from)
-- human-readable LLVM assembly, objects without string names are numbered
-- sequentially (resp. must be numbered sequentially). String names may be
-- quoted, and are quoted when printed if they would otherwise be misread - e.g.
-- when containing special characters.
--
-- > 7
--
-- means the seventh unnamed object, while
--
-- > "7"
--
-- means the object named with the string "7".
--
-- This libraries handling of 'UnName's during translation of the AST down into
-- C++ IR is somewhat more forgiving than the LLVM assembly parser: it does not
-- require that unnamed values be numbered sequentially; however, the numbers of
-- 'UnName's passed into C++ cannot be preserved in the C++ objects. If the C++
-- IR is printed as assembly or translated into a Haskell AST, unnamed nodes
-- will be renumbered sequentially. Thus unnamed node numbers should be thought
-- of as having any scope limited to the 'LLVM.AST.Module' in which they
-- are used.
--
type role Name representational
data Name a
  = Name String         -- ^ a string name
  | UnName Word         -- ^ a number for a nameless thing
  deriving (Eq, Ord, Read, Show, Typeable, Data)

instance IsString (Name a) where
  fromString = Name


-- TLM: 'Name' is used a lot over the place, to refer to things like variables
--      as well as basic block labels. In the first case the type makes sense,
--      but what about the latter? Should basic blocks have type '()', or 'IO
--      ()', or the type of the thing that they "return" (although, from memory
--      BBs don't really return anything, they just compute a bunch of stuff
--      which is now in scope. Hmm... the only types that we can truly know are
--      the inputs to the basic block bound via phi nodes, but this is
--      unsatisfactory... )
--

data Label = Label String
  deriving (Eq, Ord, Read, Show, Typeable, Data)

instance IsString Label where
  fromString = Label

