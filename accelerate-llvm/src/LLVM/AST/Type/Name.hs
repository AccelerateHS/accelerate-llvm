{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Name
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Name
  where

import Data.ByteString.Short                                        ( ShortByteString )
import Data.Data
#if __GLASGOW_HASKELL__ >= 800
import Data.Semigroup
#endif
import Data.String
import Data.Word
import Prelude

import LLVM.AST.Type.Downcast

import qualified LLVM.AST.Name                                      as LLVM


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
  = Name   {-# UNPACK #-} !ShortByteString    -- ^ a string name
  | UnName {-# UNPACK #-} !Word               -- ^ a number for a nameless thing
  deriving (Eq, Ord, Read, Show, Typeable, Data)

instance IsString (Name a) where
  fromString = Name . fromString


-- TLM: 'Name' is used a lot over the place, to refer to things like variables
--      as well as basic block labels. In the first case the type makes sense,
--      but what about the latter? Should basic blocks have type '()', or 'IO
--      ()', or the type of the thing that they "return" (although, from memory
--      BBs don't really return anything, they just compute a bunch of stuff
--      which is now in scope. Hmm... the only types that we can truly know are
--      the inputs to the basic block bound via phi nodes, but this is
--      unsatisfactory... )
--

data Label = Label {-# UNPACK #-} !ShortByteString
  deriving (Eq, Ord, Read, Show, Typeable, Data)

instance IsString Label where
  fromString = Label . fromString

#if __GLASGOW_HASKELL__ >= 800
instance Semigroup Label where
  Label x <> Label y = Label (x <> y)
#endif

instance Monoid Label where
  mempty                      = Label mempty
#if   __GLASGOW_HASKELL__ < 804
  {-# INLINE mappend #-}
  mappend                     = (<>)
#elif __GLASGOW_HASKELL__ < 800
  mappend (Label x) (Label y) = Label (mappend x y)
#endif


-- | Convert to llvm-hs
--
instance Downcast (Name a) LLVM.Name where
  downcast (Name s)   = LLVM.Name s
  downcast (UnName n) = LLVM.UnName n

instance Downcast Label LLVM.Name where
  downcast (Label l)  = LLVM.Name l

