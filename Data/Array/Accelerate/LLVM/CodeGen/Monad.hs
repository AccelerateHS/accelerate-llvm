{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Monad
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--


module Data.Array.Accelerate.LLVM.CodeGen.Monad
  where

-- standard library
import Data.Word
import Data.Maybe
import Data.String
import Data.Function
import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Map                                         ( Map )
import Data.Sequence                                    ( Seq )
import qualified Data.Map                               as Map
import qualified Data.Sequence                          as Seq
import qualified Data.Foldable                          as Seq

-- llvm-general
import LLVM.General.AST

#include "accelerate.h"


-- TODO: We might need to use LLVM for module-level code generation, for example
--       to keep track of adding global definitions (external functions, etc.)
--
type LLVM a = CodeGen a

-- This isn't correct, since we should really be returning a module, with the
-- blocks formed, etc.
--
runLLVM :: LLVM a -> a
runLLVM ll = evalState (runCodeGen ll) st
  where
    st  = CodeGenState "entry" (Map.singleton "entry" blk) 0
    blk = BlockState 0 Seq.empty Nothing


-- Names
-- =====

instance IsString Name where
  fromString = Name . fromString

-- | Generate a fresh (un)name.
--
freshName :: CodeGen Name
freshName = state $ \s@CodeGenState{..} ->
  ( UnName next, s { next = next + 1 })


-- Code generation operations
-- ==========================

-- | The code generation state for scalar expressions.
--
-- We use two records: one to hold all the code generation state as it walks the
-- AST, and one for each of the basic blocks that are generated during the walk.
--
data CodeGenState = CodeGenState
  { currentBlock        :: Name                         -- name of the currently active block
  , blockChain          :: Map Name BlockState          -- blocks for this function
  , next                :: {-# UNPACK #-} !Word         -- for unique name supply
  }
  deriving Show

data BlockState = BlockState
  { blockIndex          :: {-# UNPACK #-} !Int          -- index of this block (for sorting on output)
  , instructions        :: Seq (Named Instruction)      -- stack of instructions
  , terminator          :: Maybe (Named Terminator)     -- block terminator
  }
  deriving Show

newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState, Typeable)


-- | Extract the block state and construct the basic blocks
--
createBlocks :: CodeGenState -> [BasicBlock]
createBlocks
  = makeBlocks
  . sortBlocks
  . Map.toList
  . blockChain
  where
    makeBlocks = map (\(label, BlockState{..}) ->
      let err   = INTERNAL_ERROR(error) "createBlocks" ("Block has no terminator (" ++ show label ++ ")")
          term  = fromMaybe err terminator
          ins   = Seq.toList instructions
      in
      BasicBlock label ins term)

    sortBlocks =
      sortBy (compare `on` (blockIndex . snd))


-- | Add an instruction to the state of the currently active block so that it is
-- computed, and return the operand (LocalReference) that can be used to later
-- refer to it.
--
instr :: Instruction -> CodeGen Operand
instr op = do
  name  <- freshName
  modify $ \s -> s { blockChain = Map.adjust (\b -> b { instructions = instructions b Seq.|> name := op })
                                             (currentBlock s)
                                             (blockChain s)}
  return (LocalReference name)


{--
-- Block chain
-- ===========

-- | Replace the block state of the currently active block
--
modifyCurrentBlock :: BlockState -> CodeGen ()
modifyCurrentBlock new = modify $ \s@CodeGenState{..} ->
  s { blockChain = Map.insert currentBlock new blockChain }

-- | Retrieve the state of the currently active block
--
getCurrentBlock :: CodeGen BlockState
getCurrentBlock = state $ \s@CodeGenState{..} ->
  case Map.lookup currentBlock blockChain of
    Just b      -> (b, s)
    Nothing     -> INTERNAL_ERROR(error) "getCurrentBlock"
                 $ "Unknown block: " ++ show currentBlock
--}

