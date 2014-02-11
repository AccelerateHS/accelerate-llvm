{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS -fno-warn-orphans #-}
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
import LLVM.General.AST.Global

#include "accelerate.h"

{--
-- TODO: We might need to use LLVM for module-level code generation, for example
--       to keep track of adding global definitions (external functions, etc.)
--
type LLVM a = CodeGen a


-- This isn't correct, since we should really be returning a module, with the
-- blocks formed, etc. The outer LLVM state should probably track what global
-- symbol declarations need to be emitted (as well as a per-skeleton counter).
-- The inner CodeGen monad keeps track of the basic blocks for an individual
-- function or expression.
--
runLLVM :: LLVM a -> (a, CodeGenState)
runLLVM ll = runState (runCodeGen ll) st
  where
    st  = CodeGenState "entry" (Map.singleton "entry" blk) 0
    blk = BlockState 0 Seq.empty Nothing
--}

-- Names
-- =====

instance IsString Name where
  fromString = Name . fromString

-- | Generate a fresh (un)name.
--
freshName :: LLVM Name
freshName = state $ \s@ModuleState{..} -> ( UnName next, s { next = next + 1 } )


-- Code generation operations
-- ==========================

-- | The code generation state for a module. This keeps track of the module-wide
-- symbol table as well as our unique name supply counter.
--
-- Although instructions are only numbered sequentially within each function
-- definition (not per module), we will eventually inline all scalar expressions
-- and functions directly into the skeleton body. If instead we attempt generate
-- individual functions for each of the scalar fragments, even with inlining
-- there is some redundant code that LLVM is unable to remove; mainly related to
-- marshalling elements into and out of `struct`s.
--
data ModuleState = ModuleState
  { symbolTable         :: Map Name Global              -- global function declarations
  , next                :: {-# UNPACK #-} !Word         -- a name supply
  }
  deriving Show

newtype LLVM a = LLVM { unLLVM :: State ModuleState a }
  deriving (Functor, Applicative, Monad, MonadState ModuleState)

runLLVM :: String -> {- Target -> -} LLVM Global -> Module
runLLVM nm ll =
  let (r, s) = runState (unLLVM ll) (ModuleState Map.empty 0)
      dfs    = map GlobalDefinition (r : Map.elems (symbolTable s))
  in
  Module { moduleName         = nm
         , moduleDataLayout   = Nothing
         , moduleTargetTriple = Nothing
         , moduleDefinitions  = dfs
         }


-- | The code generation state for scalar functions and expressions.
--
-- We use two records: one to hold all the code generation state as it walks the
-- AST, and one for each of the basic blocks that are generated during the walk.
-- All this is stacked on top of the module state.
--
data CodeGenState = CodeGenState
  { currentBlock        :: Name                         -- name of the currently active block
  , blockChain          :: Map Name BlockState          -- blocks for this function
  }
  deriving Show

data BlockState = BlockState
  { blockIndex          :: {-# UNPACK #-} !Int          -- index of this block (for sorting on output)
  , instructions        :: Seq (Named Instruction)      -- stack of instructions
  , terminator          :: Maybe (Named Terminator)     -- block terminator
  }
  deriving Show

newtype CodeGenT m a = CodeGenT { unCodeGen :: StateT CodeGenState m a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState, MonadTrans)

type CodeGen a = CodeGenT LLVM a

runCodeGen :: Name -> CodeGen a -> LLVM (a, [BasicBlock])
runCodeGen nm cg = do
  let st = CodeGenState nm bs
      bs = Map.singleton nm (BlockState 0 Seq.empty Nothing)
  --
  (r, s) <- runStateT (unCodeGen cg) st
  return (r, createBlocks s)


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
          term  = Do (Ret Nothing []) -- fromMaybe err terminator
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
  name  <- lift freshName
  let push Nothing  = INTERNAL_ERROR(error) "instr" "unknown block"
      push (Just b) = Just $ b { instructions = instructions b Seq.|> name := op }
  --
  modify $ \s -> s { blockChain = Map.alter push (currentBlock s) (blockChain s) }
  return (LocalReference name)


-- | Add a global declaration to the symbol table
--
declare :: Global -> CodeGen ()
declare g =
  let unique (Just _) = INTERNAL_ERROR(error) "global" "duplicate symbol"
      unique Nothing  = Just g
  in
  lift $ modify (\s -> s { symbolTable = Map.alter unique (name g) (symbolTable s) })


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

