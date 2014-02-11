{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Map                                                 ( Map )
import Data.Sequence                                            ( Seq )
import qualified Data.Map                                       as Map
import qualified Data.Sequence                                  as Seq
import qualified Data.Foldable                                  as Seq

-- llvm-general
import LLVM.General.AST                                         hiding ( Module )
import qualified LLVM.General.AST                               as AST
import qualified LLVM.General.AST.Global                        as AST

-- accelerate
import Data.Array.Accelerate.LLVM.Target

#include "accelerate.h"


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

-- | A compiled module consists of a number of global functions (kernels)
--
data Module aenv a = Module { unModule :: AST.Module }

-- | A fully-instantiated skeleton is a kernel that can be compiled by LLVM into
-- a global function that we can execute.
--
-- The data type, rather than type synonym, is required to fix the phantom type
-- parameters, which is useful during code generation.
--
data Kernel aenv a = Kernel { unKernel :: AST.Global }


-- | The code generation state for a module. This keeps track of the module-wide
-- symbol table as well as our unique name supply counter.
--
-- Although instructions are only numbered sequentially within each function
-- definition (not per module), we will eventually inline all scalar expressions
-- and functions directly into the skeleton body. If instead we attempt generate
-- individual functions for each of the scalar fragments, even with inlining
-- there is some redundant code that LLVM is unable to remove; mainly related to
-- marshalling elements into and out of `struct`s (which are required to return
-- tuples from functions (unless we pass the result value by reference?))
--
data ModuleState = ModuleState
  { symbolTable         :: Map Name AST.Global          -- global function declarations
  , next                :: {-# UNPACK #-} !Word         -- a name supply
  }
  deriving Show

newtype LLVM a = LLVM { unLLVM :: State ModuleState a }
  deriving (Functor, Applicative, Monad, MonadState ModuleState)

runLLVM :: Target -> LLVM [Kernel aenv a] -> Module aenv a
runLLVM Target{..} ll =
  let (r, st)           = runState (unLLVM ll) (ModuleState Map.empty 0)
      kernels           = map unKernel r
      defs              = map GlobalDefinition (kernels ++ Map.elems (symbolTable st))

      name | x:_          <- kernels
           , f@Function{} <- x
           , Name s <- AST.name f = s
           | otherwise            = "<string>"
  in
  Module $ AST.Module
    { moduleName         = name
    , moduleDataLayout   = targetDataLayout
    , moduleTargetTriple = targetTriple
    , moduleDefinitions  = defs
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

-- TLM: I think this idea of using a Map for the Blocks is not right. I think
--      this should just be another Sequence. We sort the blocks on output based
--      on the order in which they were defined, but that is not necessarily the
--      order in which they are used. E.g. we define the bottom and top of a
--      loop, so these blocks are sorted sequentially, but during definition of
--      the loop body other blocks may be added, and so these will effectively
--      be sorted after the loop end. oops!
--
--      Changes:
--
--        * newBlock: creates a new block, but doesn't add it to the queue.
--        * setBlock: add the block to the end of the block chain.
--
--      Then, instructions are always appended to the last block in the chain,
--      whatever that may be, instead of searching in the map and altering the
--      appropriate block.
--

data BlockState = BlockState
  { blockIndex          :: {-# UNPACK #-} !Int          -- index of this block (for sorting on output)
  , instructions        :: Seq (Named Instruction)      -- stack of instructions
  , terminator          :: Maybe (Named Terminator)     -- block terminator
  }
  deriving Show

newtype CodeGenT m a = CodeGenT { unCodeGen :: StateT CodeGenState m a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState, MonadTrans)

type CodeGen a = CodeGenT LLVM a

runCodeGen :: CodeGen a -> LLVM (a, [BasicBlock])
runCodeGen cg = do
  let st  = CodeGenState undefined Map.empty
      cg' = newBlock "entry" >>= setBlock >> cg
  --
  (r, s) <- runStateT (unCodeGen cg') st
  return (r, createBlocks s)

execCodeGen :: CodeGen a -> LLVM [BasicBlock]
execCodeGen = fmap snd . runCodeGen


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
      let err   = Do (Ret Nothing []) -- TODO: INTERNAL_ERROR(error) "createBlocks" ("Block has no terminator (" ++ show label ++ ")")
          term  = fromMaybe err terminator
          ins   = Seq.toList instructions
      in
      BasicBlock label ins term)

    sortBlocks =
      sortBy (compare `on` (blockIndex . snd))

-- Instructions
-- ------------

-- | Add an instruction to the state of the currently active block so that it is
-- computed, and return the operand (LocalReference) that can be used to later
-- refer to it.
--
instr :: Instruction -> CodeGen Operand
instr op = do
  name  <- lift freshName
  let push Nothing  = INTERNAL_ERROR(error) "instr" "unknown basic block"
      push (Just b) = Just $ b { instructions = instructions b Seq.|> name := op }
  --
  modify $ \s -> s { blockChain = Map.alter push (currentBlock s) (blockChain s) }
  return (LocalReference name)

do_ :: Instruction -> CodeGen ()
do_ op = do
  let push Nothing  = INTERNAL_ERROR(error) "do_" "unknown basic block"
      push (Just b) = Just $ b { instructions = instructions b Seq.|> Do op }
  --
  modify $ \s -> s { blockChain = Map.alter push (currentBlock s) (blockChain s) }


-- | Add a global declaration to the symbol table
--
declare :: Global -> CodeGen ()
declare g =
  let unique (Just q) | g /= q    = INTERNAL_ERROR(error) "global" "duplicate symbol"
                      | otherwise = Just g
      unique _                    = Just g
  in
  lift $ modify (\s -> s { symbolTable = Map.alter unique (AST.name g) (symbolTable s) })

-- | Add a termination condition to the current instruction stream. Return the
-- name of the block chain that was just terminated.
--
terminate :: Named Terminator -> CodeGen Name
terminate target =
  let end Nothing  = INTERNAL_ERROR(error) "terminate" "unknown basic block"
      end (Just b) = Just $ b { terminator = Just target }
  in
  state $ \s -> ( currentBlock s, s { blockChain = Map.alter end (currentBlock s) (blockChain s) })


-- Block chain
-- ===========

-- | Add a new block to the block chain. It is not made active.
--
newBlock :: String -> CodeGen Name
newBlock nm =
  state $ \s@CodeGenState{..} ->
    let idx     = Map.size blockChain
        name    = Name (nm ++ show idx)
        blk     = BlockState idx Seq.empty Nothing
    in
    ( name, s { blockChain = Map.insert name blk blockChain } )

-- | Same as 'newBlock', but the given string is appended to the name of the
--   currently active block.
--
subBlock :: String -> CodeGen Name
subBlock ext = do
  Name base <- gets currentBlock
  newBlock  $  base ++ ('.':ext)

-- | Mark the specified block as being the active block. Any instructions pushed
-- onto the stream by 'instr' will be appended to this block.
--
setBlock :: Name -> CodeGen ()
setBlock name = modify (\s -> s { currentBlock = name })

