{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
import Control.Applicative
import Control.Monad.State
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
freshName :: CodeGen Name
freshName = state $ \s@CodeGenState{..} -> ( UnName next, s { next = next + 1 } )


-- Code generation operations
-- ==========================

-- | A compiled module consists of a number of global functions (kernels)
--
data Module t aenv a = Module { unModule :: AST.Module }

-- | A fully-instantiated skeleton is a kernel that can be compiled by LLVM into
-- a global function that we can execute.
--
-- The data type, rather than type synonym, is required to fix the phantom type
-- parameters, which is useful during code generation.
--
data Kernel t aenv a = Kernel { unKernel :: AST.Global }


-- | The code generation state for scalar functions and expressions.
--
-- We use two records: one to hold all the code generation state as it walks the
-- AST, and one for each of the basic blocks that are generated during the walk.
--
data CodeGenState = CodeGenState
  { blockChain          :: Seq Block                    -- blocks for this function
  , symbolTable         :: Map Name AST.Global          -- global (external) function declarations
  , next                :: {-# UNPACK #-} !Word         -- a name supply
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
--      Actually, it turns out that this doesn't matter, and LLVM just deals
--      with the graph. However, I think that it is fair to avoid an O(log n)
--      lookup in order to add each instruction, when 99% of the time we are
--      always adding to the end of the last block chain. The only exception
--      here is that of the phi node.
--

data Block = Block
  { blockLabel          :: Name                         -- block label
  , instructions        :: Seq (Named Instruction)      -- stack of instructions
  , terminator          :: Maybe (Named Terminator)     -- block terminator
  }
  deriving Show

newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)


runLLVM :: forall t aenv a. Target t => CodeGen [Kernel t aenv a] -> Module t aenv a
runLLVM ll =
  let ll'               = newBlock "entry" >>= setBlock >> ll
      (r, st)           = runState (runCodeGen ll') (CodeGenState Seq.empty Map.empty 0)
      kernels           = map unKernel r
      defs              = map GlobalDefinition (kernels ++ Map.elems (symbolTable st))

      name | x:_          <- kernels
           , f@Function{} <- x
           , Name s <- AST.name f = s
           | otherwise            = "<string>"
  in
  Module $ AST.Module
    { moduleName         = name
    , moduleDataLayout   = targetDataLayout (undefined::t)
    , moduleTargetTriple = targetTriple (undefined::t)
    , moduleDefinitions  = defs
    }


-- | Extract the block state and construct the basic blocks to form a function
-- body. This also clears the block stream, ready for a new function to be
-- generated.
--
createBlocks :: CodeGen [BasicBlock]
createBlocks = state $ \s -> ( Seq.toList $ fmap makeBlock (blockChain s) , s { blockChain = Seq.empty, next = 0 } )
  where
    makeBlock Block{..} =
      let err   = INTERNAL_ERROR(error) "createBlocks" $ "Block has no terminator (" ++ show blockLabel ++ ")"
          term  = fromMaybe err terminator
          ins   = Seq.toList instructions
      in
      BasicBlock blockLabel ins term


-- Instructions
-- ------------

-- | Add an instruction to the state of the currently active block so that it is
-- computed, and return the operand (LocalReference) that can be used to later
-- refer to it.
--
instr :: Instruction -> CodeGen Operand
instr op = do
  name  <- freshName
  state $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> INTERNAL_ERROR(error) "instr" "empty block chain"
      bs Seq.:> b -> ( LocalReference name
                     , s { blockChain = bs Seq.|> b { instructions = instructions b Seq.|> name := op } } )

-- | Execute an unnamed instruction
--
do_ :: Instruction -> CodeGen ()
do_ op = do
  modify $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> INTERNAL_ERROR(error) "do_" "empty block chain"
      bs Seq.:> b -> s { blockChain = bs Seq.|> b { instructions = instructions b Seq.|> Do op } }


-- | Add a phi node to the top of the specified block
--
phi :: Block                    -- ^ the basic block to modify
    -> Name                     -- ^ the name of the critical variable (to assign the result of the phi instruction)
    -> Type                     -- ^ type of the critical variable
    -> [(Operand, Block)]       -- ^ list of incoming operands and the precursor basic block they come from
    -> CodeGen Operand
phi target crit t incoming =
  let op          = Phi t [ (o,blockLabel) | (o,Block{..}) <- incoming ] []
      search this = blockLabel this == blockLabel target
  in
  state $ \s ->
    case Seq.findIndexR search (blockChain s) of
      Nothing -> INTERNAL_ERROR(error) "phi" "unknown basic block"
      Just i  -> ( LocalReference crit
                 , s { blockChain = Seq.adjust (\b -> b { instructions = crit := op Seq.<| instructions b }) i (blockChain s) } )

phi' :: Type -> [(Operand,Block)] -> CodeGen Operand
phi' t incoming = do
  crit  <- freshName
  block <- state $ \s -> case Seq.viewr (blockChain s) of
                           Seq.EmptyR -> INTERNAL_ERROR(error) "phi'" "empty block chain"
                           _ Seq.:> b -> ( b, s )
  phi block crit t incoming


-- | Unconditional branch
--
br :: Block -> CodeGen Block
br target = terminate $ Do (Br (blockLabel target) [])

-- | Conditional branch
--
cbr :: Operand -> Block -> Block -> CodeGen Block
cbr cond t f = terminate $ Do (CondBr cond (blockLabel t) (blockLabel f) [])

-- | Add a termination condition to the current instruction stream. Return the
-- name of the block chain that was just terminated.
--
terminate :: Named Terminator -> CodeGen Block
terminate target =
  state $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> INTERNAL_ERROR(error) "terminate" "empty block chain"
      bs Seq.:> b -> ( b, s { blockChain = bs Seq.|> b { terminator = Just target } } )


-- | Add a global declaration to the symbol table
--
declare :: Global -> CodeGen ()
declare g =
  let unique (Just q) | g /= q    = INTERNAL_ERROR(error) "global" "duplicate symbol"
                      | otherwise = Just g
      unique _                    = Just g
  in
  modify (\s -> s { symbolTable = Map.alter unique (AST.name g) (symbolTable s) })


-- Block chain
-- ===========

-- | Create a new basic block, but don't yet add it to the block chain. You need
-- to call setBlock to append it to the chain, so that new instructions are then
-- added to this block.
--
newBlock :: String -> CodeGen Block
newBlock nm =
  state $ \s ->
    let idx     = Seq.length (blockChain s)
        label   = Name $ let (h,t) = break (== '.') nm in (h ++ shows idx t)
        next    = Block label Seq.empty Nothing
    in
    ( next, s )

-- | Same as 'newBlock', but the given string is appended to the name of the
-- currently active block.
--
newSubBlock :: String -> CodeGen Block
newSubBlock ext = do
  chain <- gets blockChain
  case Seq.viewr chain of
    _ Seq.:> b | Name base <- blockLabel b -> newBlock $ base ++ ('.':ext)
    _                                      -> INTERNAL_ERROR(error) "newSubBlock" "empty block chain"

-- | Add this block to the block stream. Any instructions pushed onto the stream
-- by 'instr' and friends will now apply to this block.
--
setBlock :: Block -> CodeGen ()
setBlock next =
  modify $ \s -> s { blockChain = blockChain s Seq.|> next }

