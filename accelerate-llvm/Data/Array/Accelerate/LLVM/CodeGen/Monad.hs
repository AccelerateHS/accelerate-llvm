{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Monad
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Monad
  where

-- standard library
import Control.Applicative
import Control.Monad.State.Strict
-- import Data.Maybe
-- import Data.Map                                                 ( Map )
import Data.Sequence                                            ( Seq )
-- import Data.HashMap.Strict                                      ( HashMap )
-- import Data.String
import Data.Word
import qualified Data.Sequence                                  as Seq

-- accelerate
import Data.Array.Accelerate.Error
import qualified Data.Array.Accelerate.Debug                    as Debug

-- llvm-general-typed
import LLVM.General.AST.Type.Downcast
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Representation

-- llvm-general-pure
import qualified LLVM.General.AST.Instruction                   as L


-- Code generation
-- ===============

-- | The code generation state for scalar functions and expressions.
--
-- We use two records: one to hold all the code generation state as it walks the
-- AST, and one for each of the basic blocks that are generated during the walk.
--
data CodeGenState = CodeGenState
  { blockChain          :: Seq Block                            -- blocks for this function
--  , symbolTable         :: Map Name AST.Global                  -- global (external) function declarations
--  , metadataTable       :: HashMap String (Seq [Maybe Operand]) -- module metadata to be collected
--  , intrinsicTable      :: HashMap String Name                  -- standard math intrinsic functions
  , next                :: {-# UNPACK #-} !Word                 -- a name supply
  }
  deriving Show

data Block = Block
  { blockLabel          :: Label                                -- block label
  , instructions        :: Seq (L.Named L.Instruction)          -- stack of instructions
  , terminator          :: L.Named L.Terminator                 -- block terminator
--  , returnType          :: L.Type
  }
  deriving Show

newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)


initCodeGenState :: CodeGenState
initCodeGenState = CodeGenState
  { blockChain  = initBlockChain
  , next        = 0
  }


-- Block chain
-- ===========

-- | An initial block chain
--
initBlockChain :: Seq Block
initBlockChain
  = Seq.singleton
  $ Block "entry" Seq.empty ($internalError "entry" "block has no terminator")


-- | Create a new basic block, but don't yet add it to the block chain. You need
-- to call 'setBlock' to append it to the chain, so that subsequent instructions
-- are added to this block.
--
-- Note: [Basic blocks]
--
-- The names of basic blocks are generated based on the base name provided to
-- the 'newBlock' function, as well as the current state (length) of the block
-- stream. By not immediately adding new blocks to the stream, we have the
-- advantage that:
--
--   1. Instructions are generated "in order", and are always appended to the
--      stream. There is no need to search the stream for a block of the right
--      name.
--
--   2. Blocks are named in groups, which helps readability. For example, the
--      blocks for the then and else branches of a conditional, created at the
--      same time, will be named similarly: 'if4.then' and 'if4.else', etc.
--
-- However, this leads to a slight awkwardness when walking the AST. Since a new
-- naming group scheme is only applied *after* a call to 'setBlock',
-- encountering (say) nested conditionals in the walk will generate logically
-- distinct blocks that happen to have the same name. This means that
-- instructions might be added to the wrong blocks, or the first set of blocks
-- will be emitted empty and/or without a terminator.
--
newBlock :: String -> CodeGen Block
newBlock nm =
  state $ \s ->
    let idx     = Seq.length (blockChain s)
        label   = let (h,t) = break (== '.') nm in (h ++ shows idx t)
        next    = Block (Label label) Seq.empty err
        err     = $internalError label "Block has no terminator"
    in
    ( next, s )


-- | Add this block to the block stream. Any instructions pushed onto the stream
-- by 'instr' and friends will now apply to this block.
--
setBlock :: Block -> CodeGen ()
setBlock next =
  modify $ \s -> s { blockChain = blockChain s Seq.|> next }


-- Instructions
-- ------------

-- | Generate a fresh (un)name.
--
freshName :: CodeGen (Name a)
freshName = state $ \s@CodeGenState{..} -> ( UnName next, s { next = next + 1 } )

-- | Add an instruction to the state of the currently active block so that it is
-- computed, and return the operand (LocalReference) that can be used to later
-- refer to it.
--
instr :: Instruction a -> CodeGen (Operand a)
instr ins = do
  name  <- freshName
  state $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> $internalError "instr" "empty block chain"
      bs Seq.:> b -> ( LocalReference (instructionType ins) name
                     , s { blockChain = bs Seq.|> b { instructions = instructions b Seq.|> downcast name L.:= downcast ins } } )


-- | Execute an unnamed instruction
--
do_ :: Instruction () -> CodeGen ()
do_ ins =
  modify $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> $internalError "do_" "empty block chain"
      bs Seq.:> b -> s { blockChain = bs Seq.|> b { instructions = instructions b Seq.|> L.Do (downcast ins) } }


-- | Return void from a basic block
--
return_ :: CodeGen ()
return_ = void $ terminate (L.Do Ret)

-- | Return a value from a basic block
--
retval_ :: IsType a => Operand a -> CodeGen ()
retval_ x = void $ terminate (L.Do (RetVal x))


-- | Add a termination condition to the current instruction stream. Also return
-- the block that was just terminated.
--
terminate :: forall a. IsType a => L.Named (Terminator a) -> CodeGen Block
terminate target =
  state $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> $internalError "terminate" "empty block chain"
      bs Seq.:> b -> ( b, s { blockChain = bs Seq.|> b { terminator = downcast target } } )
--                                                       , returnType = llvmType (undefined :: a) } } )


-- Debug
-- =====

{-# INLINE trace #-}
trace :: String -> a -> a
trace msg = Debug.trace Debug.dump_cc ("llvm: " ++ msg)

