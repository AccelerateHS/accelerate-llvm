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

module Data.Array.Accelerate.LLVM.CodeGen.Monad (

  CodeGen,
  runLLVM,

  -- declarations
  freshName, -- declare, intrinsic,

  -- basic blocks
  Block,
  newBlock, setBlock, beginBlock, createBlocks,

  -- instructions
  instr, do_, return_, retval_, br, cbr, phi, phi',

  -- metadata
  addMetadata,

) where

-- standard library
import Control.Applicative
import Control.Monad.State.Strict
import Data.Function
import Data.HashMap.Strict                                              ( HashMap )
import Data.Map                                                         ( Map )
import Data.Sequence                                                    ( Seq )
import Data.Word
import Text.Printf
import qualified Data.Foldable                                          as Seq
import qualified Data.HashMap.Strict                                    as HashMap
import qualified Data.Map                                               as Map
import qualified Data.Sequence                                          as Seq

-- accelerate
import Data.Array.Accelerate.Error
import qualified Data.Array.Accelerate.Debug                            as Debug

-- accelerate-llvm
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Metadata
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand

import Data.Array.Accelerate.LLVM.Target
import Data.Array.Accelerate.LLVM.CodeGen.Downcast
import Data.Array.Accelerate.LLVM.CodeGen.Intrinsic
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.CodeGen.Sugar

-- llvm-general-pure
import qualified LLVM.General.AST                                       as LLVM
import qualified LLVM.General.AST.Global                                as LLVM
-- import qualified LLVM.General.AST.Instruction                           as LLVM


-- Code generation
-- ===============

-- | The code generation state for scalar functions and expressions.
--
-- We use two records: one to hold all the code generation state as it walks the
-- AST, and one for each of the basic blocks that are generated during the walk.
--
data CodeGenState = CodeGenState
  { blockChain          :: Seq Block                                    -- blocks for this function
  , symbolTable         :: Map Label LLVM.Global                        -- global (external) function declarations
  , metadataTable       :: HashMap String (Seq [Maybe Metadata])        -- module metadata to be collected
  , intrinsicTable      :: HashMap String Label                         -- standard math intrinsic functions
  , next                :: {-# UNPACK #-} !Word                         -- a name supply
  }

data Block = Block
  { blockLabel          :: Label                                        -- block label
  , instructions        :: Seq (LLVM.Named LLVM.Instruction)            -- stack of instructions
  , terminator          :: LLVM.Terminator                              -- block terminator
--  , returnType          :: LLVM.Type
  }

newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)


runLLVM
    :: forall arch aenv a. (Target arch, Intrinsic arch)
    => CodeGen (IROpenAcc arch aenv a)
    -> Module arch aenv a
runLLVM  ll =
  let
      (kernels, st)     = case runState (runCodeGen ll) initCodeGenState of
                            (IROpenAcc r, s) -> (map unKernel r, s)
      defs              = map LLVM.GlobalDefinition (kernels ++ Map.elems (symbolTable st))
                       ++ createMetadata (metadataTable st)

      name | x:_               <- kernels
           , f@LLVM.Function{} <- x
           , LLVM.Name s <- LLVM.name f = s
           | otherwise                  = "<undefined>"

  in
  Module $ LLVM.Module
    { LLVM.moduleName         = name
    , LLVM.moduleDataLayout   = targetDataLayout (undefined::arch)
    , LLVM.moduleTargetTriple = targetTriple (undefined::arch)
    , LLVM.moduleDefinitions  = defs
    }


initCodeGenState :: CodeGenState
initCodeGenState = CodeGenState
  { blockChain          = initBlockChain
  , symbolTable         = Map.empty
  , metadataTable       = HashMap.empty
  , intrinsicTable      = HashMap.empty
  , next                = 0
  }


-- Basic Blocks
-- ============

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


-- | Generate a new block and branch unconditionally to it.
--
beginBlock :: String -> CodeGen Block
beginBlock nm = do
  next <- newBlock nm
  _    <- br next
  setBlock next
  return next


-- | Extract the block state and construct the basic blocks that form a function
-- body. The block stream is re-initialised, but module-level state such as the
-- global symbol table is left intact.
--
createBlocks :: CodeGen [LLVM.BasicBlock]
createBlocks
  = state
  $ \s -> let s'     = s { blockChain = initBlockChain, next = 0 }
              blocks = makeBlock `fmap` blockChain s
              m      = Seq.length (blockChain s)
              n      = Seq.foldl' (\i b -> i + Seq.length (instructions b)) 0 (blockChain s)
          in
          trace (printf "generated %d instructions in %d blocks" (n+m) m) ( Seq.toList blocks , s' )
  where
    makeBlock Block{..} =
      LLVM.BasicBlock (downcast blockLabel) (Seq.toList instructions) (LLVM.Do terminator)


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
      bs Seq.:> b -> ( LocalReference (typeOf ins) name
                     , s { blockChain = bs Seq.|> b { instructions = instructions b Seq.|> downcast (name := ins) } } )


-- | Execute an unnamed instruction
--
do_ :: Instruction () -> CodeGen ()
do_ ins =
  modify $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> $internalError "do_" "empty block chain"
      bs Seq.:> b -> s { blockChain = bs Seq.|> b { instructions = instructions b Seq.|> downcast (Do ins) } }


-- | Return void from a basic block
--
return_ :: CodeGen ()
return_ = void $ terminate Ret

-- | Return a value from a basic block
--
retval_ :: Operand a -> CodeGen ()
retval_ x = void $ terminate (RetVal x)


-- | Unconditional branch. Return the name of the block that was branched from.
--
br :: Block -> CodeGen Block
br target = terminate $ Br (blockLabel target)


-- | Conditional branch. Return the name of the block that was branched from.
--
cbr :: Operand Bool -> Block -> Block -> CodeGen Block
cbr cond t f = terminate $ CondBr cond (blockLabel t) (blockLabel f)


-- | Add a phi node to the top of the current block
--
phi :: [(Operand a, Block)] -> CodeGen (Operand a)
phi incoming = do
  crit  <- freshName
  block <- state $ \s -> case Seq.viewr (blockChain s) of
                           Seq.EmptyR -> $internalError "phi" "empty block chain"
                           _ Seq.:> b -> ( b, s )
  phi' block crit incoming

phi' :: Block -> Name a -> [(Operand a, Block)] -> CodeGen (Operand a)
phi' target crit incoming =
  let cmp       = (==) `on` blockLabel
      update b  = b { instructions = downcast (crit := Phi ty [ (op,blockLabel) | (op,Block{..}) <- incoming ]) Seq.<| instructions b }
      ty        = case incoming of
                    []        -> $internalError "phi" "no incoming values specified"
                    (o,_):_   -> typeOf o
  in
  state $ \s ->
    case Seq.findIndexR (cmp target) (blockChain s) of
      Nothing -> $internalError "phi" "unknown basic block"
      Just i  -> ( LocalReference ty crit
                 , s { blockChain = Seq.adjust update i (blockChain s) } )


-- | Add a termination condition to the current instruction stream. Also return
-- the block that was just terminated.
--
terminate :: Terminator a -> CodeGen Block
terminate term =
  state $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> $internalError "terminate" "empty block chain"
      bs Seq.:> b -> ( b, s { blockChain = bs Seq.|> b { terminator = downcast term } } )


-- Metadata
-- ========

-- | Insert a metadata key/value pair into the current module.
--
addMetadata :: String -> [Maybe Metadata] -> CodeGen ()
addMetadata key val =
  modify $ \s ->
    s { metadataTable = HashMap.insertWith (flip (Seq.><)) key (Seq.singleton val) (metadataTable s) }


-- | Generate the metadata definitions for the file. Every key in the map
-- represents a named metadata definition. The values associated with that key
-- represent the metadata node definitions that will be attached to that
-- definition.
--
createMetadata :: HashMap String (Seq [Maybe Metadata]) -> [LLVM.Definition]
createMetadata md = build (HashMap.toList md) (Seq.empty, Seq.empty)
  where
    build :: [(String, Seq [Maybe Metadata])]
          -> (Seq LLVM.Definition, Seq LLVM.Definition) -- accumulator of (names, metadata)
          -> [LLVM.Definition]
    build []     (k,d) = Seq.toList (k Seq.>< d)
    build (x:xs) (k,d) =
      let (k',d') = meta (Seq.length d) x
      in  build xs (k Seq.|> k', d Seq.>< d')

    meta :: Int                                         -- number of metadata node definitions so far
         -> (String, Seq [Maybe Metadata])              -- current assoc of the metadata map
         -> (LLVM.Definition, Seq LLVM.Definition)
    meta n (key, vals)
      = let node i      = LLVM.MetadataNodeID (fromIntegral (i+n))
            nodes       = Seq.mapWithIndex (\i x -> LLVM.MetadataNodeDefinition (node i) (downcast (Seq.toList x))) vals
            name        = LLVM.NamedMetadataDefinition key [ node i | i <- [0 .. Seq.length vals - 1] ]
        in
        (name, nodes)


-- Debug
-- =====

{-# INLINE trace #-}
trace :: String -> a -> a
trace msg = Debug.trace Debug.dump_cc ("llvm: " ++ msg)

