{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Monad
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Monad (

  CodeGen,
  evalCodeGen,
  liftCodeGen,

  -- declarations
  fresh, freshName,
  declare,
  intrinsic,

  -- basic blocks
  Block,
  newBlock, setBlock, beginBlock, createBlocks,

  -- instructions
  instr, instr', do_, return_, retval_, br, cbr, phi, phi',
  instr_,

  -- metadata
  addMetadata,

) where

-- standard library
import Control.Applicative
import Control.Monad.State
import Data.ByteString.Short                                        ( ShortByteString )
import Data.Function
import Data.HashMap.Strict                                          ( HashMap )
import Data.Map                                                     ( Map )
import Data.Sequence                                                ( Seq )
import Data.String
import Data.Word
import Prelude
import Text.Printf
import qualified Data.Foldable                                      as F
import qualified Data.HashMap.Strict                                as HashMap
import qualified Data.Map                                           as Map
import qualified Data.Sequence                                      as Seq
import qualified Data.ByteString.Short                              as B

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Array.Sugar                            ( Elt, eltType )
import qualified Data.Array.Accelerate.Debug                        as Debug

-- accelerate-llvm
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Metadata
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation
import LLVM.AST.Type.Terminator

import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Intrinsic
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.State                             ( LLVM )
import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.CodeGen.Sugar                     ( IROpenAcc(..) )

-- llvm-hs
import qualified LLVM.AST                                           as LLVM
import qualified LLVM.AST.Global                                    as LLVM


-- Code generation
-- ===============

-- | The code generation state for scalar functions and expressions.
--
-- We use two records: one to hold all the code generation state as it walks the
-- AST, and one for each of the basic blocks that are generated during the walk.
--
data CodeGenState = CodeGenState
  { blockChain          :: Seq Block                                      -- blocks for this function
  , symbolTable         :: Map Label LLVM.Global                          -- global (external) function declarations
  , metadataTable       :: HashMap ShortByteString (Seq [Maybe Metadata]) -- module metadata to be collected
  , intrinsicTable      :: HashMap ShortByteString Label                  -- standard math intrinsic functions
  , next                :: {-# UNPACK #-} !Word                           -- a name supply
  }

data Block = Block
  { blockLabel          :: {-# UNPACK #-} !Label                          -- block label
  , instructions        :: Seq (LLVM.Named LLVM.Instruction)              -- stack of instructions
  , terminator          :: LLVM.Terminator                                -- block terminator
  }

newtype CodeGen target a = CodeGen { runCodeGen :: StateT CodeGenState (LLVM target) a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)

liftCodeGen :: LLVM arch a -> CodeGen arch a
liftCodeGen = CodeGen . lift


{-# INLINEABLE evalCodeGen #-}
evalCodeGen
    :: forall arch aenv a. (Target arch, Intrinsic arch)
    => CodeGen arch (IROpenAcc arch aenv a)
    -> LLVM    arch (Module    arch aenv a)
evalCodeGen ll = do
  (IROpenAcc ks, st)   <- runStateT (runCodeGen ll)
                        $ CodeGenState
                            { blockChain        = initBlockChain
                            , symbolTable       = Map.empty
                            , metadataTable     = HashMap.empty
                            , intrinsicTable    = intrinsicForTarget @arch
                            , next              = 0
                            }

  let (kernels, md)     = let (fs, as) = unzip [ (f , (LLVM.name f, a)) | Kernel f a <- ks ]
                          in  (fs, Map.fromList as)

      definitions       = map LLVM.GlobalDefinition (kernels ++ Map.elems (symbolTable st))
                       ++ createMetadata (metadataTable st)

      name | x:_               <- kernels
           , f@LLVM.Function{} <- x
           , LLVM.Name s       <- LLVM.name f = s
           | otherwise                        = "<undefined>"

  return $
    Module { moduleMetadata = md
           , unModule       = LLVM.Module
                            { LLVM.moduleName           = name
                            , LLVM.moduleSourceFileName = B.empty
                            , LLVM.moduleDataLayout     = targetDataLayout @arch
                            , LLVM.moduleTargetTriple   = targetTriple @arch
                            , LLVM.moduleDefinitions    = definitions
                            }
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
newBlock :: String -> CodeGen arch Block
newBlock nm =
  state $ \s ->
    let idx     = Seq.length (blockChain s)
        label   = let (h,t) = break (== '.') nm in (h ++ shows idx t)
        next    = Block (fromString label) Seq.empty err
        err     = $internalError label "Block has no terminator"
    in
    ( next, s )


-- | Add this block to the block stream. Any instructions pushed onto the stream
-- by 'instr' and friends will now apply to this block.
--
setBlock :: Block -> CodeGen arch ()
setBlock next =
  modify $ \s -> s { blockChain = blockChain s Seq.|> next }


-- | Generate a new block and branch unconditionally to it.
--
beginBlock :: String -> CodeGen arch Block
beginBlock nm = do
  next <- newBlock nm
  _    <- br next
  setBlock next
  return next


-- | Extract the block state and construct the basic blocks that form a function
-- body. The block stream is re-initialised, but module-level state such as the
-- global symbol table is left intact.
--
createBlocks :: CodeGen arch [LLVM.BasicBlock]
createBlocks
  = state
  $ \s -> let s'     = s { blockChain = initBlockChain, next = 0 }
              blocks = makeBlock `fmap` blockChain s
              m      = Seq.length (blockChain s)
              n      = F.foldl' (\i b -> i + Seq.length (instructions b)) 0 (blockChain s)
          in
          trace (printf "generated %d instructions in %d blocks" (n+m) m) ( F.toList blocks , s' )
  where
    makeBlock Block{..} =
      LLVM.BasicBlock (downcast blockLabel) (F.toList instructions) (LLVM.Do terminator)


-- Instructions
-- ------------

-- | Generate a fresh local reference
--
fresh :: forall arch a. Elt a => CodeGen arch (IR a)
fresh = IR <$> go (eltType @a)
  where
    go :: TupleType t -> CodeGen arch (Operands t)
    go TypeRunit         = return OP_Unit
    go (TypeRpair t2 t1) = OP_Pair <$> go t2 <*> go t1
    go (TypeRscalar t)   = ir' t . LocalReference (PrimType (ScalarPrimType t)) <$> freshName

-- | Generate a fresh (un)name.
--
freshName :: CodeGen arch (Name a)
freshName = state $ \s@CodeGenState{..} -> ( UnName next, s { next = next + 1 } )


-- | Add an instruction to the state of the currently active block so that it is
-- computed, and return the operand (LocalReference) that can be used to later
-- refer to it.
--
instr :: Instruction a -> CodeGen arch (IR a)
instr ins = ir (typeOf ins) <$> instr' ins

instr' :: Instruction a -> CodeGen arch (Operand a)
instr' ins =
  -- LLVM-5 does not allow instructions of type void to have a name.
  case typeOf ins of
    VoidType -> do
      do_ ins
      return $ LocalReference VoidType (Name B.empty)
    --
    ty -> do
      name <- freshName
      instr_ $ downcast (name := ins)
      return $ LocalReference ty name

-- | Execute an unnamed instruction
--
do_ :: Instruction () -> CodeGen arch ()
do_ ins = instr_ $ downcast (Do ins)

-- | Add raw assembly instructions to the execution stream
--
instr_ :: LLVM.Named LLVM.Instruction -> CodeGen arch ()
instr_ ins =
  modify $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> $internalError "instr_" "empty block chain"
      bs Seq.:> b -> s { blockChain = bs Seq.|> b { instructions = instructions b Seq.|> ins } }


-- | Return void from a basic block
--
return_ :: CodeGen arch ()
return_ = void $ terminate Ret

-- | Return a value from a basic block
--
retval_ :: Operand a -> CodeGen arch ()
retval_ x = void $ terminate (RetVal x)


-- | Unconditional branch. Return the name of the block that was branched from.
--
br :: Block -> CodeGen arch Block
br target = terminate $ Br (blockLabel target)


-- | Conditional branch. Return the name of the block that was branched from.
--
cbr :: IR Bool -> Block -> Block -> CodeGen arch Block
cbr cond t f = terminate $ CondBr (op scalarType cond) (blockLabel t) (blockLabel f)


-- | Add a phi node to the top of the current block
--
phi :: forall arch a. Elt a => [(IR a, Block)] -> CodeGen arch (IR a)
phi incoming = do
  crit  <- fresh
  block <- state $ \s -> case Seq.viewr (blockChain s) of
                           Seq.EmptyR -> $internalError "phi" "empty block chain"
                           _ Seq.:> b -> ( b, s )
  phi' block crit incoming

phi' :: forall arch a. Elt a => Block -> IR a -> [(IR a, Block)] -> CodeGen arch (IR a)
phi' target (IR crit) incoming = IR <$> go (eltType @a) crit [ (o,b) | (IR o, b) <- incoming ]
  where
    go :: TupleType t -> Operands t -> [(Operands t, Block)] -> CodeGen arch (Operands t)
    go TypeRunit OP_Unit _
      = return OP_Unit
    go (TypeRpair t2 t1) (OP_Pair n2 n1) inc
      = OP_Pair <$> go t2 n2 [ (x, b) | (OP_Pair x _, b) <- inc ]
                <*> go t1 n1 [ (y, b) | (OP_Pair _ y, b) <- inc ]
    go (TypeRscalar t) tup inc
      | LocalReference _ v <- op' t tup = ir' t <$> phi1 target v [ (op' t x, b) | (x, b) <- inc ]
      | otherwise                       = $internalError "phi" "expected critical variable to be local reference"


phi1 :: Block -> Name a -> [(Operand a, Block)] -> CodeGen arch (Operand a)
phi1 target crit incoming =
  let cmp       = (==) `on` blockLabel
      update b  = b { instructions = downcast (crit := Phi t [ (p,blockLabel) | (p,Block{..}) <- incoming ]) Seq.<| instructions b }
      t         = case incoming of
                    []        -> $internalError "phi" "no incoming values specified"
                    (o,_):_   -> case typeOf o of
                                   VoidType    -> $internalError "phi" "operand has void type"
                                   PrimType x  -> x
  in
  state $ \s ->
    case Seq.findIndexR (cmp target) (blockChain s) of
      Nothing -> $internalError "phi" "unknown basic block"
      Just i  -> ( LocalReference (PrimType t) crit
                 , s { blockChain = Seq.adjust update i (blockChain s) } )


-- | Add a termination condition to the current instruction stream. Also return
-- the block that was just terminated.
--
terminate :: Terminator a -> CodeGen arch Block
terminate term =
  state $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> $internalError "terminate" "empty block chain"
      bs Seq.:> b -> ( b, s { blockChain = bs Seq.|> b { terminator = downcast term } } )


-- | Add a global declaration to the symbol table
--
declare :: LLVM.Global -> CodeGen arch ()
declare g =
  let unique (Just q) | g /= q    = $internalError "global" "duplicate symbol"
                      | otherwise = Just g
      unique _                    = Just g

      name = case LLVM.name g of
               LLVM.Name n      -> Label n
               LLVM.UnName n    -> Label (fromString (show n))
  in
  modify (\s -> s { symbolTable = Map.alter unique name (symbolTable s) })


-- | Get name of the corresponding intrinsic function implementing a given C
-- function. If there is no mapping, the C function name is used.
--
intrinsic :: ShortByteString -> CodeGen arch Label
intrinsic key =
  state $ \s ->
    let name = HashMap.lookupDefault (Label key) key (intrinsicTable s)
    in  (name, s)



-- Metadata
-- ========

-- | Insert a metadata key/value pair into the current module.
--
addMetadata :: ShortByteString -> [Maybe Metadata] -> CodeGen arch ()
addMetadata key val =
  modify $ \s ->
    s { metadataTable = HashMap.insertWith (flip (Seq.><)) key (Seq.singleton val) (metadataTable s) }


-- | Generate the metadata definitions for the file. Every key in the map
-- represents a named metadata definition. The values associated with that key
-- represent the metadata node definitions that will be attached to that
-- definition.
--
createMetadata :: HashMap ShortByteString (Seq [Maybe Metadata]) -> [LLVM.Definition]
createMetadata md = build (HashMap.toList md) (Seq.empty, Seq.empty)
  where
    build :: [(ShortByteString, Seq [Maybe Metadata])]
          -> (Seq LLVM.Definition, Seq LLVM.Definition) -- accumulator of (names, metadata)
          -> [LLVM.Definition]
    build []     (k,d) = F.toList (k Seq.>< d)
    build (x:xs) (k,d) =
      let (k',d') = meta (Seq.length d) x
      in  build xs (k Seq.|> k', d Seq.>< d')

    meta :: Int                                         -- number of metadata node definitions so far
         -> (ShortByteString, Seq [Maybe Metadata])     -- current assoc of the metadata map
         -> (LLVM.Definition, Seq LLVM.Definition)
    meta n (key, vals)
      = let node i      = LLVM.MetadataNodeID (fromIntegral (i+n))
            nodes       = Seq.mapWithIndex (\i x -> LLVM.MetadataNodeDefinition (node i) (downcast (F.toList x))) vals
            name        = LLVM.NamedMetadataDefinition key [ node i | i <- [0 .. Seq.length vals - 1] ]
        in
        (name, nodes)


-- Debug
-- =====

{-# INLINE trace #-}
trace :: String -> a -> a
trace msg = Debug.trace Debug.dump_cc ("llvm: " ++ msg)

