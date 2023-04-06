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
-- Copyright   : [2015..2020] The Accelerate Team
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
  fresh, freshLocalName, freshGlobalName,
  declare,
  typedef,
  intrinsic,

  -- basic blocks
  Block,
  newBlock, setBlock, beginBlock, createBlocks,

  -- instructions
  instr, instr', do_, return_, retval_, br, cbr, switch, phi, phi', phi1,
  instr_,

  -- metadata
  addMetadata,

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Intrinsic
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Sugar                     ( IROpenAcc(..) )
import Data.Array.Accelerate.LLVM.State                             ( LLVM )
import Data.Array.Accelerate.LLVM.Target
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import qualified Data.Array.Accelerate.Debug.Internal               as Debug

import LLVM.AST.Orphans                                             ()
import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Metadata
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation
import LLVM.AST.Type.Terminator
import qualified LLVM.AST                                           as LLVM
import qualified LLVM.AST.Global                                    as LLVM

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.ByteString.Short                                        ( ShortByteString )
import Data.Function
import Data.HashMap.Strict                                          ( HashMap )
import Data.Sequence                                                ( Seq )
import Data.String
import Data.Text.Lazy.Builder                                       ( Builder )
import Formatting
import Prelude
import qualified Data.Foldable                                      as F
import qualified Data.HashMap.Strict                                as HashMap
import qualified Data.Sequence                                      as Seq
import qualified Data.ByteString.Short                              as B


-- Code generation
-- ===============

-- | The code generation state for scalar functions and expressions.
--
-- We use two records: one to hold all the code generation state as it walks the
-- AST, and one for each of the basic blocks that are generated during the walk.
--
data CodeGenState = CodeGenState
  { blockChain          :: Seq Block                                      -- blocks for this function
  , symbolTable         :: HashMap Label LLVM.Global                      -- global (external) function declarations
  , typedefTable        :: HashMap Label (Maybe LLVM.Type)                -- global type definitions
  , metadataTable       :: HashMap ShortByteString (Seq [Maybe Metadata]) -- module metadata to be collected
  , intrinsicTable      :: HashMap ShortByteString Label                  -- standard math intrinsic functions
  , local               :: {-# UNPACK #-} !Word                           -- a name supply
  , global              :: {-# UNPACK #-} !Word                           -- a name supply for global variables
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
    :: forall arch aenv a. (HasCallStack, Target arch, Intrinsic arch)
    => CodeGen arch (IROpenAcc arch aenv a)
    -> LLVM    arch (Module    arch aenv a)
evalCodeGen ll = do
  (IROpenAcc ks, st)   <- runStateT (runCodeGen ll)
                        $ CodeGenState
                            { blockChain        = initBlockChain
                            , symbolTable       = HashMap.empty
                            , typedefTable      = HashMap.empty
                            , metadataTable     = HashMap.empty
                            , intrinsicTable    = intrinsicForTarget @arch
                            , local             = 0
                            , global            = 0
                            }

  let (kernels, md)     = let (fs, as) = unzip [ (f , (LLVM.name f, a)) | Kernel f a <- ks ]
                          in  (fs, HashMap.fromList as)

      createTypedefs    = map (\(n,t) -> LLVM.TypeDefinition (downcast n) t) . HashMap.toList

      definitions       = createTypedefs (typedefTable st)
                       ++ map LLVM.GlobalDefinition (kernels ++ HashMap.elems (symbolTable st))
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
initBlockChain :: HasCallStack => Seq Block
initBlockChain
  = Seq.singleton
  $ Block "entry" Seq.empty (internalError "block has no terminator")


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
newBlock :: HasCallStack => String -> CodeGen arch Block
newBlock nm =
  state $ \s ->
    let idx     = Seq.length (blockChain s)
        label   = let (h,t) = break (== '.') nm in (h ++ shows idx t)
        next    = Block (fromString label) Seq.empty err
        err     = internalError ("block `" % string % "' has no terminator") label
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
beginBlock :: HasCallStack => String -> CodeGen arch Block
beginBlock nm = do
  next <- newBlock nm
  _    <- br next
  setBlock next
  return next


-- | Extract the block state and construct the basic blocks that form a function
-- body. The block stream is re-initialised, but module-level state such as the
-- global symbol table is left intact.
--
createBlocks :: HasCallStack => CodeGen arch [LLVM.BasicBlock]
createBlocks
  = state
  $ \s -> let s'     = s { blockChain = initBlockChain, local = 0 }
              blocks = makeBlock `fmap` blockChain s
              m      = Seq.length (blockChain s)
              n      = F.foldl' (\i b -> i + Seq.length (instructions b)) 0 (blockChain s)
          in
          trace (bformat ("generated " % int % " instructions in " % int % " blocks") (n+m) m) ( F.toList blocks , s' )
  where
    makeBlock Block{..} =
      LLVM.BasicBlock (downcast blockLabel) (F.toList instructions) (LLVM.Do terminator)


-- Instructions
-- ------------

-- | Generate a fresh local reference
--
fresh :: TypeR a -> CodeGen arch (Operands a)
fresh TupRunit         = return OP_Unit
fresh (TupRpair t2 t1) = OP_Pair <$> fresh t2 <*> fresh t1
fresh (TupRsingle t)   = ir t . LocalReference (PrimType (ScalarPrimType t)) <$> freshLocalName

-- | Generate a fresh local (un)name
--
freshLocalName :: CodeGen arch (Name a)
freshLocalName = state $ \s@CodeGenState{..} -> ( UnName local, s { local = local + 1 } )

-- | Generate a fresh global (un)name
--
freshGlobalName :: CodeGen arch (Name a)
freshGlobalName = state $ \s@CodeGenState{..} -> ( UnName global, s { global = global + 1 } )


-- | Add an instruction to the state of the currently active block so that it is
-- computed, and return the operand (LocalReference) that can be used to later
-- refer to it.
--
instr :: HasCallStack => Instruction a -> CodeGen arch (Operands a)
instr ins = ir (typeOf ins) <$> instr' ins

instr' :: HasCallStack => Instruction a -> CodeGen arch (Operand a)
instr' ins =
  -- LLVM-5 does not allow instructions of type void to have a name.
  case typeOf ins of
    VoidType -> do
      do_ ins
      return $ LocalReference VoidType (Name B.empty)
    --
    ty -> do
      name <- freshLocalName
      instr_ $ downcast (name := ins)
      return $ LocalReference ty name

-- | Execute an unnamed instruction
--
do_ :: HasCallStack => Instruction () -> CodeGen arch ()
do_ ins = instr_ $ downcast (Do ins)

-- | Add raw assembly instructions to the execution stream
--
instr_ :: HasCallStack => LLVM.Named LLVM.Instruction -> CodeGen arch ()
instr_ ins =
  modify $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> internalError "empty block chain"
      bs Seq.:> b -> s { blockChain = bs Seq.|> b { instructions = instructions b Seq.|> ins } }


-- | Return void from a basic block
--
return_ :: HasCallStack => CodeGen arch ()
return_ = void $ terminate Ret

-- | Return a value from a basic block
--
retval_ :: HasCallStack => Operand a -> CodeGen arch ()
retval_ x = void $ terminate (RetVal x)


-- | Unconditional branch. Return the name of the block that was branched from.
--
br :: HasCallStack => Block -> CodeGen arch Block
br target = terminate $ Br (blockLabel target)


-- | Conditional branch. Return the name of the block that was branched from.
--
cbr :: HasCallStack => Operands Bool -> Block -> Block -> CodeGen arch Block
cbr (OP_Bool cond) t f = terminate $ CondBr cond (blockLabel t) (blockLabel f)

-- | Switch statement. Return the name of the block that was branched from.
--
switch :: HasCallStack => Operands TAG -> Block -> [(TAG, Block)] -> CodeGen arch Block
switch tag def eqs = terminate $ Switch (op scalarType tag) (blockLabel def) [(ScalarConstant scalarType t, blockLabel b) | (t,b) <- eqs]

-- | Add a phi node to the top of the current block
--
phi :: forall arch a. HasCallStack => TypeR a -> [(Operands a, Block)] -> CodeGen arch (Operands a)
phi tp incoming = do
  crit  <- fresh tp
  block <- state $ \s -> case Seq.viewr (blockChain s) of
                           Seq.EmptyR -> internalError "empty block chain"
                           _ Seq.:> b -> ( b, s )
  phi' tp block crit incoming

phi' :: HasCallStack => TypeR a -> Block -> Operands a -> [(Operands a, Block)] -> CodeGen arch (Operands a)
phi' tp target = go tp
  where
    go :: TypeR t -> Operands t -> [(Operands t, Block)] -> CodeGen arch (Operands t)
    go TupRunit OP_Unit _
      = return OP_Unit
    go (TupRpair t2 t1) (OP_Pair n2 n1) inc
      = OP_Pair <$> go t2 n2 [ (x, b) | (OP_Pair x _, b) <- inc ]
                <*> go t1 n1 [ (y, b) | (OP_Pair _ y, b) <- inc ]
    go (TupRsingle t) tup inc
      | LocalReference _ v <- op t tup = ir t <$> phi1 target v [ (op t x, b) | (x, b) <- inc ]
      | otherwise                      = internalError "expected critical variable to be local reference"


phi1 :: HasCallStack => Block -> Name a -> [(Operand a, Block)] -> CodeGen arch (Operand a)
phi1 target crit incoming =
  let cmp       = (==) `on` blockLabel
      update b  = b { instructions = downcast (crit := Phi t [ (p,blockLabel) | (p,Block{..}) <- incoming ]) Seq.<| instructions b }
      t         = case incoming of
                    []        -> internalError "no incoming values specified"
                    (o,_):_   -> case typeOf o of
                                   VoidType    -> internalError "operand has void type"
                                   PrimType x  -> x
  in
  state $ \s ->
    case Seq.findIndexR (cmp target) (blockChain s) of
      Nothing -> internalError "unknown basic block"
      Just i  -> ( LocalReference (PrimType t) crit
                 , s { blockChain = Seq.adjust update i (blockChain s) } )


-- | Add a termination condition to the current instruction stream. Also return
-- the block that was just terminated.
--
terminate :: HasCallStack => Terminator a -> CodeGen arch Block
terminate term =
  state $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> internalError "empty block chain"
      bs Seq.:> b -> ( b, s { blockChain = bs Seq.|> b { terminator = downcast term } } )


-- | Add a global declaration to the symbol table
--
declare :: HasCallStack => LLVM.Global -> CodeGen arch ()
declare g =
  let unique (Just q) | g /= q    = internalError "duplicate symbol"
                      | otherwise = Just g
      unique _                    = Just g

      name = case LLVM.name g of
               LLVM.Name n      -> Label n
               LLVM.UnName n    -> Label (fromString (show n))
  in
  modify (\s -> s { symbolTable = HashMap.alter unique name (symbolTable s) })


-- | Add a global type definition
--
typedef :: HasCallStack => Label -> Maybe LLVM.Type -> CodeGen arch ()
typedef name t =
  let unique (Just s) | t /= s    = internalError "duplicate typedef"
                      | otherwise = Just t
      unique _                    = Just t
  in
  modify (\s -> s { typedefTable = HashMap.alter unique name (typedefTable s) })


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
createMetadata md = go (HashMap.toList md) (Seq.empty, Seq.empty)
  where
    go :: [(ShortByteString, Seq [Maybe Metadata])]
       -> (Seq LLVM.Definition, Seq LLVM.Definition) -- accumulator of (names, metadata)
       -> [LLVM.Definition]
    go []     (k,d) = F.toList (k Seq.>< d)
    go (x:xs) (k,d) =
      let (k',d') = meta (Seq.length d) x
      in  go xs (k Seq.|> k', d Seq.>< d')

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
trace :: Builder -> a -> a
trace msg = Debug.trace Debug.dump_cc ("llvm: " <> msg)

