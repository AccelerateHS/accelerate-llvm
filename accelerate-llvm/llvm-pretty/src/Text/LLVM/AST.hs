{- |
Because this library supports many LLVM versions, it is possible to construct
an AST with the types in this module that only some LLVM versions will accept.
These cases are usually documented in the Haddocks for the relevant data types.
When trying to pretty-print constructions that are unsupported by the current
LLVM version, pretty-printing may 'error'.

At the same time, while the AST coverage is fairly extensive, it is also
incomplete: there are some values that new LLVM versions would accept but are
not yet represented here.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Text.LLVM.AST
  ( -- * Modules
    Module(..)
  , emptyModule
    -- * Named Metadata
  , NamedMd(..)
    -- * Unnamed Metadata
  , UnnamedMd(..)
    -- * Aliases
  , GlobalAlias(..)
    -- * Data Layout
  , DataLayout
  , LayoutSpec(..)
  , Alignment(..)
  , FunctionPointerAlignType(..)
  , Storage(..)
  , PointerSize(..)
  , AddressSpace
  , NumBits
  , Mangling(..)
  , parseDataLayout
    -- * Inline Assembly
  , InlineAsm
    -- * Comdat
  , SelectionKind(..)
    -- * Identifiers
  , Ident(..)
    -- * Symbols
  , Symbol(..)
    -- * Types
  , PrimType(..)
  , FloatType(..)
  , Type, Type'(..)
  , AddrSpace(..)
  , defaultAddrSpace
  , updateAliasesA, updateAliases
  , isFloatingPoint
  , isAlias
  , isPrimTypeOf
  , isLabel
  , isInteger
  , isVector
  , isVectorOf
  , isArray
  , isPointer
  , eqTypeModuloOpaquePtrs
  , cmpTypeModuloOpaquePtrs
  , fixupOpaquePtrs
    -- * Null values
  , NullResult(..)
  , primTypeNull
  , floatTypeNull
  , typeNull
    -- * Type Elimination
  , elimFunTy
  , elimAlias
  , elimPtrTo
  , elimVector
  , elimArray
  , elimFunPtr
  , elimPrimType
  , elimFloatType
  , elimSequentialType
    -- * Top-level Type Aliases
  , TypeDecl(..)
    -- * Globals
  , Global(..)
  , addGlobal
  , GlobalAttrs(..)
  , emptyGlobalAttrs
    -- * Declarations
  , Declare(..)
  , decFunType
    -- * Function Definitions
  , Define(..)
  , defFunType, addDefine
    -- * Function Attributes and attribute groups
  , FunAttr(..)
    -- * Basic Block Labels
  , BlockLabel(..)
    -- * Basic Blocks
  , BasicBlock'(..), BasicBlock
  , brTargets
    -- * Attributes
  , Linkage(..)
  , Visibility(..)
  , GC(..)
    -- * Typed Things
  , Typed(..)
  , mapMTyped
    -- * Instructions
  , ArithOp(..)
  , isIArith
  , isFArith
  , UnaryArithOp(..)
  , BitOp(..)
  , ConvOp(..)
  , AtomicRWOp(..)
  , AtomicOrdering(..)
  , Align
  , Instr'(..), Instr
  , Clause'(..), Clause
  , isTerminator
  , isComment
  , isPhi
  , ICmpOp(..)
  , FCmpOp(..)
  , FMF(..)
    -- * Values
  , Value'(..), Value
  , FP80Value(..)
  , ValMd'(..), ValMd
  , KindMd
  , FnMdAttachments
  , GlobalMdAttachments
  , DebugLoc'(..), DebugLoc
  , isConst
    -- * Value Elimination
  , elimValSymbol
  , elimValInteger
    -- * Statements
  , Stmt'(..), Stmt
  , stmtInstr
  , stmtMetadata
  , extendMetadata
  , addDebugRecord
    -- * Constant Expressions
  , ConstExpr'(..), ConstExpr
  , GEPAttr(..)
  , orderedGEPAttrs
  , RangeSpec(RangeIndex, Range)
    -- * DWARF Debug Info
  , DebugInfo'(..), DebugInfo
  , DILabel, DILabel'(..)
  , DIImportedEntity, DIImportedEntity'(..)
  , DITemplateTypeParameter, DITemplateTypeParameter'(..)
  , DITemplateValueParameter, DITemplateValueParameter'(..)
  , DINameSpace, DINameSpace'(..)
  , DwarfAttrEncoding
  , DwarfLang
  , DwarfTag
  , DwarfVirtuality
  , DIFlags
  , DIEmissionKind
  , DIBasicType'(..), DIBasicType
  , DICompileUnit'(..), DICompileUnit
  , DICompositeType'(..), DICompositeType
  , DIDerivedType'(..), DIDerivedType
  , DIExpression(..)
  , DIFile(..)
  , DIGlobalVariable'(..), DIGlobalVariable
  , DIGlobalVariableExpression'(..), DIGlobalVariableExpression
  , DILexicalBlock'(..), DILexicalBlock
  , DILexicalBlockFile'(..), DILexicalBlockFile
  , DILocalVariable'(..), DILocalVariable
  , DISubprogram'(..), DISubprogram
  , DISubrange'(..), DISubrange
  , DISubroutineType'(..), DISubroutineType
  , DIArgList'(..), DIArgList
  , dwarf_DW_APPLE_ENUM_KIND_invalid
  , DebugRecord, DebugRecord'(..)
  , DbgRecAssign, DbgRecAssign'(..)
  , DbgRecDeclare, DbgRecDeclare'(..)
  , DbgRecLabel, DbgRecLabel'(..)
  , DbgRecValueSimple, DbgRecValueSimple'(..)
  , DbgRecValue, DbgRecValue'(..)
    -- * Aggregate Utilities
  , IndexResult(..)
  , isInvalid
  , resolveGepFull
  , resolveGep
  , resolveGepBody
  , isGepIndex
  , isGepStructIndex
  , resolveValueIndex
  ) where

import Control.Monad (MonadPlus(mzero,mplus),(<=<),guard)
import Data.Bits ( complement )
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Functor.Identity (Identity(..))
import Data.Generics (everywhere, extQ, mkT, something)
import Data.Int (Int32,Int64)
import Data.List (genericIndex,genericLength)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Semigroup as Sem
import Data.String (IsString(fromString))
import Data.Typeable (Typeable)
import Data.Word (Word8,Word16,Word32,Word64)
import GHC.Generics (Generic, Generic1)
import Language.Haskell.TH.Syntax (Lift)

import Text.Parsec
import Text.Parsec.String

import Text.LLVM.Triple.AST (TargetTriple)


-- Modules ---------------------------------------------------------------------

data Module = Module
  { modSourceName :: Maybe String
  , modTriple     :: TargetTriple  -- ^ target triple
  , modDataLayout :: DataLayout    -- ^ type size and alignment information
  , modTypes      :: [TypeDecl]    -- ^ top-level type aliases
  , modNamedMd    :: [NamedMd]
  , modUnnamedMd  :: [UnnamedMd]
  , modComdat     :: Map.Map String SelectionKind
  , modGlobals    :: [Global]      -- ^ global value declarations
  , modDeclares   :: [Declare]     -- ^ external function declarations (without definitions)
  , modDefines    :: [Define]      -- ^ internal function declarations (with definitions)
  , modInlineAsm  :: InlineAsm
  , modAliases    :: [GlobalAlias]
  } deriving (Data, Eq, Ord, Generic, Show, Typeable)

-- | Combines fields pointwise.
instance Sem.Semigroup Module where
  m1 <> m2 = Module
    { modSourceName = modSourceName m1 `mplus`   modSourceName m2
    , modTriple     = modTriple m1     <> modTriple     m2
    , modDataLayout = modDataLayout m1 <> modDataLayout m2
    , modTypes      = modTypes      m1 <> modTypes      m2
    , modUnnamedMd  = modUnnamedMd  m1 <> modUnnamedMd  m2
    , modNamedMd    = modNamedMd    m1 <> modNamedMd    m2
    , modGlobals    = modGlobals    m1 <> modGlobals    m2
    , modDeclares   = modDeclares   m1 <> modDeclares   m2
    , modDefines    = modDefines    m1 <> modDefines    m2
    , modInlineAsm  = modInlineAsm  m1 <> modInlineAsm  m2
    , modAliases    = modAliases    m1 <> modAliases    m2
    , modComdat     = modComdat     m1 <> modComdat     m2
    }

instance Monoid Module where
  mempty = emptyModule
  mappend = (<>)

emptyModule :: Module
emptyModule  = Module
  { modSourceName = mempty
  , modTriple     = mempty
  , modDataLayout = mempty
  , modTypes      = mempty
  , modNamedMd    = mempty
  , modUnnamedMd  = mempty
  , modGlobals    = mempty
  , modDeclares   = mempty
  , modDefines    = mempty
  , modInlineAsm  = mempty
  , modAliases    = mempty
  , modComdat     = mempty
  }


-- Named Metadata --------------------------------------------------------------

data NamedMd = NamedMd
  { nmName   :: String
  , nmValues :: [Int]
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- Unnamed Metadata ------------------------------------------------------------

data UnnamedMd = UnnamedMd
  { umIndex    :: !Int
  , umValues   :: ValMd
  , umDistinct :: Bool
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- Aliases ---------------------------------------------------------------------

data GlobalAlias = GlobalAlias
  { aliasLinkage    :: Maybe Linkage
  , aliasVisibility :: Maybe Visibility
  , aliasName       :: Symbol
  , aliasType       :: Type
  , aliasTarget     :: Value
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- Data Layout -----------------------------------------------------------------
-- https://releases.llvm.org/19.1.0/docs/LangRef.html#data-layout

type DataLayout = [LayoutSpec]

data LayoutSpec
  = BigEndian
  | LittleEndian
  | PointerSize PointerSize
  | IntegerSize Storage
  | VectorSize Storage
  | FloatSize Storage
  | StackObjSize  Storage
  | AggregateSize (Maybe Int) !Alignment -- n.b. first Int present pre-LLVM4
  | NativeIntSize [NumBits]
  | StackAlign    !NumBits -- ^ size
  | ProgramAddrSpace !AddressSpace
  | GlobalAddrSpace !AddressSpace
  | AllocaAddrSpace !AddressSpace
  | FunctionPointerAlign !FunctionPointerAlignType !NumBits -- ^ type, abi
  | Mangling Mangling
  | NonIntegralPointerSpaces [AddressSpace]
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Alignment = Alignment
  { alignABI :: !NumBits
  , alignPreferred :: Maybe NumBits  -- ^ default = alignABI
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | How should a function pointer be aligned?
data FunctionPointerAlignType
  = IndependentOfFunctionAlign
    -- ^ The alignment of function pointers is independent of the alignment of
    -- functions.
  | MultipleOfFunctionAlign
    -- ^ The alignment of function pointers is a multiple of the explicit
    -- alignment specified on the function.
  deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

data Storage = Storage
  { storageSize :: !NumBits  -- ^ valid range [1,2^24)
  , storageAlignment :: Alignment
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data PointerSize = PtrSize
  { ptrAddrSpace :: !AddressSpace
  , ptrStorage :: Storage
  , ptrAddrIndexSize :: Maybe NumBits  -- ^ m.b. <= ptrSize, default = ptrSize
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

type AddressSpace = Int
type NumBits = Int

data Mangling = ElfMangling
              | GoffMangling
              | MipsMangling
              | MachOMangling
              | WindowsCoffMangling
              | WindowsX86CoffMangling
              | XCoffMangling
                deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

-- | Parse the data layout string.
parseDataLayout :: MonadPlus m => String -> m DataLayout
parseDataLayout str =
  case parse (pDataLayout <* eof) "<internal>" str of
    Left _err -> {- debugging: trace (show err) -} mzero
    Right specs -> return specs
  where
    pDataLayout :: Parser DataLayout
    pDataLayout = sepBy pLayoutSpec (char '-')

    pLayoutSpec :: Parser LayoutSpec
    pLayoutSpec =
      do c <- letter
         case c of
           'E' -> return BigEndian
           'e' -> return LittleEndian
           'S' -> StackAlign <$> pInt
           'P' -> ProgramAddrSpace <$> pInt -- Added in LLVM7
           'G' -> GlobalAddrSpace <$> pInt -- Added in LLVM11
           'A' -> AllocaAddrSpace <$> pInt -- Added in LLVM11
           'p' -> do as <- pInt <|> return 0
                     st <- char ':' >> pStorage
                     idx <- pCOInt -- Added in LLVM7
                     return $ PointerSize $ PtrSize as st idx
           'i' -> IntegerSize <$> pStorage
           'v' -> VectorSize <$> pStorage
           'f' -> FloatSize <$> pStorage
                  -- Note that the data layout specified in the LLVM
                  -- BC/IR file is not a directive to the backend, but
                  -- is instead an indication of what the particular
                  -- backend chosen expects to receive.  The actual
                  -- floating point size and alignment is specified as
                  -- zero or more "fSZ:A1:A2" portions of the
                  -- datawidth, where SZ is the size, A1 is the ABI
                  -- alignment, and A2 is the preferred alignment
                  -- (defaulting to A1 if not specified).  Not
                  -- included in the data layout is the actual width,
                  -- alignment, and format for implementation.  See
                  -- (for example) references to LongDoubleWidth and
                  -- LongDoubleFormat in
                  -- https://github.com/llvm/llvm-project/blob/release_60/clang/lib/Basic/Targets/X86.h
           's' -> StackObjSize <$> pStorage -- Obsoleted in LLVM4
           'a' -> alphaNum >>= \case
             ':' -> AggregateSize Nothing <$> pAlignment
             d   -> AggregateSize <$> (Just <$> pIntWithFirstDigit d)
                    <* char ':' <*> pAlignment
           'F' -> FunctionPointerAlign <$> pFunctionPointerAlignType <*> pInt -- Added in LLVM9
           'm' -> Mangling <$ char ':' <*> pMangling
           'n' -> alphaNum >>= \case
             'i' -> char ':'
                    >> (NonIntegralPointerSpaces <$> sepBy pInt (char ':'))
             d -> do fs <- pIntWithFirstDigit d
                     ss <- char ':' *> sepBy pInt (char ':')
                     return $ NativeIntSize $ fs : ss
           _   -> mzero

    pFunctionPointerAlignType :: Parser FunctionPointerAlignType
    pFunctionPointerAlignType =
      do c <- letter
         case c of
           'i' -> return IndependentOfFunctionAlign
           'n' -> return MultipleOfFunctionAlign
           _   -> mzero

    pMangling :: Parser Mangling
    pMangling =
      do c <- letter
         case c of
           'e' -> return ElfMangling
           'l' -> return GoffMangling
           'm' -> return MipsMangling
           'o' -> return MachOMangling
           'w' -> return WindowsCoffMangling
           'x' -> return WindowsX86CoffMangling
           'a' -> return XCoffMangling
           _   -> mzero

    pAlignment :: Parser Alignment
    pAlignment = Alignment <$> pInt <*> pCOInt

    pStorage :: Parser Storage
    pStorage = Storage <$> pInt <* char ':' <*> pAlignment

    pInt :: Parser Int
    pInt = read <$> many1 digit

    pIntWithFirstDigit :: Char -> Parser Int
    pIntWithFirstDigit d0 = read . (d0:) <$> many digit

    pCInt :: Parser Int
    pCInt = char ':' >> pInt

    pCOInt :: Parser (Maybe Int)
    pCOInt = optionMaybe pCInt

-- Inline Assembly -------------------------------------------------------------

type InlineAsm = [String]

-- Comdat ----------------------------------------------------------------------

data SelectionKind = ComdatAny
                   | ComdatExactMatch
                   | ComdatLargest
                   | ComdatNoDuplicates
                   | ComdatSameSize
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

-- Identifiers -----------------------------------------------------------------

newtype Ident = Ident String
    deriving (Data, Eq, Generic, Ord, Show, Typeable, Lift)

instance IsString Ident where
  fromString = Ident

-- Symbols ---------------------------------------------------------------------

newtype Symbol = Symbol String
    deriving (Data, Eq, Generic, Ord, Show, Typeable, Lift)

instance Sem.Semigroup Symbol where
  Symbol a <> Symbol b = Symbol (a <> b)

instance Monoid Symbol where
  mappend = (<>)
  mempty  = Symbol mempty

instance IsString Symbol where
  fromString = Symbol

-- Types -----------------------------------------------------------------------

data PrimType
  = Label
  | Void
  | Integer Word32
  | FloatType FloatType
  | X86mmx
  | Metadata
    deriving (Data, Eq, Generic, Ord, Show, Typeable, Lift)

data FloatType
  = Half
  | Float
  | Double
  | Fp128
  | X86_fp80
  | PPC_fp128
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable, Lift)

type Type = Type' Ident

data Type' ident
  = PrimType PrimType
  | Alias ident
  | Array Word64 (Type' ident)
  | FunTy (Type' ident) [Type' ident] Bool
  | PtrTo (Type' ident) AddrSpace
    -- ^ A pointer to a memory location of a particular type. See also
    -- 'PtrOpaque', which represents a pointer without a pointee type.
  | PtrOpaque AddrSpace
    -- ^ A pointer to a memory location. Unlike 'PtrTo', a 'PtrOpaque' does not
    -- have a pointee type. Instead, instructions interacting through opaque
    -- pointers specify the type of the underlying memory they are interacting
    -- with.
    --
    -- 'PtrOpaque' should not be confused with 'Opaque', which is a completely
    -- separate type with a similar-sounding name.
  | Struct [Type' ident]
  | PackedStruct [Type' ident]
  | Vector Word64 (Type' ident)
  | Opaque
    -- ^ An opaque structure type, used to represent structure types that do not
    -- have a body specified. This is similar to C's notion of a
    -- forward-declared structure.
    --
    -- 'Opaque' should not be confused with 'PtrOpaque', which is a completely
    -- separate type with a similar-sounding name.
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

data AddrSpace = AddrSpace Word32
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

defaultAddrSpace :: AddrSpace
defaultAddrSpace = AddrSpace 0

-- | Applicatively traverse a type, updating or removing aliases.
updateAliasesA :: (Applicative f) => (a -> f (Type' b)) -> Type' a -> f (Type' b)
updateAliasesA f = loop
  where
  loop ty = case ty of
    Array len ety    -> Array len    <$> (loop ety)
    FunTy res ps var -> FunTy        <$> (loop res) <*> (traverse loop ps) <*> pure var
    PtrTo pty addr   -> PtrTo        <$> (loop pty) <*> pure addr
    PtrOpaque addr   -> pure (PtrOpaque addr)
    Struct fs        -> Struct       <$> (traverse loop fs)
    PackedStruct fs  -> PackedStruct <$> (traverse loop fs)
    Vector len ety   -> Vector       <$> pure len <*> (loop ety)
    PrimType pty     -> pure $ PrimType pty
    Opaque           -> pure $ Opaque
    Alias lab        -> f lab

-- | Traverse a type, updating or removing aliases.
updateAliases :: (a -> Type' b) -> Type' a -> Type' b
updateAliases f = coerce $ updateAliasesA (Identity . f)

isFloatingPoint :: PrimType -> Bool
isFloatingPoint (FloatType _) = True
isFloatingPoint _             = False

isAlias :: Type' ident -> Bool
isAlias Alias{} = True
isAlias _       = False

isPrimTypeOf :: (PrimType -> Bool) -> Type' ident -> Bool
isPrimTypeOf p (PrimType pt) = p pt
isPrimTypeOf _ _             = False

isLabel :: PrimType -> Bool
isLabel Label = True
isLabel _     = False

isInteger :: PrimType -> Bool
isInteger Integer{} = True
isInteger _         = False

isVector :: Type' ident -> Bool
isVector Vector{} = True
isVector _        = False

isVectorOf :: (Type' ident -> Bool) -> Type' ident -> Bool
isVectorOf p (Vector _ e) = p e
isVectorOf _ _            = False

isArray :: Type' ident -> Bool
isArray ty = case ty of
  Array _ _ -> True
  _         -> False

isPointer :: Type' ident -> Bool
isPointer PtrTo{}     = True
isPointer PtrOpaque{} = True
isPointer _           = False


-- | Like `Type'`, but where the 'PtrTo' and 'PtrOpaque' constructors have been
-- collapsed into a single 'PtrView' constructor. This provides a coarser notion
-- of type equality than what `Type'` provides, which distinguishes the two
-- types of pointers.
--
-- `TypeView'` is not used directly in any of the other AST types. Instead, it
-- is used only as an internal data type to power the 'eqTypeModuloOpaquePtrs'
-- and 'cmpTypeModuloOpaquePtrs' functions.
data TypeView' ident
  = PrimTypeView PrimType
  | AliasView ident
  | ArrayView Word64 (TypeView' ident)
  | FunTyView (TypeView' ident) [TypeView' ident] Bool
  | PtrView
    -- ^ The sole pointer type. Both 'PtrTo' and 'PtrOpaque' are mapped to
    -- 'PtrView'.
  | StructView [TypeView' ident]
  | PackedStructView [TypeView' ident]
  | VectorView Word64 (TypeView' ident)
  | OpaqueView
    -- ^ An opaque structure type, used to represent structure types that do not
    -- forward-declared structure.
    --
    -- 'OpaqueView' should not be confused with opaque pointers, which are
    -- mapped to 'PtrView'.
    deriving (Eq, Ord)

-- | Convert a `Type'` value to a `TypeView'` value.
typeView :: Type' ident -> TypeView' ident
-- The two most important cases. Both forms of pointers are mapped to PtrView.
typeView PtrTo{}             = PtrView
typeView PtrOpaque{}         = PtrView
-- All other cases are straightforward.
typeView (PrimType pt)       = PrimTypeView pt
typeView (Alias lab)         = AliasView lab
typeView (Array len et)      = ArrayView len (typeView et)
typeView (FunTy ret args va) = FunTyView (typeView ret) (map typeView args) va
typeView (Struct fs)         = StructView (map typeView fs)
typeView (PackedStruct fs)   = PackedStructView (map typeView fs)
typeView (Vector len et)     = VectorView len (typeView et)
typeView Opaque              = OpaqueView

-- | Check two 'Type's for equality, but treat 'PtrOpaque' types as being equal
-- to @'PtrTo' ty@ types (for any type @ty@). This is a coarser notion of
-- equality than what is provided by the 'Eq' instance for 'Type'.
eqTypeModuloOpaquePtrs :: Eq ident => Type' ident -> Type' ident -> Bool
eqTypeModuloOpaquePtrs x y = typeView x == typeView y

-- | Compare two 'Type's, but treat 'PtrOpaque' types as being equal to
-- @'PtrTo' ty@ types (for any type @ty@). This is a coarser notion of ordering
-- than what is provided by the 'Ord' instance for 'Type'.
cmpTypeModuloOpaquePtrs :: Ord ident => Type' ident -> Type' ident -> Ordering
cmpTypeModuloOpaquePtrs x y = typeView x `compare` typeView y

-- | Ensure that if there are any occurrences of opaque pointers, then all
-- non-opaque pointers are converted to opaque ones.
--
-- This is useful because LLVM tools like @llvm-as@ are stricter than
-- @llvm-pretty@ in that the former forbids mixing opaque and non-opaque
-- pointers, whereas the latter allows this. As a result, the result of
-- pretty-printing an @llvm-pretty@ AST might not be suitable for @llvm-as@'s
-- needs unless you first call this function to ensure that the two types of
-- pointers are not intermixed.
--
-- This is implemented using "Data.Data" combinators under the hood, which could
-- potentially require a full traversal of the AST. Because of the performance
-- implications of this, we do not call 'fixupOpaquePtrs' in @llvm-pretty@'s
-- pretty-printer. If you wish to combine opaque and non-opaque pointers in your
-- AST, the burden is on you to call this function before pretty-printing.
fixupOpaquePtrs :: Data a => a -> a
fixupOpaquePtrs m
    | isJust (gfind isOpaquePtr m)
    = everywhere (mkT opaquifyPtr) m
    | otherwise
    = m
  where
    isOpaquePtr :: Type -> Bool
    isOpaquePtr PtrOpaque{} = True
    isOpaquePtr _           = False

    opaquifyPtr :: Type -> Type
    opaquifyPtr (PtrTo _ addr) = PtrOpaque addr
    opaquifyPtr t              = t

    -- Find the first occurrence of a @b@ value within the @a@ value that
    -- satisfies the predicate and return it with 'Just'. Return 'Nothing' if there
    -- are no such occurrences.
    gfind :: (Data a, Typeable b) => (b -> Bool) -> a -> Maybe b
    gfind p = something (const Nothing `extQ` \x -> if p x then Just x else Nothing)

-- Null Values -----------------------------------------------------------------

data NullResult lab
  = HasNull (Value' lab)
  | ResolveNull Ident
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

primTypeNull :: PrimType -> Value' lab
primTypeNull (Integer 1)    = ValBool False
primTypeNull (Integer _)    = ValInteger 0
primTypeNull (FloatType ft) = floatTypeNull ft
primTypeNull _              = ValZeroInit

floatTypeNull :: FloatType -> Value' lab
floatTypeNull Float    = ValFloat 0
floatTypeNull Double   = ValDouble 0 -- XXX not sure about this
floatTypeNull X86_fp80 = ValFP80 $ FP80_LongDouble 0 0
floatTypeNull _        = error "must be a float type"

typeNull :: Type -> NullResult lab
typeNull (PrimType pt) = HasNull (primTypeNull pt)
typeNull PtrTo{}       = HasNull ValNull
typeNull PtrOpaque{}   = HasNull ValNull
typeNull (Alias i)     = ResolveNull i
typeNull _             = HasNull ValZeroInit

-- Type Elimination ------------------------------------------------------------

elimFunTy :: MonadPlus m => Type -> m (Type,[Type],Bool)
elimFunTy (FunTy ret args va) = return (ret,args,va)
elimFunTy _                   = mzero

elimAlias :: MonadPlus m => Type -> m Ident
elimAlias (Alias i) = return i
elimAlias _         = mzero

elimPtrTo :: MonadPlus m => Type -> m Type
elimPtrTo (PtrTo ty _) = return ty
elimPtrTo _            = mzero

elimVector :: MonadPlus m => Type -> m (Word64,Type)
elimVector (Vector n pty) = return (n,pty)
elimVector _              = mzero

elimArray :: MonadPlus m => Type -> m (Word64, Type)
elimArray (Array n ety) = return (n, ety)
elimArray _             = mzero

elimFunPtr :: MonadPlus m => Type -> m (Type,[Type],Bool)
elimFunPtr  = elimFunTy <=< elimPtrTo

elimPrimType :: MonadPlus m => Type -> m PrimType
elimPrimType (PrimType pt) = return pt
elimPrimType _             = mzero

elimFloatType :: MonadPlus m => PrimType -> m FloatType
elimFloatType (FloatType ft) = return ft
elimFloatType _              = mzero

-- | Eliminator for array, pointer and vector types.
elimSequentialType :: MonadPlus m => Type -> m Type
elimSequentialType ty = case ty of
  Array _ elTy -> return elTy
  PtrTo elTy _ -> return elTy
  Vector _ pty -> return pty
  _            -> mzero


-- Top-level Type Aliases ------------------------------------------------------

data TypeDecl = TypeDecl
  { typeName  :: Ident
  , typeValue :: Type
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- Globals ---------------------------------------------------------------------

data Global = Global
  { globalSym      :: Symbol
  , globalAttrs    :: GlobalAttrs
  , globalType     :: Type
  , globalValue    :: Maybe Value
  , globalAlign    :: Maybe Align
  , globalMetadata :: GlobalMdAttachments
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

addGlobal :: Global -> Module -> Module
addGlobal g m = m { modGlobals = g : modGlobals m }

data GlobalAttrs = GlobalAttrs
  { gaLinkage    :: Maybe Linkage
  , gaVisibility :: Maybe Visibility
  , gaAddrSpace  :: AddrSpace
  , gaConstant   :: Bool
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

emptyGlobalAttrs :: GlobalAttrs
emptyGlobalAttrs  = GlobalAttrs
  { gaLinkage    = Nothing
  , gaVisibility = Nothing
  , gaAddrSpace  = defaultAddrSpace
  , gaConstant   = False
  }


-- Declarations ----------------------------------------------------------------

data Declare = Declare
  { decLinkage    :: Maybe Linkage
  , decVisibility :: Maybe Visibility
  , decRetType    :: Type
  , decName       :: Symbol
  , decArgs       :: [Type]
  , decVarArgs    :: Bool
  , decAttrs      :: [FunAttr]
  , decComdat     :: Maybe String
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | The function type of this declaration
decFunType :: Declare -> Type
decFunType Declare { .. } = PtrTo (FunTy decRetType decArgs decVarArgs) defaultAddrSpace


-- Function Definitions --------------------------------------------------------

data Define = Define
  { defLinkage    :: Maybe Linkage
  , defVisibility :: Maybe Visibility
  , defRetType    :: Type
  , defName       :: Symbol
  , defArgs       :: [Typed Ident]
  , defVarArgs    :: Bool
  , defAttrs      :: [FunAttr]
  , defSection    :: Maybe String
  , defGC         :: Maybe GC
  , defBody       :: [BasicBlock]
  , defMetadata   :: FnMdAttachments
  , defComdat     :: Maybe String
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

defFunType :: Define -> Type
defFunType Define { .. } =
  PtrTo (FunTy defRetType (map typedType defArgs) defVarArgs) defaultAddrSpace

addDefine :: Define -> Module -> Module
addDefine d m = m { modDefines = d : modDefines m }

-- Function Attributes and attribute groups ------------------------------------


data FunAttr
   = AlignStack Int
   | Alwaysinline
   | Builtin
   | Cold
   | Convergent  -- ^ Introduced in LLVM 3.7
   | InaccessibleMemOnly  -- ^ Introduced in LLVM 3.8
   | Inlinehint
   | Jumptable
   | Minsize
   | Naked
   | Nobuiltin
   | Noduplicate
   | Noimplicitfloat
   | Noinline
   | Nonlazybind
   | Noredzone
   | Noreturn
   | Nounwind
   | Optnone
   | Optsize
   | Readnone
   | Readonly
   | ReturnsTwice
   | SanitizeAddress
   | SanitizeMemory
   | SanitizeThread
   | SSP
   | SSPreq
   | SSPstrong
   | UWTable
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- Basic Block Labels ----------------------------------------------------------

data BlockLabel
  = Named Ident
  | Anon Int
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance IsString BlockLabel where
  fromString str = Named (fromString str)

-- Basic Blocks ----------------------------------------------------------------

data BasicBlock' lab = BasicBlock
  { bbLabel :: Maybe lab
  , bbStmts :: [Stmt' lab]
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type BasicBlock = BasicBlock' BlockLabel

brTargets :: BasicBlock' lab -> [lab]
brTargets (BasicBlock _ stmts) =
  case stmtInstr (last stmts) of
    Br _ t1 t2         -> [t1, t2]
    Invoke _ _ _ to uw -> [to, uw]
    Jump t             -> [t]
    Switch _ l ls      -> l : map snd ls
    IndirectBr _ ls    -> ls
    _                  -> []

-- Attributes ------------------------------------------------------------------

-- | Symbol Linkage
data Linkage
  = Private
  | LinkerPrivate
  | LinkerPrivateWeak
  | LinkerPrivateWeakDefAuto
  | Internal
  | AvailableExternally
  | Linkonce
  | Weak
  | Common
  | Appending
  | ExternWeak
  | LinkonceODR
  | WeakODR
  | External
  | DLLImport
  | DLLExport
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

data Visibility = DefaultVisibility
                | HiddenVisibility
                | ProtectedVisibility
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

newtype GC = GC
  { getGC :: String
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- Typed Things ----------------------------------------------------------------

data Typed a = Typed
  { typedType  :: Type
  , typedValue :: a
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

instance Foldable Typed where
  foldMap f t = f (typedValue t)

instance Traversable Typed where
  sequenceA t = mk `fmap` typedValue t
    where
    mk b = t { typedValue = b }

mapMTyped :: Monad m => (a -> m b) -> Typed a -> m (Typed b)
mapMTyped f t = do
  b <- f (typedValue t)
  return t { typedValue = b }

-- Instructions ----------------------------------------------------------------

data ArithOp
  = Add Bool Bool
    {- ^ * Integral addition.
         * First boolean flag: check for unsigned overflow.
         * Second boolean flag: check for signed overflow.
         * If the checks fail, then the result is poisoned. -}
  | FAdd [FMF]
    -- ^ Floating point addition.

  | Sub Bool Bool
    {- ^ * Integral subtraction.
         * First boolean flag: check for unsigned overflow.
         * Second boolean flag: check for signed overflow.
         * If the checks fail, then the result is poisoned. -}

  | FSub [FMF]
    -- ^ Floating point subtraction.

  | Mul Bool Bool
    {- ^ * Integral multiplication.
         * First boolean flag: check for unsigned overflow.
         * Second boolean flag: check for signed overflow.
         * If the checks fail, then the result is poisoned. -}

  | FMul [FMF]
    -- ^ Floating point multiplication.

  | UDiv Bool
    {- ^ * Integral unsigned division.
         * Boolean flag: check for exact result.
         * If the check fails, then the result is poisoned. -}

  | SDiv Bool
    {- ^ * Integral signed division.
         * Boolean flag: check for exact result.
         * If the check fails, then the result is poisoned. -}

  | FDiv [FMF]
    -- ^ Floating point division.

  | URem
    -- ^ Integral unsigned reminder resulting from unsigned division.
    -- Division by 0 is undefined.

  | SRem
    -- ^ * Integral signded reminder resulting from signed division.
    --   * The sign of the reminder matches the divident (first parameter).
    --   * Division by 0 is undefined.

  | FRem [FMF]
    -- ^ * Floating point reminder resulting from floating point division.
    --   * The reminder has the same sign as the divident (first parameter).

    deriving (Data, Eq, Generic, Ord, Show, Typeable)

isIArith :: ArithOp -> Bool
isIArith Add{}  = True
isIArith Sub{}  = True
isIArith Mul{}  = True
isIArith UDiv{} = True
isIArith SDiv{} = True
isIArith URem   = True
isIArith SRem   = True
isIArith _      = False

isFArith :: ArithOp -> Bool
isFArith  = not . isIArith

data UnaryArithOp
  = FNeg [FMF]
    -- ^ Floating point negation.
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | Binary bitwise operators.
data BitOp
  = Shl Bool Bool
    {- ^ * Shift left.
         * First bool flag: check for unsigned overflow (i.e., shifted out a 1).
         * Second bool flag: check for signed overflow
              (i.e., shifted out something that does not match the sign bit)

         If a check fails, then the result is poisoned.

         The value of the second parameter must be strictly less than the
           number of bits in the first parameter,
           otherwise the result is undefined.  -}

  | Lshr Bool
    {- ^ * Logical shift right.
         * The boolean is for exact check: poison the result,
              if we shift out a 1 bit (i.e., had to round).

    The value of the second parameter must be strictly less than the
    number of bits in the first parameter, otherwise the result is undefined.
    -}

  | Ashr Bool
    {- ^ * Arithmetic shift right.
         * The boolean is for exact check: poison the result,
                if we shift out a 1 bit (i.e., had to round).

    The value of the second parameter must be strictly less than the
    number of bits in the first parameter, otherwise the result is undefined.
    -}

  | And
  | Or
  | Xor
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | Conversions from one type to another.
data ConvOp
  = Trunc Bool Bool
    -- ^ Truncate an integer value to a smaller integer type.
    --
    -- The 'Bool' fields (added in in LLVM 20) encode whether to perform
    -- overflow-related checks:
    --
    -- * First 'Bool': check for unsigned overflow.
    -- * Second 'Bool': check for signed overflow.
    --
    -- If the checks fail, then the result is poisoned.
    --
    -- These fields can only ever 'True' in 'Conv' instructions in LLVM 20 or
    -- later. These fields are always 'False' in 'ConstConv' constant
    -- expressions or if the LLVM version is older than 20.
  | ZExt Bool
    -- ^ Zero extension.
    --
    -- The 'Bool' field (added in LLVM 18) encodes whether to enforce that the
    -- argument is non-negative. If the 'Bool' is 'True' and the argument is
    -- negative, then the result is poisoned.
    --
    -- This field can only ever 'True' in 'Conv' instructions in LLVM 18 or
    -- later. This field is always 'False' in 'ConstConv' constant expressions
    -- or if the LLVM version is older than 18.
  | SExt
  | FpTrunc
  | FpExt
  | FpToUi
  | FpToSi
  | UiToFp Bool
    -- ^ Convert the argument from an unsigned integer to a floating-point
    -- value.
    --
    -- The 'Bool' field (added in LLVM 19) encodes whether to enforce that the
    -- argument is non-negative. If the 'Bool' is 'True' and the argument is
    -- negative, then the result is poisoned.
    --
    -- This field can only ever 'True' in 'Conv' instructions in LLVM 19 or
    -- later. This field is always 'False' in 'ConstConv' constant expressions
    -- or if the LLVM version is older than 19.
  | SiToFp
  | PtrToInt
  | IntToPtr
  | BitCast
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

data AtomicRWOp
  = AtomicXchg
  | AtomicAdd
  | AtomicSub
  | AtomicAnd
  | AtomicNand
  | AtomicOr
  | AtomicXor
  | AtomicMax
  | AtomicMin
  | AtomicUMax
  | AtomicUMin
  | AtomicFAdd  -- ^ Introduced in LLVM 9
  | AtomicFSub  -- ^ Introduced in LLVM 9
  | AtomicFMax  -- ^ Introduced in LLVM 15
  | AtomicFMin  -- ^ Introduced in LLVM 15
  | AtomicUIncWrap  -- ^ Introduced in LLVM 16
  | AtomicUDecWrap  -- ^ Introduced in LLVM 16
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

data AtomicOrdering
  = Unordered
  | Monotonic
  | Acquire
  | Release
  | AcqRel
  | SeqCst
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

type Align = Int

data Instr' lab
  = Ret (Typed (Value' lab))
    {- ^ * Return from function with the given value.
         * Ends basic block. -}

  | RetVoid
    {- ^ * Return from function.
         * Ends basic block. -}

  | Arith ArithOp (Typed (Value' lab)) (Value' lab)
    {- ^ * Binary arithmetic operation, both operands have the same type.
         * Middle of basic block.
         * The result is the same as parameters. -}

  | UnaryArith UnaryArithOp (Typed (Value' lab))
    {- ^ * Unary arithmetic operation.
         * Middle of basic block.
         * The result is the same as the parameter. -}

  | Bit BitOp (Typed (Value' lab)) (Value' lab)
    {- ^ * Binary bit-vector operation, both operands have the same type.
         * Middle of basic block.
         * The result is the same as parameters. -}

  | Conv ConvOp (Typed (Value' lab)) Type
    {- ^ * Convert a value from one type to another.
         * Middle of basic block.
         * The result matches the 3rd parameter.
         Note: fptrunc and fpext allow fast-math flags in LLVM, but this is not represented yet. -}

  | Call Bool [FMF] Type (Value' lab) [Typed (Value' lab)]
    {- ^ * Call a function.
            The boolean is tail-call hint (XXX: needs to be updated)
         * Middle of basic block.
         * The result is as indicated by the provided type.
         * Fast-math flags are only allowed if the type is a floating-point type; see LLVM documentation. -}

  | CallBr Type (Value' lab) [Typed (Value' lab)] lab [lab]
    {- ^ * Call a function in asm-goto style:
             return type;
             function operand;
             arguments;
             default basic block destination;
             other basic block destinations.
         * Middle of basic block.
         * The result is as indicated by the provided type.
         * Introduced in LLVM 9. -}

  | Alloca Type (Maybe (Typed (Value' lab))) (Maybe Int)
    {- ^ * Allocated space on the stack:
           type of elements;
           how many elements (1 if 'Nothing');
           required alignment.
         * Middle of basic block.
         * Returns a pointer to hold the given number of elements. -}

  | Load Bool Type (Typed (Value' lab)) (Maybe AtomicOrdering) (Maybe Align)
    {- ^ * Read a value from the given address:
           whether the load is volatile;
           type being loaded;
           address to read from;
           atomic ordering;
           assumptions about alignment of the given pointer.
         * Middle of basic block.
         * Returns a value of type matching the pointer. -}

  | Store Bool (Typed (Value' lab)) (Typed (Value' lab)) (Maybe AtomicOrdering) (Maybe Align)
    {- ^ * Write a value to memory:
             whether the store is volatile;
             value to store;
             pointer to location where to store;
             atomic ordering;
             assumptions about the alignment of the given pointer.
         * Middle of basic block.
         * Effect. -}


  | Fence (Maybe String) AtomicOrdering
    {- ^ * Introduce a happens-before relationship between operations:
             synchronization scope;
             type of ordering.
         * Middle of basic block. -}

  | CmpXchg Bool Bool (Typed (Value' lab)) (Typed (Value' lab)) (Typed (Value' lab)) (Maybe String) AtomicOrdering AtomicOrdering
    {- ^ * Atomically compare and maybe exchange values in memory:
             whether the exchange is weak;
             whether the exchange is volatile;
             pointer to read;
             value to compare it with;
             new value to write if the two prior values are equal;
             synchronization scope;
             synchronization ordering on success;
             synchronization ordering on failure.
         * Returns a pair of the original value and whether an exchange occurred.
         * Middle of basic block.
         * Effect. -}

  | AtomicRW Bool AtomicRWOp (Typed (Value' lab)) (Typed (Value' lab)) (Maybe String) AtomicOrdering
    {- ^ * Perform an atomic load, operation, and store:
             whether the operation is volatile;
             operation to apply to the read value and the provided value;
             pointer to read;
             value to combine it with, using the given operation;
             synchronization scope;
             synchronization ordering.
         * Returns the original value at the given location.
         * Middle of basic block.
         * Effect. -}

  | ICmp Bool ICmpOp (Typed (Value' lab)) (Value' lab)
    {- ^ * Compare two integral values.
         * Middle of basic block.
         * Returns a boolean value.
         * The 'Bool' field (added in LLVM 20) encodes whether to enforce that
           the arguments have the same sign. If the 'Bool' is 'True' and the
           arguments have mismatched signs, then the result is poisoned. This
           field is always 'False' if the LLVM version is older than 20. -}

  | FCmp [FMF] FCmpOp (Typed (Value' lab)) (Value' lab)
    {- ^ * Compare two floating point values.
         * Middle of basic block.
         * Returns a boolean value. -}

  | Phi [FMF] Type [(Value' lab,lab)]
    {- ^ * Join point for an SSA value: we get one value per predecessor
           basic block.
         * Middle of basic block.
         * Returns a value of the specified type.
         * Fast-math flags are only allowed if the type is a floating-point type; see LLVM documentation. -}

  | GEP [GEPAttr] Type (Typed (Value' lab)) [Typed (Value' lab)]
    {- ^ * "Get element pointer",
            compute the address of a field in a structure:
            inbounds check attr (value poisoned if this fails);
            type to use as a basis for calculations;
            pointer to parent structure;
            path to a sub-component of a structure.
         * Middle of basic block.
         * Returns the address of the requested member.

    It's recommended that the GEPAttr list should be normalized (i.e. only one of
    each entry).

    The types in path are the types of the index, not the fields.

    The indexes are in units of fields (i.e., the first element in
    a struct is field 0, the next one is 1, etc., regardless of the size
    of the fields in bytes). -}

  | Select [FMF] (Typed (Value' lab)) (Typed (Value' lab)) (Value' lab)
    {- ^ * Local if-then-else; the first argument is boolean, if
           true pick the 2nd argument, otherwise evaluate to the 3rd.
         * Middle of basic block.
         * Returns either the 2nd or the 3rd argument.
         * Fast-math flags are only allowed if the type is a floating-point type; see LLVM documentation.-}

  | ExtractValue (Typed (Value' lab)) [Int32]
    {- ^ * Get the value of a member of an aggregate value:
           the first argument is an aggregate value (not a pointer!),
           the second is a path of indexes, similar to the one in 'GEP'.
         * Middle of basic block.
         * Returns the given member of the aggregate value. -}

  | InsertValue (Typed (Value' lab)) (Typed (Value' lab)) [Int32]
    {- ^ * Set the value for a member of an aggregate value:
           the first argument is the value to insert, the second is the
           aggreagate value to be modified.
         * Middle of basic block.
         * Returns an updated aggregate value. -}

  | ExtractElt (Typed (Value' lab)) (Value' lab)
    {- ^ * Get an element from a vector: the first argument is a vector,
           the second an index.
         * Middle of basic block.
         * Returns the element at the given position. -}

  | InsertElt (Typed (Value' lab)) (Typed (Value' lab)) (Value' lab)
    {- ^ * Modify an element of a vector: the first argument is the vector,
           the second the value to be inserted, the third is the index where
           to insert the value.
         * Middle of basic block.
         * Returns an updated vector. -}


  | ShuffleVector (Typed (Value' lab)) (Value' lab) (Typed (Value' lab))
    {- ^ * Constructs a fixed permutation of two input vectors: the first
           and second arguments are input vectors, and the third argument
           is the mask.
         * Middle of basic block.
         * Returns the permuted vector. For each element, the mask selects
           an element from one of the input vectors to copy to the result:
            * non-negative mask values represent an index into the concatenated
              pair of input vectors, and
            * -1 mask value indicates that the output element is poison.
    -}

  | Jump lab
    {- ^ * Jump to the given basic block.
         * Ends basic block. -}

  | Br (Typed (Value' lab)) lab lab
    {- ^ * Conditional jump: if the value is true jump to the first basic
           block, otherwise jump to the second.
         * Ends basic block. -}

  | Invoke Type (Value' lab) [Typed (Value' lab)] lab lab
    {- ^ * Calls the specified target function, then branches to the success
           label.  If an exception occurs during the call, the exception unwind
           handling branches to the second label.
         * Arguments:
           1. The function's return type
           2. The function target itself (to be called)
           3. arguments to the function
           4. successful return target label
           5. on-exception unwind target label
         * Ends basic block. -}

  | Comment String
    -- ^ Comment

  | Unreachable
    -- ^ No defined sematics, we should not get to here.

  | Unwind
  | VaArg (Typed (Value' lab)) Type
    -- ^ Accesses arguments passed through \"varargs\" areas of a function call.
    -- The argument is a @va_list*@; this instruction returns the value of the
    -- specified type located at the target and increments the pointer.

  | IndirectBr (Typed (Value' lab)) [lab]
    -- ^ Branch via pointer indirection.  The argument is the address of the
    -- label to jump to.  (All) Possible destination targets are provided.

  | Switch (Typed (Value' lab)) lab [(Integer,lab)]
    {- ^ * Multi-way branch: the first value determines the target index
           for the jump, which is looked up in the third argument table
           (key values are unique).  The second argument is the default
           destination if the target is not found in the table.
         * Ends basic block. -}

  | LandingPad Type (Maybe (Typed (Value' lab))) Bool [Clause' lab]
    {- ^ Target of an exception (from the 'Invoke' instruction).
         * Arguments:
           1. The result type (the values set by the personality function
              on re-entry to the function).
           2. The second argument may be the personality function, which defines
              values on re-entry. This is used in older LLVM versions and is
              not supplied for recent LLVM versions.
           3. True if this block is a "cleanup".
           4. The list of clauses to handle the exception;  the clauses are
              used to match the exception thrown.
         * If no clause matches and cleanup not set, continue unwinding up
           the stack (see 'Resume').
         * If cleanup is false, there must be at least one clause
     -}

  | Resume (Typed (Value' lab))
    {- ^ Resumes propagation of an in-flight exception whose unwinding was
         interrupted by a 'LandingPad' instruction.
         * Argument: the value of the exception to propagate.
    -}

  | Freeze (Typed (Value' lab))
    {- ^ * Used to stop propagation of @undef@ and @poison@ values.
         * If the argument is @undef@ or @poison@, returns an arbitrary
           (but fixed) value of that type instead, otherwise a no-op and
           returns its argument.
         * Middle of basic block. -}

    deriving (Data, Eq, Functor, Generic, Ord, Show, Typeable)

type Instr = Instr' BlockLabel

data Clause' lab
  = Catch  (Typed (Value' lab))
  | Filter (Typed (Value' lab))
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type Clause = Clause' BlockLabel


isTerminator :: Instr' lab -> Bool
isTerminator instr = case instr of
  Ret{}        -> True
  RetVoid      -> True
  Jump{}       -> True
  CallBr{}     -> True
  Br{}         -> True
  Unreachable  -> True
  Unwind       -> True
  Invoke{}     -> True
  IndirectBr{} -> True
  Switch{}     -> True
  Resume{}     -> True
  _            -> False

isComment :: Instr' lab -> Bool
isComment Comment{} = True
isComment _         = False

isPhi :: Instr' lab -> Bool
isPhi Phi{} = True
isPhi _     = False

-- | Integer comparison operators.
data ICmpOp = Ieq | Ine | Iugt | Iuge | Iult | Iule | Isgt | Isge | Islt | Isle
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

-- | Floating-point comparison operators.
data FCmpOp = Ffalse  | Foeq | Fogt | Foge | Folt | Fole | Fone
            | Ford    | Fueq | Fugt | Fuge | Fult | Fule | Fune
            | Funo    | Ftrue
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

-- | Fast-math flags. <https://llvm.org/docs/LangRef.html#fast-math-flags>
data FMF
  = Ffast  -- ^ Short-hand for all other flags combined
  | Fnnan  -- ^ No NaNs
  | Fninf  -- ^ No infs
  | Fnsz  -- ^ No signed zeros
  | Farcp  -- ^ Allow reciprocal (a / b == a * (1.0 / b))
  | Fcontract  -- ^ Allow contraction (e.g. fused multiply-add)
  | Fafn  -- ^ Approximate functions (for sqrt, sin, log, etc.)
  | Freassoc  -- ^ Reassociation (important for e.g. vectorisation of horizontal sums)
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)


-- Debug Instructions ----------------------------------------------------------

-- | Debug Instructions
--
-- In LLVM 19, debug instructions were added as a replacement for the intrinsic
-- functions previously used.  This addition is described in
-- llvm-project/llvm/docs/RemoveDIsDebugInfo.md in the LLVM repository.
data DebugRecord' lab
  = DebugRecordValue (DbgRecValue' lab)
  | DebugRecordDeclare (DbgRecDeclare' lab)
  | DebugRecordAssign (DbgRecAssign' lab)
  | DebugRecordValueSimple (DbgRecValueSimple' lab)
  | DebugRecordLabel (DbgRecLabel' lab)
  deriving (Data, Eq, Functor, Generic, Ord, Show, Typeable)

type DebugRecord = DebugRecord' BlockLabel

data DbgRecValue' lab = DbgRecValue
  {
    drvLocation :: ValMd' lab -- ^ Expected to be a DILocation
  , drvLocalVariable :: ValMd' lab -- ^ Expected to be a DILocalVariable
  , drvExpression :: ValMd' lab -- ^ Expected to be a DIExpression
  , drvValAsMetadata :: ValMd' lab
  }
  deriving (Data, Eq, Functor, Generic, Ord, Show, Typeable)

type DbgRecValue = DbgRecValue' BlockLabel

data DbgRecValueSimple' lab = DbgRecValueSimple
  {
    drvsLocation :: ValMd' lab -- ^ Expected to be a DILocation
  , drvsLocalVariable :: ValMd' lab -- ^ Expected to be a DILocalVariable
  , drvsExpression :: ValMd' lab -- ^ Expected to be a DIExpression
  , drvsValue :: Typed (Value' lab)
  }
  deriving (Data, Eq, Functor, Generic, Ord, Show, Typeable)

type DbgRecValueSimple = DbgRecValueSimple' BlockLabel

data DbgRecDeclare' lab = DbgRecDeclare
  {
    drdLocation :: ValMd' lab -- ^ Expected to be a DILocation
  , drdLocalVariable :: ValMd' lab -- ^ Expected to be a DILocalVariable
  , drdExpression :: ValMd' lab -- ^ Expected to be a DIExpression
  , drdValAsMetadata :: ValMd' lab
  }
  deriving (Data, Eq, Functor, Generic, Ord, Show, Typeable)

type DbgRecDeclare = DbgRecDeclare' BlockLabel

data DbgRecAssign' lab = DbgRecAssign
  {
    draLocation :: ValMd' lab -- ^ Expected to be a DILocation
  , draLocalVariable :: ValMd' lab -- ^ Expected to be a DILocalVariable
  , draExpression :: ValMd' lab -- ^ Expected to be a DIExpression
  , draValAsMetadata :: ValMd' lab
  , draAssignID :: ValMd' lab -- ^ Expected to be a DIAssignID
  , draExpressionAddr :: ValMd' lab -- ^ Expected to be a DIExpression
  , draValAsMetadataAddr :: ValMd' lab
  }
  deriving (Data, Eq, Functor, Generic, Ord, Show, Typeable)

type DbgRecAssign = DbgRecAssign' BlockLabel

data DbgRecLabel' lab = DbgRecLabel
  {
    drlLocation :: ValMd' lab -- ^ Expected to be a DILocation
  , drlLabel :: ValMd' lab -- ^ Expected to be a DILabel
  }
  deriving (Data, Eq, Functor, Generic, Ord, Show, Typeable)

type DbgRecLabel = DbgRecLabel' BlockLabel


-- Values ----------------------------------------------------------------------

data Value' lab
  = ValInteger Integer
  | ValBool Bool
  | ValFloat Float
  | ValDouble Double
  | ValFP80 FP80Value
  | ValIdent Ident
  | ValSymbol Symbol
  | ValNull
  | ValArray Type [Value' lab]
  | ValVector Type [Value' lab]
  | ValStruct [Typed (Value' lab)]
  | ValPackedStruct [Typed (Value' lab)]
  | ValString [Word8]
  | ValConstExpr (ConstExpr' lab)
  | ValUndef
  | ValLabel lab
  | ValZeroInit
  | ValAsm Bool Bool String String
  | ValMd (ValMd' lab)
  | ValPoison
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type Value = Value' BlockLabel

data FP80Value = FP80_LongDouble Word16 Word64
               deriving (Data, Eq, Ord, Generic, Show, Typeable)

data ValMd' lab
  = ValMdString String
  | ValMdValue (Typed (Value' lab))
  | ValMdRef Int
  | ValMdNode [Maybe (ValMd' lab)]
  | ValMdLoc (DebugLoc' lab)
  | ValMdDebugInfo (DebugInfo' lab)
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type ValMd = ValMd' BlockLabel

type KindMd = String
type FnMdAttachments = Map.Map KindMd ValMd
type GlobalMdAttachments = Map.Map KindMd ValMd

data DebugLoc' lab = DebugLoc
  { dlLine  :: Word32
  , dlCol   :: Word32
  , dlScope :: ValMd' lab
  , dlIA    :: Maybe (ValMd' lab)
  , dlImplicit :: Bool
  , dlAtomGroup :: Word64 -- ^ Introduced in LLVM 21
  , dlAtomRank :: Word64 -- ^ Introduced in LLVM 21
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DebugLoc = DebugLoc' BlockLabel

isConst :: Value' lab -> Bool
isConst ValInteger{}   = True
isConst ValBool{}      = True
isConst ValFloat{}     = True
isConst ValDouble{}    = True
isConst ValFP80{}      = True
isConst ValConstExpr{} = True
isConst ValZeroInit    = True
isConst ValNull        = True
isConst _              = False

-- Value Elimination -----------------------------------------------------------

elimValSymbol :: MonadPlus m => Value' lab -> m Symbol
elimValSymbol (ValSymbol sym) = return sym
elimValSymbol _               = mzero

elimValInteger :: MonadPlus m => Value' lab -> m Integer
elimValInteger (ValInteger i) = return i
elimValInteger _              = mzero

-- Statements ------------------------------------------------------------------

-- | Each statement, which can return a value (`Result`) referenced by the
-- `Ident` or else it has no return value (`Effect`).  The statement has a single
-- Instruction, followed by any Debug Records or associated metadata.
--
-- See llvm-project/llvm/docs/RemoveDIsDebugInfo.md for discussion on the
-- [DebugRecord] fields.  Note that DebugRecords and debug intrinsics may not be
-- mixed in a module; the former is new and preferred over the latter.
--
-- Technically, DebugRecords are attached to Instructions, but since there's a
-- 1:1 correspondence between Stmt and Instr, it is cleaner to attach the
-- DebugRecords to the Stmt to keep the Instrs from getting additional
-- complications.
--
-- Each statement may have both Debug Records (2nd-to-last field) and a list of
-- metadata attributes (last field).  As noted above, bitcode file should not mix
-- Debug Records and intrinsics; if Debug Records are used, the metadata
-- attribute list should not contain intrinsics (although it may contain other
-- metadata associated with this statement).

data Stmt' lab
  = Result Ident (Instr' lab) [DebugRecord' lab] [(String, ValMd' lab)]
  | Effect (Instr' lab) [DebugRecord' lab] [(String, ValMd' lab)]
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type Stmt = Stmt' BlockLabel

stmtMetadata :: Stmt' lab -> [(String, ValMd' lab)]
stmtMetadata = \case
  Result _ _ _ mds -> mds
  Effect _ _ mds   -> mds

stmtInstr :: Stmt' lab -> Instr' lab
stmtInstr (Result _ i _ _) = i
stmtInstr (Effect i _ _)   = i

extendMetadata :: Show lab => (String, ValMd' lab) -> Stmt' lab -> Stmt' lab
extendMetadata md stmt = case stmt of
  Result r i [] mds -> Result r i [] (md:mds)
  Result _ _ _ _ -> error $ "Adding MD " <> show md <> " after DebugRecord"
  Effect i drs mds   -> Effect i drs (md:mds)

addDebugRecord :: DebugRecord' lab -> Stmt' lab -> Stmt' lab
addDebugRecord dr = \case
  Result r i drs mds -> Result r i (snoc dr drs) mds
  Effect i drs mds   -> Effect i (snoc dr drs) mds
  where
    snoc e ls = ls <> [e]

-- Constant Expressions --------------------------------------------------------

data ConstExpr' lab
  = ConstGEP [GEPAttr] (Maybe RangeSpec) Type (Typed (Value' lab)) [Typed (Value' lab)]
  -- ^ Since LLVM 3.7, constant @getelementptr@ expressions include an explicit
  -- type to use as a basis for calculations. For older versions of LLVM, this
  -- type can be reconstructed by inspecting the pointee type of the parent
  -- pointer value.
  --
  -- Since LLVM 19, the bool "inbounds" is now [GEPAttr] and range is via
  -- RangeSpec instead of just Word64.  It's recommended that the GEPAttr list
  -- should be normalized (i.e. only one of each entry).
  | ConstConv ConvOp (Typed (Value' lab)) Type
  | ConstSelect (Typed (Value' lab)) (Typed (Value' lab)) (Typed (Value' lab))
  | ConstBlockAddr (Typed (Value' lab)) lab
  | ConstFCmp FCmpOp (Typed (Value' lab)) (Typed (Value' lab))
  | ConstICmp ICmpOp (Typed (Value' lab)) (Typed (Value' lab))
  | ConstArith ArithOp (Typed (Value' lab)) (Value' lab)
  | ConstUnaryArith UnaryArithOp (Typed (Value' lab))
  | ConstBit BitOp (Typed (Value' lab)) (Value' lab)
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type ConstExpr = ConstExpr' BlockLabel

-- | Attributes imposing rules on the GEP; violating any rule results in a poison
-- value.  If the base is a vector of pointers, the attributes apply to each
-- computation element-wise.  See
-- https://llvm.org/docs/LangRef.html#getelementptr-instruction for more
-- information.
data GEPAttr
  = GEP_Inbounds
    -- ^ Rules:
    -- * Base pointer has an inbounds (but not necessarily live) address of the
    --   allocated object it is based on (i.e. points into that allocation or to
    --   its end.  Size for a growable allocated object is the max size, not the
    --   current size.
    -- * Pointer must remain inbounds at all times when adding the offsets
    -- * Implies 'GEP_NUSW'
  | GEP_NUSW
    -- ^ No unsigned signed wrap.
    -- Rules:
    -- * If type of index is larger than ptr index type, truncation preserves
    --   the signed value.
    -- * Multiplication of an index by the type size does not wrap in a
    --   signed sense.
    -- * Offset additions (excluding base address) does not wrap in a
    --   signed sense
    -- * Addition of the current address (as unsigned, truncated to ptr
    --   index type) and each offset (as signed) does not wrap the ptr
    --   index type.
  | GEP_NUW
    -- ^ No unsigned wrap
    -- Rules:
    -- * If type of index is larger than ptr index type, truncation preserves
    --   the unsigned value.
    -- * Multiplication of an index by the type size does not wrap in an
    --   unsigned sense.
    -- * Offset additions (excluding base address) does not wrap in an
    --   unsigned sense
    -- * Addition of the current address (as unsigned, truncated to ptr
    --   index type) and each offset (as unsigned) does not wrap the ptr
    --   index type.
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

orderedGEPAttrs :: [GEPAttr]
orderedGEPAttrs = [GEP_Inbounds, GEP_NUSW, GEP_NUW] -- bit0, bit1, ...

data RangeSpec
  = RangeIndex Word64
    -- ^ index of valid range as used in pre-LLVM19 for when "inbounds" as a
    -- boolean was True.  Deprecated.
  | Range Int Integer Integer
    -- ^ width of arbitrary-precision integer (in bits) and lower and upper
    -- arbitrary-precision integer bounds of that size as [lower, upper).
  deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- DWARF Debug Info ------------------------------------------------------------

data DebugInfo' lab
  = DebugInfoBasicType (DIBasicType' lab)
  | DebugInfoCompileUnit (DICompileUnit' lab)
  | DebugInfoCompositeType (DICompositeType' lab)
  | DebugInfoDerivedType (DIDerivedType' lab)
  | DebugInfoEnumerator String !Integer Bool
    -- ^ The 'Bool' field represents @isUnsigned@, introduced in LLVM 7.
  | DebugInfoExpression DIExpression
  | DebugInfoFile DIFile
  | DebugInfoGlobalVariable (DIGlobalVariable' lab)
  | DebugInfoGlobalVariableExpression (DIGlobalVariableExpression' lab)
  | DebugInfoLexicalBlock (DILexicalBlock' lab)
  | DebugInfoLexicalBlockFile (DILexicalBlockFile' lab)
  | DebugInfoLocalVariable (DILocalVariable' lab)
  | DebugInfoSubprogram (DISubprogram' lab)
  | DebugInfoSubrange (DISubrange' lab)
  | DebugInfoSubroutineType (DISubroutineType' lab)
  | DebugInfoNameSpace (DINameSpace' lab)
  | DebugInfoTemplateTypeParameter (DITemplateTypeParameter' lab)
  | DebugInfoTemplateValueParameter (DITemplateValueParameter' lab)
  | DebugInfoImportedEntity (DIImportedEntity' lab)
  | DebugInfoLabel (DILabel' lab)
  | DebugInfoArgList (DIArgList' lab)
  | DebugInfoAssignID -- ^ Introduced in LLVM 17.
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DebugInfo = DebugInfo' BlockLabel

type DILabel = DILabel' BlockLabel
data DILabel' lab = DILabel
    { dilScope :: Maybe (ValMd' lab)
    , dilName  :: String
    , dilFile  :: Maybe (ValMd' lab)
    , dilLine  :: Word32
    , dilColumn :: Word32 -- ^ Introduced in LLVM 21.
    , dilIsArtificial :: Bool -- ^ Introduced in LLVM 21.
    , dilCoroSuspendIdx :: Maybe Word32 -- ^ Introduced in LLVM 21.
    } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DIImportedEntity = DIImportedEntity' BlockLabel
data DIImportedEntity' lab = DIImportedEntity
    { diieTag    :: DwarfTag
    , diieScope  :: Maybe (ValMd' lab)
    , diieEntity :: Maybe (ValMd' lab)
    , diieFile   :: Maybe (ValMd' lab)
    , diieLine   :: Word32
    , diieName   :: Maybe String
    } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DITemplateTypeParameter = DITemplateTypeParameter' BlockLabel
data DITemplateTypeParameter' lab = DITemplateTypeParameter
    { dittpName      :: Maybe String
    , dittpType      :: Maybe (ValMd' lab)
    , dittpIsDefault :: Maybe Bool         -- since LLVM 11
    } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DITemplateValueParameter = DITemplateValueParameter' BlockLabel
data DITemplateValueParameter' lab = DITemplateValueParameter
    { ditvpTag       :: DwarfTag
    , ditvpName      :: Maybe String
    , ditvpType      :: Maybe (ValMd' lab)
    , ditvpIsDefault :: Maybe Bool         -- since LLVM 11
    , ditvpValue     :: ValMd' lab
    } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DINameSpace = DINameSpace' BlockLabel
data DINameSpace' lab = DINameSpace
    { dinsName  :: Maybe String
    , dinsScope :: ValMd' lab
    , dinsFile  :: ValMd' lab
    , dinsLine  :: Word32
    } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

-- TODO: Turn these into sum types
-- See https://github.com/llvm-mirror/llvm/blob/release_38/include/llvm/Support/Dwarf.def
type DwarfAttrEncoding = Word16
type DwarfLang = Word16
type DwarfTag = Word16
type DwarfVirtuality = Word8
-- See https://github.com/llvm-mirror/llvm/blob/release_38/include/llvm/IR/DebugInfoMetadata.h#L175
type DIFlags = Word32
-- This seems to be defined internally as a small enum, and defined
-- differently across versions. Maybe turn this into a sum type once
-- it stabilizes.
type DIEmissionKind = Word8

-- See https://github.com/llvm/llvm-project/commit/eb8901bda11fd55deeecd067fc4c9dcc0fb89984
dwarf_DW_APPLE_ENUM_KIND_invalid :: Word32
dwarf_DW_APPLE_ENUM_KIND_invalid = complement (0 :: Word32) -- ~ LLVM 19

data DIBasicType' lab = DIBasicType
  { dibtTag      :: DwarfTag
  , dibtName     :: String
  , dibtSize     :: Maybe (ValMd' lab)
    -- ^ If using LLVM 20 or older, this will always be @Just@ an 'ValMdValue',
    -- where the underlying value is a 64-bit 'ValInteger'. If using LLVM 21 or
    -- later, this can also be a null reference (i.e., 'Nothing'), a variable
    -- (i.e., @Just@ a 'DIGlobalVariable' or 'DILocalVariable'), or an
    -- expression (i.e., @Just@ a 'DIExpression').
  , dibtAlign    :: Word64
  , dibtEncoding :: DwarfAttrEncoding
  , dibtFlags    :: Maybe DIFlags
  , dibtNumExtraInhabitants :: Word64 -- ^ added in LLVM 20.
  } deriving (Data, Eq, Functor, Generic, Ord, Show, Typeable)

type DIBasicType = DIBasicType' BlockLabel

data DICompileUnit' lab = DICompileUnit
  { dicuLanguage           :: DwarfLang
  , dicuFile               :: Maybe (ValMd' lab)
  , dicuProducer           :: Maybe String
  , dicuIsOptimized        :: Bool
  , dicuFlags              :: Maybe String
  , dicuRuntimeVersion     :: Word16
  , dicuSplitDebugFilename :: Maybe FilePath
  , dicuEmissionKind       :: DIEmissionKind
  , dicuEnums              :: Maybe (ValMd' lab)
  , dicuRetainedTypes      :: Maybe (ValMd' lab)
  , dicuSubprograms        :: Maybe (ValMd' lab)
  , dicuGlobals            :: Maybe (ValMd' lab)
  , dicuImports            :: Maybe (ValMd' lab)
  , dicuMacros             :: Maybe (ValMd' lab)
  , dicuDWOId              :: Word64
  , dicuSplitDebugInlining :: Bool
  , dicuDebugInfoForProf   :: Bool
  , dicuNameTableKind      :: Word64
    -- added in LLVM 11: dicuRangesBaseAddress, dicuSysRoot, and dicuSDK
  , dicuRangesBaseAddress  :: Bool
  , dicuSysRoot            :: Maybe String
  , dicuSDK                :: Maybe String
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DICompileUnit = DICompileUnit' BlockLabel

data DICompositeType' lab = DICompositeType
  { dictTag            :: DwarfTag
  , dictName           :: Maybe String
  , dictFile           :: Maybe (ValMd' lab)
  , dictLine           :: Word32
  , dictScope          :: Maybe (ValMd' lab)
  , dictBaseType       :: Maybe (ValMd' lab)
  , dictSize           :: Maybe (ValMd' lab)
    -- ^ If using LLVM 20 or older, this will always be @Just@ an 'ValMdValue',
    -- where the underlying value is a 64-bit 'ValInteger'. If using LLVM 21 or
    -- later, this can also be a null reference (i.e., 'Nothing'), a variable
    -- (i.e., @Just@ a 'DIGlobalVariable' or 'DILocalVariable'), or an
    -- expression (i.e., @Just@ a 'DIExpression').
  , dictAlign          :: Word64
  , dictOffset         :: Maybe (ValMd' lab)
    -- ^ If using LLVM 20 or older, this will always be @Just@ an 'ValMdValue',
    -- where the underlying value is a 64-bit 'ValInteger'. If using LLVM 21 or
    -- later, this can also be a null reference (i.e., 'Nothing'), a variable
    -- (i.e., @Just@ a 'DIGlobalVariable' or 'DILocalVariable'), or an
    -- expression (i.e., @Just@ a 'DIExpression').
  , dictFlags          :: DIFlags
  , dictElements       :: Maybe (ValMd' lab)
  , dictRuntimeLang    :: DwarfLang
  , dictVTableHolder   :: Maybe (ValMd' lab)
  , dictTemplateParams :: Maybe (ValMd' lab)
  , dictIdentifier     :: Maybe String
  , dictDiscriminator  :: Maybe (ValMd' lab)
  , dictDataLocation   :: Maybe (ValMd' lab)
  , dictAssociated     :: Maybe (ValMd' lab)
  , dictAllocated      :: Maybe (ValMd' lab)
  , dictRank           :: Maybe (ValMd' lab)
  , dictAnnotations    :: Maybe (ValMd' lab) -- ^ Introduced in LLVM 14.
  , dictNumExtraInhabitants :: Word64        -- ^ added in LLVM 20.
  , dictSpecification  :: Maybe (ValMd' lab) -- ^ added in LLVM 20.
  , dictEnumKind       :: Maybe Word32       -- ^ added in LLVM 20.
  , dictBitStride      :: Maybe (ValMd' lab) -- ^ added in LLVM 20.
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DICompositeType = DICompositeType' BlockLabel

data DIDerivedType' lab = DIDerivedType
  { didtTag :: DwarfTag
  , didtName :: Maybe String
  , didtFile :: Maybe (ValMd' lab)
  , didtLine :: Word32
  , didtScope :: Maybe (ValMd' lab)
  , didtBaseType :: Maybe (ValMd' lab)
  , didtSize :: Maybe (ValMd' lab)
    -- ^ If using LLVM 20 or older, this will always be @Just@ an 'ValMdValue',
    -- where the underlying value is a 64-bit 'ValInteger'. If using LLVM 21 or
    -- later, this can also be a null reference (i.e., 'Nothing'), a variable
    -- (i.e., @Just@ a 'DIGlobalVariable' or 'DILocalVariable'), or an
    -- expression (i.e., @Just@ a 'DIExpression').
  , didtAlign :: Word64
  , didtOffset :: Maybe (ValMd' lab)
    -- ^ If using LLVM 20 or older, this will always be @Just@ an 'ValMdValue',
    -- where the underlying value is a 64-bit 'ValInteger'. If using LLVM 21 or
    -- later, this can also be a null reference (i.e., 'Nothing'), a variable
    -- (i.e., @Just@ a 'DIGlobalVariable' or 'DILocalVariable'), or an
    -- expression (i.e., @Just@ a 'DIExpression').
  , didtFlags :: DIFlags
  , didtExtraData :: Maybe (ValMd' lab)
  , didtDwarfAddressSpace :: Maybe Word32
  -- ^ Introduced in LLVM 5.
  --
  -- The 'Maybe' encodes the possibility that there is no associated address
  -- space (in LLVM, the sentinel value @0@ is used for this).
  , didtAnnotations :: Maybe (ValMd' lab)
  -- ^ Introduced in LLVM 14
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DIDerivedType = DIDerivedType' BlockLabel

data DIExpression = DIExpression
  { dieElements :: [Word64]
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

data DIFile = DIFile
  { difFilename  :: FilePath
  , difDirectory :: FilePath
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

data DIGlobalVariable' lab = DIGlobalVariable
  { digvScope                :: Maybe (ValMd' lab)
  , digvName                 :: Maybe String
  , digvLinkageName          :: Maybe String
  , digvFile                 :: Maybe (ValMd' lab)
  , digvLine                 :: Word32
  , digvType                 :: Maybe (ValMd' lab)
  , digvIsLocal              :: Bool
  , digvIsDefinition         :: Bool
  , digvVariable             :: Maybe (ValMd' lab)
  , digvDeclaration          :: Maybe (ValMd' lab)
  , digvAlignment            :: Maybe Word32
  , digvAnnotations          :: Maybe (ValMd' lab)
    -- ^ Introduced in LLVM 14.
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DIGlobalVariable = DIGlobalVariable' BlockLabel

data DIGlobalVariableExpression' lab = DIGlobalVariableExpression
  { digveVariable   :: Maybe (ValMd' lab)
  , digveExpression :: Maybe (ValMd' lab)
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DIGlobalVariableExpression = DIGlobalVariableExpression' BlockLabel

data DILexicalBlock' lab = DILexicalBlock
  { dilbScope  :: Maybe (ValMd' lab)
  , dilbFile   :: Maybe (ValMd' lab)
  , dilbLine   :: Word32
  , dilbColumn :: Word16
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DILexicalBlock = DILexicalBlock' BlockLabel

data DILexicalBlockFile' lab = DILexicalBlockFile
  { dilbfScope         :: ValMd' lab
  , dilbfFile          :: Maybe (ValMd' lab)
  , dilbfDiscriminator :: Word32
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DILexicalBlockFile = DILexicalBlockFile' BlockLabel

data DILocalVariable' lab = DILocalVariable
  { dilvScope :: Maybe (ValMd' lab)
  , dilvName :: Maybe String
  , dilvFile :: Maybe (ValMd' lab)
  , dilvLine :: Word32
  , dilvType :: Maybe (ValMd' lab)
  , dilvArg :: Word16
  , dilvFlags :: DIFlags
  , dilvAlignment :: Maybe Word32
    -- ^ Introduced in LLVM 4.
  , dilvAnnotations :: Maybe (ValMd' lab)
    -- ^ Introduced in LLVM 14.
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DILocalVariable = DILocalVariable' BlockLabel

data DISubprogram' lab = DISubprogram
  { dispScope          :: Maybe (ValMd' lab)
  , dispName           :: Maybe String
  , dispLinkageName    :: Maybe String
  , dispFile           :: Maybe (ValMd' lab)
  , dispLine           :: Word32
  , dispType           :: Maybe (ValMd' lab)
  , dispIsLocal        :: Bool
  , dispIsDefinition   :: Bool
  , dispScopeLine      :: Word32
  , dispContainingType :: Maybe (ValMd' lab)
  , dispVirtuality     :: DwarfVirtuality
  , dispVirtualIndex   :: Word32
  , dispThisAdjustment :: Int64
  , dispFlags          :: DIFlags
  , dispIsOptimized    :: Bool
  , dispUnit           :: Maybe (ValMd' lab)
  , dispTemplateParams :: Maybe (ValMd' lab)
  , dispDeclaration    :: Maybe (ValMd' lab)
  , dispRetainedNodes  :: Maybe (ValMd' lab)
  , dispThrownTypes    :: Maybe (ValMd' lab)
  , dispAnnotations    :: Maybe (ValMd' lab)
    -- ^ Introduced in LLVM 14.
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DISubprogram = DISubprogram' BlockLabel

-- | The DISubrange is a Value subrange specification, usually associated with
-- arrays or enumerations.
--
-- * Early LLVM: only 'disrCount' and 'disrLowerBound' were present, where both
--   were a direct signed 64-bit value.  This corresponds to "format 0" in the
--   bitcode encoding (see reference below).
--
-- * LLVM 7: 'disrCount' changed to metadata representation ('ValMd').  The
--   metadata representation should only be a signed 64-bit integer, a Variable,
--   or an Expression.  This corresponds to "format 1" in the bitcode encoding.
--
-- * LLVM 11: 'disrLowerBound' was changed to a metadata representation and
--   'disrUpperBound' and 'disrStride' were added (primarily driven by the
--   addition of Fortran support in llvm).  All three should only be represented
--   as a signed 64-bit integer, a Variable, or an Expression.  This corresponds
--   to "format 2" in the bitcode encoding.  See
--   https://github.com/llvm/llvm-project/commit/d20bf5a for this change.
--
-- Also see
-- https://github.com/llvm/llvm-project/blob/bbe8cd1/llvm/lib/Bitcode/Reader/MetadataLoader.cpp#L1435-L1461
-- for how this is read from the bitcode encoding and the use of the format
-- values mentioned above.

data DISubrange' lab = DISubrange
  { disrCount      :: Maybe (ValMd' lab)
  , disrLowerBound :: Maybe (ValMd' lab)
  , disrUpperBound :: Maybe (ValMd' lab)
  , disrStride     :: Maybe (ValMd' lab)
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DISubrange = DISubrange' BlockLabel

data DISubroutineType' lab = DISubroutineType
  { distFlags     :: DIFlags
  , distTypeArray :: Maybe (ValMd' lab)
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DISubroutineType = DISubroutineType' BlockLabel

-- | See <https://releases.llvm.org/13.0.0/docs/LangRef.html#diarglist>.
newtype DIArgList' lab = DIArgList
  { dialArgs :: [ValMd' lab]
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DIArgList = DIArgList' BlockLabel

-- Aggregate Utilities ---------------------------------------------------------

data IndexResult
  = Invalid                             -- ^ An invalid use of GEP
  | HasType Type                        -- ^ A resolved type
  | Resolve Ident (Type -> IndexResult) -- ^ Continue, after resolving an alias
  deriving (Generic, Typeable)

isInvalid :: IndexResult -> Bool
isInvalid ir = case ir of
  Invalid -> True
  _       -> False

-- | Resolves the type of a GEP instruction. Type aliases are resolved
-- using the given function. An invalid use of GEP or one relying
-- on unknown type aliases will return 'Nothing'
resolveGepFull ::
  (Ident -> Maybe Type) {- ^ Type alias resolution -} ->
  Type                  {- ^ Base type used for calculations -} ->
  Typed (Value' lab)    {- ^ Pointer value         -} ->
  [Typed (Value' lab)]  {- ^ Path                  -} ->
  Maybe Type            {- ^ Type of result        -}
resolveGepFull env baseTy tv ixs = go (resolveGep baseTy tv ixs)
  where
  go Invalid                = Nothing
  go (HasType result)       = Just result
  go (Resolve ident resume) = go . resume =<< env ident


-- | Resolve the type of a GEP instruction.  Note that the type produced is the
-- type of the result, not necessarily a pointer.
resolveGep :: Type -> Typed (Value' lab) -> [Typed (Value' lab)] -> IndexResult
resolveGep baseTy tv ixs =
  case ixs of
    v:ixs0
      |  -- If headed by a pointer and the first index value has a valid GEP
         -- index type, proceed to resolve the body of the GEP instruction.
         isPointer t
      ,  isGepIndex v
      -> resolveGepBody baseTy ixs0

      |  -- If headed by a pointer and the first index has an alias type,
         -- resolve the alias and try again.
         isPointer t
      ,  Just i <- elimAlias (typedType v)
      -> Resolve i (\ty' -> resolveGep baseTy tv (Typed ty' (typedValue v):ixs0))

    _ |  -- If headed by a value with an alias type, resolve the alias and
         -- try again.
         Alias i <- t
      -> Resolve i (\ty' -> resolveGep baseTy (Typed ty' (typedValue tv)) ixs)

      |  -- Otherwise, the GEP instruction is invalid.
         otherwise
      -> Invalid
  where
    t = typedType tv

-- | Resolve the type of a GEP instruction.  This assumes that the input has
-- already been processed as a pointer.
resolveGepBody :: Type -> [Typed (Value' lab)] -> IndexResult
resolveGepBody (Struct fs) (v:ixs)
  | Just i <- isGepStructIndex v, genericLength fs > i =
    resolveGepBody (genericIndex fs i) ixs
resolveGepBody (PackedStruct fs) (v:ixs)
  | Just i <- isGepStructIndex v, genericLength fs > i =
    resolveGepBody (genericIndex fs i) ixs
resolveGepBody (Alias name) is
  | not (null is) =
    Resolve name (\ty' -> resolveGepBody ty' is)
resolveGepBody (Array _ ty') (v:ixs)
  | isGepIndex v =
    resolveGepBody ty' ixs
resolveGepBody (Vector _ tp) [val]
  | isGepIndex val =
    HasType tp
resolveGepBody ty (v:ixs)
  | Just i <- elimAlias (typedType v) =
    Resolve i (\ty' -> resolveGepBody ty (Typed ty' (typedValue v):ixs))
resolveGepBody ty [] =
    HasType ty
resolveGepBody _ _ =
    Invalid

isGepIndex :: Typed (Value' lab) -> Bool
isGepIndex tv =
  isPrimTypeOf isInteger (typedType tv) ||
  isVectorOf (isPrimTypeOf isInteger) (typedType tv)

isGepStructIndex :: Typed (Value' lab) -> Maybe Integer
isGepStructIndex tv = do
  guard (isGepIndex tv)
  elimValInteger (typedValue tv)

resolveValueIndex :: Type -> [Int32] -> IndexResult
resolveValueIndex ty is@(ix:ixs) = case ty of
  Struct fs | genericLength fs > ix
    -> resolveValueIndex (genericIndex fs ix) ixs

  PackedStruct fs | genericLength fs > ix
    -> resolveValueIndex (genericIndex fs ix) ixs

  Array n ty' | fromIntegral ix < n
    -> resolveValueIndex ty' ixs

  Alias name
    -> Resolve name (\ty' -> resolveValueIndex ty' is)

  _ -> Invalid
resolveValueIndex ty [] = HasType ty
