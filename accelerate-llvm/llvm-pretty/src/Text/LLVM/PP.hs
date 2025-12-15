{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Module      :  Text.LLVM.PP
-- Copyright   :  Trevor Elliott 2011-2016
-- License     :  BSD3
--
-- Maintainer  :  awesomelyawesome@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- This is the pretty-printer for llvm assembly versions 3.6 and lower.
--
module Text.LLVM.PP where

import Text.LLVM.AST
import Text.LLVM.Triple.AST (TargetTriple)
import Text.LLVM.Triple.Print (printTriple)

import Control.Applicative ((<|>))
import Data.Bits ( shiftR, (.&.) )
import Data.Char (isAlphaNum,isAscii,isDigit,isPrint,ord,toUpper)
import Data.List ( intersperse, nub )
import qualified Data.Map as Map
import Data.Maybe (catMaybes,fromMaybe,isJust)
import GHC.Float (castDoubleToWord64, float2Double)
import Numeric (showHex)
import Text.PrettyPrint.HughesPJ
import Data.Int
import Prelude hiding ((<>))


-- Pretty-printer Config -------------------------------------------------------


-- | The value used to specify the LLVM major version.  The LLVM text format
-- (i.e. assembly code) changes with different versions of LLVM, so this value is
-- used to select the version the output should be generated for.
--
-- At the current time, changes primarily occur when the LLVM major version
-- changes, and this is expected to be the case going forward, so it is
-- sufficient to reference the LLVM version by the single major version number.
-- There is one exception and one possible future exception to this approach:
--
--  1. During LLVM v3, there were changes in 3.5, 3.6, 3.7, and 3.8.  There are
--     explicit @ppLLVMnn@ function entry points for those versions, but in the
--     event that a numerical value is needed, we note the serendipitous fact
--     that prior to LLVM 4, there are exactly 4 versions we need to
--     differentiate and can therefore assign the values of 0, 1, 2, and 3 to
--     those versions (and we have no intention of supporting any other pre-4.0
--     versions at this point).
--
--  2. If at some future date, there are text format changes associated with a
--     minor version, then the LLVM version designation here will need to be
--     enhanced and made more sophisticated.  At the present time, the likelihood
--     of that is small enough that the current simple implementation is a
--     benefit over a more complex mechanism that might not be needed.
--
type LLVMVer = Int

-- | Helpers for specifying the LLVM versions prior to v4
llvmV3_5, llvmV3_6, llvmV3_7, llvmV3_8 :: LLVMVer
llvmV3_5 = 0
llvmV3_6 = 1
llvmV3_7 = 2
llvmV3_8 = 3

-- | This value should be updated when support is added for new LLVM versions;
-- this is used for defaulting and otherwise reporting the maximum LLVM version
-- known to be supported.
llvmVlatest :: LLVMVer
llvmVlatest = 19


-- | The differences between various versions of the llvm textual AST.
newtype Config = Config { cfgVer :: LLVMVer }

withConfig :: Config -> ((?config :: Config) => a) -> a
withConfig cfg body = let ?config = cfg in body


ppLLVM :: LLVMVer -> ((?config :: Config) => a) -> a
ppLLVM llvmver = withConfig Config { cfgVer = llvmver }

ppLLVM35, ppLLVM36, ppLLVM37, ppLLVM38 :: ((?config :: Config) => a) -> a

ppLLVM35 = withConfig Config { cfgVer = llvmV3_5 }
ppLLVM36 = withConfig Config { cfgVer = llvmV3_6 }
ppLLVM37 = withConfig Config { cfgVer = llvmV3_7 }
ppLLVM38 = withConfig Config { cfgVer = llvmV3_8 }

llvmVer :: (?config :: Config) => LLVMVer
llvmVer = cfgVer ?config

llvmVerToString :: LLVMVer -> String
llvmVerToString 0 = "3.5"
llvmVerToString 1 = "3.6"
llvmVerToString 2 = "3.7"
llvmVerToString 3 = "3.8"
llvmVerToString n
  | n >= 4    = show n
  | otherwise = error $ "Invalid LLVMVer: " ++ show n

-- | This is a helper function for when a list of parameters is gated by a
-- condition (usually the llvmVer value).
when' :: Monoid a => Bool -> a -> a
when' c l = if c then l else mempty


-- | This type encapsulates the ability to convert an object into Doc
-- format. Using this abstraction allows for a consolidated representation of the
-- declaration.  Most pretty-printing for LLVM elements will have a @'Fmt' a@
-- function signature for that element.
type Fmt a = (?config :: Config) => a -> Doc


-- | The LLVMPretty class has instances for most AST elements.  It allows the
-- conversion of an AST element (and its sub-elements) into a Doc assembly format
-- by simply using the 'llvmPP' method rather than needing to explicitly invoke
-- the specific pretty-printing function for that element.
class LLVMPretty a where llvmPP :: Fmt a

instance LLVMPretty Module where llvmPP = ppModule
instance LLVMPretty Symbol where llvmPP = ppSymbol
instance LLVMPretty Ident  where llvmPP = ppIdent


-- Modules ---------------------------------------------------------------------

ppModule :: Fmt Module
ppModule m = foldr ($+$) empty
  $ ppSourceName (modSourceName m)
  : ppTargetTriple (modTriple m)
  : ppDataLayout (modDataLayout m)
  : ppInlineAsm  (modInlineAsm m)
  : concat [ map ppTypeDecl    (modTypes m)
           , map ppGlobal      (modGlobals m)
           , map ppGlobalAlias (modAliases m)
           , map ppDeclare     (modDeclares m)
           , map ppDefine      (modDefines m)
           , map ppNamedMd     (modNamedMd m)
           , map ppUnnamedMd   (modUnnamedMd m)
           , map ppComdat      (Map.toList (modComdat m))
           ]


-- Source filename -------------------------------------------------------------

ppSourceName :: Fmt (Maybe String)
ppSourceName Nothing   = empty
ppSourceName (Just sn) = "source_filename" <+> char '=' <+> doubleQuotes (text sn)

-- Metadata --------------------------------------------------------------------

ppNamedMd :: Fmt NamedMd
ppNamedMd nm =
  sep [ ppMetadata (text (nmName nm)) <+> char '='
      , ppMetadata (braces (commas (map (ppMetadata . int) (nmValues nm)))) ]

ppUnnamedMd :: Fmt UnnamedMd
ppUnnamedMd um =
  sep [ ppMetadata (int (umIndex um)) <+> char '='
      , distinct <+> ppValMd (umValues um) ]
  where
  distinct | umDistinct um = "distinct"
           | otherwise     = empty


-- Aliases ---------------------------------------------------------------------

ppGlobalAlias :: Fmt GlobalAlias
ppGlobalAlias g = ppSymbol (aliasName g)
              <+> char '='
              <+> ppMaybe ppLinkage (aliasLinkage g)
              <+> ppMaybe ppVisibility (aliasVisibility g)
              <+> body
  where
  val  = aliasTarget g
  body = case val of
    ValSymbol _sym -> ppType (aliasType g) <+> ppValue val
    _              -> ppValue val


-- Target triple ---------------------------------------------------------------

-- | Pretty print a 'TargetTriple'
ppTargetTriple :: Fmt TargetTriple
ppTargetTriple triple = "target" <+> "triple" <+> char '='
    <+> doubleQuotes (text (printTriple triple))

-- Data Layout -----------------------------------------------------------------

-- | Pretty print a data layout specification.
ppDataLayout :: Fmt DataLayout
ppDataLayout [] = empty
ppDataLayout ls = "target" <+> "datalayout" <+> char '='
    <+> doubleQuotes (hcat (intersperse (char '-') (map ppLayoutSpec ls)))

-- | Pretty print a single layout specification.
ppLayoutSpec :: Fmt LayoutSpec
ppLayoutSpec ls =
  case ls of
    BigEndian                 -> char 'E'
    LittleEndian              -> char 'e'
    PointerSize ps            -> char 'p' <> ppPointerSize ps
    IntegerSize sz            -> char 'i' <> ppStorage sz
    VectorSize  sz            -> char 'v' <> ppStorage sz
    FloatSize   sz            -> char 'f' <> ppStorage sz
    StackObjSize sz           -> char 's' <> ppStorage sz
    AggregateSize Nothing a   -> char 'a' <> char ':' <> ppAlignment a
    AggregateSize (Just s) a  -> char 'a' <> int s <> char ':' <> ppAlignment a
    NativeIntSize szs         ->
      char 'n' <> hcat (punctuate (char ':') (map int szs))
    StackAlign a              -> char 'S' <> int a
    ProgramAddrSpace as       -> char 'P' <> int as
    GlobalAddrSpace as        -> char 'G' <> int as
    AllocaAddrSpace as        -> char 'A' <> int as
    FunctionPointerAlign ty abi ->
      char 'F' <> ppFunctionPointerAlignType ty <> int abi
    Mangling m                -> char 'm' <> char ':' <> ppMangling m
    NonIntegralPointerSpaces asl ->
      "ni:" <> hcat (punctuate (char ':') (map int asl))

ppPointerSize :: Fmt PointerSize
ppPointerSize ps =
  if ptrAddrSpace ps == 0
  then char ':' <> ppStorage (ptrStorage ps)
       <> ppOptColonInt (ptrAddrIndexSize ps)
  else int (ptrAddrSpace ps) <> char ':' <> ppStorage (ptrStorage ps)
       <> ppOptColonInt (ptrAddrIndexSize ps)

ppStorage :: Fmt Storage
ppStorage s = int (storageSize s) <> char ':'
              <> ppAlignment (storageAlignment s)

ppAlignment :: Fmt Alignment
ppAlignment a = int (alignABI a) <> ppOptColonInt (alignPreferred a)

ppOptColonInt :: Fmt (Maybe Int)
ppOptColonInt = \case
  Nothing -> empty
  Just i  -> char ':' <> int i

ppFunctionPointerAlignType :: Fmt FunctionPointerAlignType
ppFunctionPointerAlignType ty =
  case ty of
    IndependentOfFunctionAlign -> char 'i'
    MultipleOfFunctionAlign -> char 'n'

ppMangling :: Fmt Mangling
ppMangling ElfMangling         = char 'e'
ppMangling GoffMangling        = char 'l'
ppMangling MipsMangling        = char 'm'
ppMangling MachOMangling       = char 'o'
ppMangling WindowsCoffMangling = char 'w'
ppMangling WindowsX86CoffMangling = char 'x'
ppMangling XCoffMangling       = char 'a'


-- Inline Assembly -------------------------------------------------------------

-- | Pretty-print the inline assembly block.
ppInlineAsm :: Fmt InlineAsm
ppInlineAsm  = foldr ($+$) empty . map ppLine
  where
  ppLine l = "module asm" <+> doubleQuotes (text l)


-- Identifiers -----------------------------------------------------------------

ppIdent :: Fmt Ident
ppIdent (Ident n)
  | validIdentifier n = char '%' <> text n
  | otherwise         = char '%' <> ppStringLiteral n

-- | According to the LLVM Language Reference Manual, the regular
-- expression for LLVM identifiers is "[-a-zA-Z$._][-a-zA-Z$._0-9]".
-- Identifiers may also be strings of one or more decimal digits.
validIdentifier :: String -> Bool
validIdentifier [] = False
validIdentifier s@(c0 : cs)
  | isDigit c0 = all isDigit cs
  | otherwise  = all isIdentChar s
  where
  isIdentChar :: Char -> Bool
  isIdentChar c = isAlphaNum c || c `elem` ("-$._" :: [Char])


-- Symbols ---------------------------------------------------------------------

ppSymbol :: Fmt Symbol
ppSymbol (Symbol n)
  | validIdentifier n = char '@' <> text n
  | otherwise         = char '@' <> ppStringLiteral n


-- Types -----------------------------------------------------------------------

ppPrimType :: Fmt PrimType
ppPrimType Label          = "label"
ppPrimType Void           = "void"
ppPrimType (Integer i)    = char 'i' <> integer (toInteger i)
ppPrimType (FloatType ft) = ppFloatType ft
ppPrimType X86mmx         = "x86mmx"
ppPrimType Metadata       = "metadata"

ppFloatType :: Fmt FloatType
ppFloatType Half      = "half"
ppFloatType Float     = "float"
ppFloatType Double    = "double"
ppFloatType Fp128     = "fp128"
ppFloatType X86_fp80  = "x86_fp80"
ppFloatType PPC_fp128 = "ppc_fp128"

ppType :: Fmt Type
ppType (PrimType pt)     = ppPrimType pt
ppType (Alias i)         = ppIdent i
ppType (Array len ty)    = brackets (integral len <+> char 'x' <+> ppType ty)
ppType (PtrTo ty addr)   = ppType ty <+> ppAddrSpace addr <> char '*'
ppType (PtrOpaque addr)  = "ptr" <+> ppAddrSpace addr
ppType (Struct ts)       = structBraces (commas (map ppType ts))
ppType (PackedStruct ts) = angles (structBraces (commas (map ppType ts)))
ppType (FunTy r as va)   = ppType r <> ppArgList va (map ppType as)
ppType (Vector len pt)   = angles (integral len <+> char 'x' <+> ppType pt)
ppType Opaque            = "opaque"

-- | If the address space is default (0), print nothing; otherwise, print
-- @addrspace(n)@.
ppAddrSpace :: Fmt AddrSpace
ppAddrSpace (AddrSpace 0) = empty
ppAddrSpace (AddrSpace n) = "addrspace(" <> integer (toInteger n) <> ")"

ppTypeDecl :: Fmt TypeDecl
ppTypeDecl td = ppIdent (typeName td) <+> char '='
            <+> "type" <+> ppType (typeValue td)


-- Declarations ----------------------------------------------------------------

ppGlobal :: Fmt Global
ppGlobal g = ppSymbol (globalSym g) <+> char '='
         <+> ppGlobalAttrs (isJust $ globalValue g) (globalAttrs g)
         <+> ppType (globalType g) <+> ppMaybe ppValue (globalValue g)
          <> ppAlign (globalAlign g)
          <> ppGlobalMetadata (Map.toList (globalMetadata g))

ppGlobalMetadata :: Fmt [(String, ValMd' BlockLabel)]
ppGlobalMetadata mds
  | null mds  = empty
  | otherwise = comma <+> commas (map step mds)
  where
  step (l,md) = ppMetadata (text l) <+> ppValMd md

-- | Pretty-print Global Attributes (usually associated with a global variable
-- declaration). The first argument to ppGlobalAttrs indicates whether there is a
-- value associated with this global declaration: a global declaration with a
-- value should not be identified as \"external\" and \"default\" visibility,
-- whereas one without a value may have those attributes.

ppGlobalAttrs :: Bool -> Fmt GlobalAttrs
ppGlobalAttrs hasValue ga
    -- LLVM 3.8 does not emit or parse linkage information w/ hidden visibility
    | llvmVer <= llvmV3_8
    , Just HiddenVisibility <- gaVisibility ga =
            ppVisibility HiddenVisibility <+> constant
    | Just External <- gaLinkage ga
    , Just DefaultVisibility <- gaVisibility ga
    , hasValue =
        -- Just show the value, no "external" or "default".  This is based on
        -- empirical testing as described in the comment above (testing the
        -- following 6 configurations:
        --   * uninitialized scalar
        --   * uninitialized structure
        --   * initialized scalar
        --   * initialized structure
        --   * external scalar
        --   * external structure
        constant
    | otherwise = ppMaybe ppLinkage (gaLinkage ga)
              <+> ppAddrSpace (gaAddrSpace ga)
              <+> ppMaybe ppVisibility (gaVisibility ga)
              <+> constant
  where
  constant | gaConstant ga = "constant"
           | otherwise     = "global"

ppDeclare :: Fmt Declare
ppDeclare d = "declare"
          <+> ppMaybe ppLinkage (decLinkage d)
          <+> ppMaybe ppVisibility (decVisibility d)
          <+> ppType (decRetType d)
          <+> ppSymbol (decName d)
           <> ppArgList (decVarArgs d) (map ppType (decArgs d))
          <+> hsep (ppFunAttr <$> decAttrs d)
          <> maybe empty ((char ' ' <>) . ppComdatName) (decComdat d)

ppComdatName :: Fmt String
ppComdatName s = "comdat" <> parens (char '$' <> text s)

ppComdat :: Fmt (String,SelectionKind)
ppComdat (n,k) = ppComdatName n <+> char '=' <+> text "comdat" <+> ppSelectionKind k

ppSelectionKind :: Fmt SelectionKind
ppSelectionKind k =
    case k of
      ComdatAny             -> "any"
      ComdatExactMatch      -> "exactmatch"
      ComdatLargest         -> "largest"
      ComdatNoDuplicates    -> "noduplicates"
      ComdatSameSize        -> "samesize"

ppDefine :: Fmt Define
ppDefine d = "define"
         <+> ppMaybe ppLinkage (defLinkage d)
         <+> ppMaybe ppVisibility (defVisibility d)
         <+> ppType (defRetType d)
         <+> ppSymbol (defName d)
          <> ppArgList (defVarArgs d) (map (ppTyped ppIdent) (defArgs d))
         <+> hsep (ppFunAttr <$> defAttrs d)
         <+> ppMaybe (\s  -> "section" <+> doubleQuotes (text s)) (defSection d)
         <+> ppMaybe (\gc -> "gc" <+> ppGC gc) (defGC d)
         <+> ppMds (defMetadata d)
         <+> char '{'
         $+$ vcat (map ppBasicBlock (defBody d))
         $+$ char '}'
  where
  ppMds mdm =
    case Map.toList mdm of
      [] -> empty
      mds -> hsep [ "!" <> text k <+> ppValMd md | (k, md) <- mds ]

-- FunAttr ---------------------------------------------------------------------

ppFunAttr :: Fmt FunAttr
ppFunAttr a =
  case a of
    AlignStack w    -> text "alignstack" <> parens (int w)
    Alwaysinline    -> text "alwaysinline"
    Builtin         -> text "builtin"
    Cold            -> text "cold"
    Convergent      -> onlyOnLLVM llvmV3_7 "Convergent" "convergent"
    InaccessibleMemOnly -> onlyOnLLVM llvmV3_8 "InaccessibleMemOnly" "inaccessiblememonly"
    Inlinehint      -> text "inlinehint"
    Jumptable       -> text "jumptable"
    Minsize         -> text "minsize"
    Naked           -> text "naked"
    Nobuiltin       -> text "nobuiltin"
    Noduplicate     -> text "noduplicate"
    Noimplicitfloat -> text "noimplicitfloat"
    Noinline        -> text "noinline"
    Nonlazybind     -> text "nonlazybind"
    Noredzone       -> text "noredzone"
    Noreturn        -> text "noreturn"
    Nounwind        -> text "nounwind"
    Optnone         -> text "optnone"
    Optsize         -> text "optsize"
    Readnone        -> text "readnone"
    Readonly        -> text "readonly"
    ReturnsTwice    -> text "returns_twice"
    SanitizeAddress -> text "sanitize_address"
    SanitizeMemory  -> text "sanitize_memory"
    SanitizeThread  -> text "sanitize_thread"
    SSP             -> text "ssp"
    SSPreq          -> text "sspreq"
    SSPstrong       -> text "sspstrong"
    UWTable         -> text "uwtable"

-- Basic Blocks ----------------------------------------------------------------

ppLabelDef :: Fmt BlockLabel
ppLabelDef (Named (Ident l)) = text l <> char ':'
ppLabelDef (Anon i)          = char ';' <+> "<label>:" <+> int i

ppLabel :: Fmt BlockLabel
ppLabel (Named l) = ppIdent l
ppLabel (Anon i)  = char '%' <> int i

ppBasicBlock :: Fmt BasicBlock
ppBasicBlock bb = ppMaybe ppLabelDef (bbLabel bb)
              $+$ nest 2 (vcat (map ppStmt (bbStmts bb)))


-- Statements ------------------------------------------------------------------

ppStmt :: Fmt Stmt
ppStmt stmt = case stmt of
  Result var i drs mds -> ppDebugRecords drs (ppIdent var <+> char '='
                                              <+> ppInstr i
                                               <> ppAttachedMetadata mds)
  Effect i drs mds     -> ppDebugRecords drs (ppInstr i
                                              <> ppAttachedMetadata mds)

ppAttachedMetadata :: Fmt [(String,ValMd)]
ppAttachedMetadata mds
  | null mds  = empty
  | otherwise = comma <+> commas (map step mds)
  where
  step (l,md) = ppMetadata (text l) <+> ppValMd md


-- Linkage ---------------------------------------------------------------------

ppLinkage :: Fmt Linkage
ppLinkage linkage = case linkage of
  Private                  -> "private"
  LinkerPrivate            -> "linker_private"
  LinkerPrivateWeak        -> "linker_private_weak"
  LinkerPrivateWeakDefAuto -> "linker_private_weak_def_auto"
  Internal                 -> "internal"
  AvailableExternally      -> "available_externally"
  Linkonce                 -> "linkonce"
  Weak                     -> "weak"
  Common                   -> "common"
  Appending                -> "appending"
  ExternWeak               -> "extern_weak"
  LinkonceODR              -> "linkonce_ddr"
  WeakODR                  -> "weak_odr"
  External                 -> "external"
  DLLImport                -> "dllimport"
  DLLExport                -> "dllexport"

ppVisibility :: Fmt Visibility
ppVisibility v = case v of
    DefaultVisibility   -> "default"
    HiddenVisibility    -> "hidden"
    ProtectedVisibility -> "protected"

ppGC :: Fmt GC
ppGC  = doubleQuotes . text . getGC


-- Expressions -----------------------------------------------------------------

ppTyped :: Fmt a -> Fmt (Typed a)
ppTyped fmt ty = ppType (typedType ty) <+> fmt (typedValue ty)

ppSignBits :: Bool -> Fmt Bool
ppSignBits nuw nsw = opt nuw "nuw" <+> opt nsw "nsw"

ppFMF1 :: Fmt FMF
ppFMF1 Ffast = "fast"
ppFMF1 Fnnan = "nnan"
ppFMF1 Fninf = "ninf"
ppFMF1 Fnsz = "nsz"
ppFMF1 Farcp = "arcp"
ppFMF1 Fcontract = "contract"
ppFMF1 Fafn = "afn"
ppFMF1 Freassoc = "reassoc"

ppFMF :: Fmt [FMF]
ppFMF = hsep . map ppFMF1

ppExact :: Fmt Bool
ppExact e = opt e "exact"

ppArithOp :: Fmt ArithOp
ppArithOp (Add nuw nsw) = "add" <+> ppSignBits nuw nsw
ppArithOp (FAdd fmf)    = "fadd" <+> ppFMF fmf
ppArithOp (Sub nuw nsw) = "sub" <+> ppSignBits nuw nsw
ppArithOp (FSub fmf)    = "fsub" <+> ppFMF fmf
ppArithOp (Mul nuw nsw) = "mul" <+> ppSignBits nuw nsw
ppArithOp (FMul fmf)    = "fmul" <+> ppFMF fmf
ppArithOp (UDiv e)      = "udiv" <+> ppExact e
ppArithOp (SDiv e)      = "sdiv" <+> ppExact e
ppArithOp (FDiv fmf)    = "fdiv" <+> ppFMF fmf
ppArithOp URem          = "urem"
ppArithOp SRem          = "srem"
ppArithOp (FRem fmf)    = "frem" <+> ppFMF fmf

ppUnaryArithOp :: Fmt UnaryArithOp
ppUnaryArithOp (FNeg fmf) = "fneg" <+> ppFMF fmf

ppBitOp :: Fmt BitOp
ppBitOp (Shl nuw nsw) = "shl"  <+> ppSignBits nuw nsw
ppBitOp (Lshr e)      = "lshr" <+> ppExact e
ppBitOp (Ashr e)      = "ashr" <+> ppExact e
ppBitOp And           = "and"
ppBitOp Or            = "or"
ppBitOp Xor           = "xor"

ppConvOp :: Fmt ConvOp
ppConvOp (Trunc nuw nsw) = "trunc" <+> ppSignBits nuw nsw
ppConvOp (ZExt nneg)  = "zext" <+> opt nneg "nneg"
ppConvOp SExt     = "sext"
ppConvOp FpTrunc  = "fptrunc"
ppConvOp FpExt    = "fpext"
ppConvOp FpToUi   = "fptoui"
ppConvOp FpToSi   = "fptosi"
ppConvOp (UiToFp nneg) = "uitofp" <+> opt nneg "nneg"
ppConvOp SiToFp   = "sitofp"
ppConvOp PtrToInt = "ptrtoint"
ppConvOp IntToPtr = "inttoptr"
ppConvOp BitCast  = "bitcast"

ppAtomicOrdering :: Fmt AtomicOrdering
ppAtomicOrdering Unordered = text "unordered"
ppAtomicOrdering Monotonic = text "monotonic"
ppAtomicOrdering Acquire   = text "acquire"
ppAtomicOrdering Release   = text "release"
ppAtomicOrdering AcqRel    = text "acq_rel"
ppAtomicOrdering SeqCst    = text "seq_cst"

ppAtomicOp :: Fmt AtomicRWOp
ppAtomicOp AtomicXchg = "xchg"
ppAtomicOp AtomicAdd  = "add"
ppAtomicOp AtomicSub  = "sub"
ppAtomicOp AtomicAnd  = "and"
ppAtomicOp AtomicNand = "nand"
ppAtomicOp AtomicOr   = "or"
ppAtomicOp AtomicXor  = "xor"
ppAtomicOp AtomicMax  = "max"
ppAtomicOp AtomicMin  = "min"
ppAtomicOp AtomicUMax = "umax"
ppAtomicOp AtomicUMin = "umin"
ppAtomicOp AtomicFAdd = onlyOnLLVM 9 "AtomicFAdd" "fadd"
ppAtomicOp AtomicFSub = onlyOnLLVM 9 "AtomicFSub" "fsub"
ppAtomicOp AtomicFMax = onlyOnLLVM 15 "AtomicFMax" "fmax"
ppAtomicOp AtomicFMin = onlyOnLLVM 15 "AtomicFMin" "fmin"
ppAtomicOp AtomicUIncWrap = onlyOnLLVM 16 "AtomicUIncWrap" "uinc_wrap"
ppAtomicOp AtomicUDecWrap = onlyOnLLVM 16 "AtomicUDecWrap" "udec_wrap"

ppScope ::  Fmt (Maybe String)
ppScope Nothing = empty
ppScope (Just s) = "syncscope" <> parens (doubleQuotes (text s))

ppInstr :: Fmt Instr
ppInstr instr = case instr of
  Ret tv                 -> "ret" <+> ppTyped ppValue tv
  RetVoid                -> "ret void"
  Arith op l r           -> ppArithOp op <+> ppTyped ppValue l
                         <> comma <+> ppValue r
  UnaryArith op a        -> ppUnaryArithOp op <+> ppTyped ppValue a
  Bit op l r             -> ppBitOp op <+> ppTyped ppValue l
                         <> comma <+> ppValue r
  Conv op a ty           -> ppConvOp op <+> ppTyped ppValue a
                        <+> "to" <+> ppType ty
  Call tc fmf ty f args  -> ppCall tc fmf ty f args
  CallBr ty f args u es  -> ppCallBr ty f args u es
  Alloca ty len align    -> ppAlloca ty len align
  Load vol ty ptr mo ma  -> ppLoad vol ty ptr mo ma
  Store vol a ptr mo ma  -> ppStore vol a ptr mo ma
  Fence scope order      -> "fence" <+> ppScope scope <+> ppAtomicOrdering order
  CmpXchg w v p a n s o o' -> "cmpxchg" <+> opt w "weak"
                         <+> opt v "volatile"
                         <+> ppTyped ppValue p
                         <> comma <+> ppTyped ppValue a
                         <> comma <+> ppTyped ppValue n
                         <+> ppScope s
                         <+> ppAtomicOrdering o
                         <+> ppAtomicOrdering o'
  AtomicRW v op p a s o  -> "atomicrmw"
                         <+> opt v "volatile"
                         <+> ppAtomicOp op
                         <+> ppTyped ppValue p
                         <> comma <+> ppTyped ppValue a
                         <+> ppScope s
                         <+> ppAtomicOrdering o
  ICmp samesign op l r   -> "icmp" <+> opt samesign "samesign" <+> ppICmpOp op
                        <+> ppTyped ppValue l <> comma <+> ppValue r
  FCmp fmf op l r        -> "fcmp" <+> ppFMF fmf <+> ppFCmpOp op
                        <+> ppTyped ppValue l <> comma <+> ppValue r
  Phi fmf ty vls         -> "phi" <+> ppFMF fmf <+> ppType ty
                        <+> commas (map ppPhiArg vls)
  Select fmf c t f       -> "select" <+> ppFMF fmf <+> ppTyped ppValue c
                         <> comma <+> ppTyped ppValue t
                         <> comma <+> ppTyped ppValue (f <$ t)
  ExtractValue v is      -> "extractvalue" <+> ppTyped ppValue v
                         <> comma <+> (commas (map integral is))
  InsertValue a v is     -> "insertvalue" <+> ppTyped ppValue a
                         <> comma <+> ppTyped ppValue v
                         <> comma <+> commas (map integral is)
  ShuffleVector a b m    -> "shufflevector" <+> ppTyped ppValue a
                         <> comma <+> ppTyped ppValue (b <$ a)
                         <> comma <+> ppTyped ppValue m
  GEP gf ty ptr ixs      -> ppGEP gf ty ptr ixs
  Comment str            -> char ';' <+> text str
  Jump i                 -> "br"
                        <+> ppTypedLabel i
  Br c t f               -> "br" <+> ppTyped ppValue c
                         <> comma <+> ppType (PrimType Label)
                        <+> ppLabel t
                         <> comma <+> ppType (PrimType Label)
                        <+> ppLabel f
  Invoke ty f args to uw -> ppInvoke ty f args to uw
  Unreachable            -> "unreachable"
  Unwind                 -> "unwind"
  VaArg al t             -> "va_arg" <+> ppTyped ppValue al
                         <> comma <+> ppType t
  ExtractElt v i         -> "extractelement"
                        <+> ppTyped ppValue v
                         <> comma <+> ppVectorIndex i
  InsertElt v e i        -> "insertelement"
                        <+> ppTyped ppValue v
                         <> comma <+> ppTyped ppValue e
                         <> comma <+> ppVectorIndex i
  IndirectBr d ls        -> "indirectbr"
                        <+> ppTyped ppValue d
                         <> comma
                        <+> char '['
                        <+> commas (map ppTypedLabel ls)
                        <+> char ']'
  Switch c d ls          -> "switch"
                        <+> ppTyped ppValue c
                         <> comma <+> ppTypedLabel d
                        <+> char '['
                         $$ nest 2 (vcat (map (ppSwitchEntry (typedType c)) ls))
                         $$ char ']'
  LandingPad ty mfn c cs  ->
        case mfn of
            Just fn -> "landingpad"
                        <+> ppType ty
                        <+> "personality"
                        <+> ppTyped ppValue fn
                        $$ nest 2 (ppClauses c cs)
            Nothing -> "landingpad"
                        <+> ppType ty
                        $$ nest 2 (ppClauses c cs)
  Resume tv           -> "resume" <+> ppTyped ppValue tv
  Freeze tv           -> "freeze" <+> ppTyped ppValue tv

ppLoad :: Bool
       -> Type
       -> Typed (Value' BlockLabel)
       -> Maybe AtomicOrdering
       -> Fmt (Maybe Align)
ppLoad volatile ty ptr mo ma =
  "load" <+> (if volatile then "volatile" else empty)
         <+> (if isAtomic   then "atomic" else empty)
         <+> (if isExplicit then explicit else empty)
         <+> ppTyped ppValue ptr
         <+> ordering
          <> ppAlign ma

  where
  isAtomic = isJust mo

  isExplicit = llvmVer >= llvmV3_7

  ordering =
    case mo of
      Just ao -> ppAtomicOrdering ao
      _       -> empty

  explicit = ppType ty <> comma

ppStore :: Bool
        -> Typed (Value' BlockLabel)
        -> Typed (Value' BlockLabel)
        -> Maybe AtomicOrdering
        -> Fmt (Maybe Align)
ppStore volatile ptr val mo ma =
  "store" <+> (if volatile then "volatile" else empty)
          <+> (if isJust mo  then "atomic" else empty)
          <+> ppTyped ppValue ptr <> comma
          <+> ppTyped ppValue val
          <+> case mo of
                Just ao -> ppAtomicOrdering ao
                _       -> empty
          <> ppAlign ma


ppClauses :: Bool -> Fmt [Clause]
ppClauses isCleanup cs = vcat (cleanup : map ppClause cs)
  where
  cleanup | isCleanup = "cleanup"
          | otherwise = empty

ppClause :: Fmt Clause
ppClause c = case c of
  Catch  tv -> "catch"  <+> ppTyped ppValue tv
  Filter tv -> "filter" <+> ppTyped ppValue tv


ppTypedLabel :: Fmt BlockLabel
ppTypedLabel i = ppType (PrimType Label) <+> ppLabel i

ppSwitchEntry :: Type -> Fmt (Integer,BlockLabel)
ppSwitchEntry ty (i,l) = ppType ty <+> integer i <> comma <+> ppTypedLabel l

ppVectorIndex :: Fmt Value
ppVectorIndex i = ppType (PrimType (Integer 32)) <+> ppValue i

ppAlign :: Fmt (Maybe Align)
ppAlign Nothing      = empty
ppAlign (Just align) = comma <+> "align" <+> int align

ppAlloca :: Type -> Maybe (Typed Value) -> Fmt (Maybe Int)
ppAlloca ty mbLen mbAlign = "alloca" <+> ppType ty <> len <> align
  where
  len = fromMaybe empty $ do
    l <- mbLen
    return (comma <+> ppTyped ppValue l)
  align = fromMaybe empty $ do
    a <- mbAlign
    return (comma <+> "align" <+> int a)

ppCall :: Bool -> [FMF] -> Type -> Value -> Fmt [Typed Value]
ppCall tc fmf ty f args
  | tc        = "tail" <+> body
  | otherwise = body
  where
  body = "call" <+> ppFMF fmf <+> ppCallSym ty f
      <> parens (commas (map (ppTyped ppValue) args))

-- | Note that the textual syntax changed in LLVM 10 (@callbr@ was introduced in
-- LLVM 9).
ppCallBr :: Type -> Value -> [Typed Value] -> BlockLabel -> Fmt [BlockLabel]
ppCallBr ty f args to indirectDests =
  "callbr"
     <+> ppCallSym ty f <> parens (commas (map (ppTyped ppValue) args))
     <+> "to" <+> ppLab to <+> brackets (commas (map ppLab indirectDests))
  where
    ppLab l = ppType (PrimType Label) <+> ppLabel l

-- | Print out the @<ty>|<fnty> <fnptrval>@ portion of a @call@, @callbr@, or
-- @invoke@ instruction, where:
--
-- * @<ty>@ is the return type.
--
-- * @<fnty>@ is the overall function type.
--
-- * @<fnptrval>@ is a pointer value, where the memory it points to is treated
--   as a value of type @<fnty>@.
--
-- The LLVM Language Reference Manual indicates that either @<ty>@ or @<fnty>@
-- can be used, but in practice, @<ty>@ is typically preferred unless the
-- function type involves varargs. We adopt the same convention here.
ppCallSym :: Type -> Fmt Value
ppCallSym ty val = pp_ty <+> ppValue val
  where
    pp_ty =
      case ty of
        FunTy res args va
          |  va
          -> ppType res <+> ppArgList va (map ppType args)
          |  otherwise
          -> ppType res
        _ -> ppType ty

ppGEP :: [GEPAttr] -> Type -> Typed Value -> Fmt [Typed Value]
ppGEP gf ty ptr ixs =
  "getelementptr"
    <+> (if inlineIsBool
         then (if GEP_Inbounds `elem` gf then "inbounds" else empty)
         else ppGepFlags gf)
    <+> (if isExplicit then explicit else empty)
    <+> commas (map (ppTyped ppValue) (ptr:ixs))
  where
  isExplicit = llvmVer >= llvmV3_7
  inlineIsBool = llvmVer < 19

  explicit = ppType ty <> comma

ppInvoke :: Type -> Value -> [Typed Value] -> BlockLabel -> Fmt BlockLabel
ppInvoke ty f args to uw = body
  where
  body = "invoke" <+> ppCallSym ty f
      <> parens (commas (map (ppTyped ppValue) args))
     <+> "to" <+> ppType (PrimType Label) <+> ppLabel to
     <+> "unwind" <+> ppType (PrimType Label) <+> ppLabel uw

ppPhiArg :: Fmt (Value,BlockLabel)
ppPhiArg (v,l) = char '[' <+> ppValue v <> comma <+> ppLabel l <+> char ']'

ppICmpOp :: Fmt ICmpOp
ppICmpOp Ieq  = "eq"
ppICmpOp Ine  = "ne"
ppICmpOp Iugt = "ugt"
ppICmpOp Iuge = "uge"
ppICmpOp Iult = "ult"
ppICmpOp Iule = "ule"
ppICmpOp Isgt = "sgt"
ppICmpOp Isge = "sge"
ppICmpOp Islt = "slt"
ppICmpOp Isle = "sle"

ppFCmpOp :: Fmt FCmpOp
ppFCmpOp Ffalse = "false"
ppFCmpOp Foeq   = "oeq"
ppFCmpOp Fogt   = "ogt"
ppFCmpOp Foge   = "oge"
ppFCmpOp Folt   = "olt"
ppFCmpOp Fole   = "ole"
ppFCmpOp Fone   = "one"
ppFCmpOp Ford   = "ord"
ppFCmpOp Fueq   = "ueq"
ppFCmpOp Fugt   = "ugt"
ppFCmpOp Fuge   = "uge"
ppFCmpOp Fult   = "ult"
ppFCmpOp Fule   = "ule"
ppFCmpOp Fune   = "une"
ppFCmpOp Funo   = "uno"
ppFCmpOp Ftrue  = "true"

ppValue' :: Fmt i -> Fmt (Value' i)
ppValue' pp val = case val of
  ValInteger i       -> integer i
  ValBool b          -> ppBool b
  -- Note: for +Inf/-Inf/NaNs, we want to output the bit-correct sequence
  ValFloat f         ->
    -- WARNING: You should **not** use `castFloatToWord32` or `float` here.  LLVM IR does not
    -- support 32-bit floating point constants, instead it wants to see 32-bit compatible 64-bit
    -- constants.  We want to preserve the exact mantissa (zero-extended to the right from 23 to
    -- 52 bits), which happens to be the behavior of `float2Double`.
    text "0x" <> text (showHex (castDoubleToWord64 (float2Double f)) "")
  ValDouble d        ->
    if isInfinite d || isNaN d
      then text "0x" <> text (showHex (castDoubleToWord64 d) "")
      else double d
  ValFP80 (FP80_LongDouble e s) ->
    -- shown as 0xK<<20-hex-digits>>, per
    -- https://llvm.org/docs/LangRef.html#simple-constants
    let pad n | n < 0x10  = shows (0::Int) . showHex n
              | otherwise = showHex n
        fld v i = pad ((v `shiftR` (i * 8)) .&. 0xff)
    in "0xK" <> text (foldr (fld e) (foldr (fld s) "" $ reverse [0..7::Int]) [1, 0])
  ValIdent i         -> ppIdent i
  ValSymbol s        -> ppSymbol s
  ValNull            -> "null"
  ValArray ty es     -> brackets
                      $ commas (map (ppTyped (ppValue' pp) . Typed ty) es)
  ValVector ty es   -> angles $ commas
                     $ map (ppTyped (ppValue' pp) . Typed ty) es
  ValStruct fs       -> structBraces (commas (map (ppTyped (ppValue' pp)) fs))
  ValPackedStruct fs -> angles
                      $ structBraces (commas (map (ppTyped (ppValue' pp)) fs))
  ValString s        -> char 'c' <> ppStringLiteral (map (toEnum . fromIntegral) s)
  ValConstExpr ce    -> ppConstExpr' pp ce
  ValUndef           -> "undef"
  ValLabel l         -> pp l
  ValZeroInit        -> "zeroinitializer"
  ValAsm s a i c     -> ppAsm s a i c
  ValMd m            -> ppValMd' pp m
  ValPoison          -> "poison"

ppValue :: Fmt Value
ppValue = ppValue' ppLabel

ppValMd' :: Fmt i -> Fmt (ValMd' i)
ppValMd' pp m = case m of
  ValMdString str   -> ppMetadata (ppStringLiteral str)
  ValMdValue tv     -> ppTyped (ppValue' pp) tv
  ValMdRef i        -> ppMetadata (int i)
  ValMdNode vs      -> ppMetadataNode' pp vs
  ValMdLoc l        -> ppDebugLoc' pp l
  ValMdDebugInfo di -> ppDebugInfo' pp di

ppValMd :: Fmt ValMd
ppValMd = ppValMd' ppLabel

ppDebugLoc' :: Fmt i -> Fmt (DebugLoc' i)
ppDebugLoc' pp dl = (if llvmVer >= llvmV3_7 then "!DILocation"
                                            else "!MDLocation")
             <> parens (commas [ "line:"   <+> integral (dlLine dl)
                               , "column:" <+> integral (dlCol dl)
                               , "scope:"  <+> ppValMd' pp (dlScope dl)
                               ] <> mbIA <> mbImplicit <>
                        when' (llvmVer >= 21) (mbAtomGroup <> mbAtomRank))

  where
  mbIA = case dlIA dl of
           Just md -> comma <+> "inlinedAt:" <+> ppValMd' pp md
           Nothing -> empty
  mbImplicit = if dlImplicit dl then comma <+> "implicit" else empty
  mbAtomGroup = if dlAtomGroup dl > 0
                  then comma <+> "atomGroup:" <+> integral (dlAtomGroup dl)
                  else empty
  mbAtomRank = if dlAtomRank dl > 0
                 then comma <+> "atomRank:" <+> integral (dlAtomRank dl)
                 else empty

ppDebugLoc :: Fmt DebugLoc
ppDebugLoc = ppDebugLoc' ppLabel

ppTypedValMd :: Fmt ValMd
ppTypedValMd  = ppTyped ppValMd . Typed (PrimType Metadata)

ppMetadata :: Fmt Doc
ppMetadata body = char '!' <> body

ppMetadataNode' :: Fmt i -> Fmt [Maybe (ValMd' i)]
ppMetadataNode' pp vs = ppMetadata (braces (commas (map arg vs)))
  where arg = maybe ("null") (ppValMd' pp)

ppMetadataNode :: Fmt [Maybe ValMd]
ppMetadataNode = ppMetadataNode' ppLabel

ppStringLiteral :: Fmt String
ppStringLiteral  = doubleQuotes . text . concatMap escape
  where
  escape c | c == '"' || c == '\\'  = '\\' : showHex (fromEnum c) ""
           | isAscii c && isPrint c = [c]
           | otherwise              = '\\' : pad (ord c)

  pad n | n < 0x10  = '0' : map toUpper (showHex n "")
        | otherwise =       map toUpper (showHex n "")

ppAsm :: Bool -> Bool -> String -> Fmt String
ppAsm s a i c =
  "asm" <+> sideeffect <+> alignstack
        <+> ppStringLiteral i <> comma <+> ppStringLiteral c
  where
  sideeffect | s         = "sideeffect"
             | otherwise = empty

  alignstack | a         = "alignstack"
             | otherwise = empty


ppConstExpr' :: Fmt i -> Fmt (ConstExpr' i)
ppConstExpr' pp expr =
  case expr of
    ConstGEP optflgs mrng ty ptr ixs  ->
      "getelementptr"
        <+> ppGepFlags optflgs
        <+> ppRange mrng
        <+> parens (commas (
                       let argIndices = 0 : [0..] -- rval, ptr, then ixs indices
                       in reverse  -- ppTyp's pushes entries to the listg head
                          $ foldl (ppTyp's mrng) [ppType ty]
                          $ zip argIndices (ptr:ixs)))
    ConstConv op tv t  ->
      let droppedIn18 = case op of
                          -- https://github.com/llvm/llvm-project commit e4a4122 dropped ZExt and SExt
                          ZExt _ -> True
                          SExt -> True
                          -- https://github.com/llvm/llvm-project commit 17764d2 dropped FpTrunc through SiToFP
                          FpTrunc -> True
                          FpExt -> True
                          FpToUi -> True
                          FpToSi -> True
                          UiToFp _ -> True
                          SiToFp -> True
                          _ -> False
          ppConstConv = ppConvOp op <+> parens (ppTyp' tv <+> "to" <+> ppType t)
      in if droppedIn18
         then droppedInLLVM 18 "fptrunc/fpext/fptoui/fptosi/uitofp/sitofp constexprs" ppConstConv
         else ppConstConv
    ConstSelect c l r  ->
      droppedInLLVM 17 "select constexpr" -- https://github.com/llvm/llvm-project commit bbfb13a

      $ "select" <+> parens (commas [ ppTyp' c, ppTyp' l , ppTyp' r])
    ConstBlockAddr t l -> "blockaddress" <+> parens (ppVal' (typedValue t) <> comma <+> pp l)
    ConstFCmp       op a b -> droppedInLLVM 19 "fcmp constexprs"
                              $ "fcmp" <+> ppFCmpOp op <+> ppTupleT a b
    ConstICmp       op a b -> droppedInLLVM 19 "icmp constexprs"
                              $ "icmp" <+> ppICmpOp op <+> ppTupleT a b
    ConstArith      op a b -> ppArithOp op <+> ppTuple a b
    ConstUnaryArith op a   -> ppUnaryArithOp op <+> ppTyp' a
    ConstBit        op@(Shl _ _) a b -> droppedInLLVM 19 "shl constexprs"
                                        $ ppBitOp op   <+> ppTuple a b
    ConstBit        Xor a b -> ppBitOp Xor <+> ppTuple a b
    ConstBit        op a b -> droppedInLLVM 18 "and/or/lshr/ashr constexprs"
                              $ ppBitOp op <+> ppTuple a b
  where ppTuple  a b = parens $ ppTyped ppVal' a <> comma <+> ppVal' b
        ppTupleT a b = parens $ ppTyped ppVal' a <> comma <+> ppTyp' b
        ppVal'       = ppValue' pp
        ppTyp'       = ppTyped ppVal'
        ppTyp's mrng a (i,t) =
          let inrangeMark = if Just (RangeIndex i) == mrng then "inrange" else empty
          in (inrangeMark <+> ppTyp' t) : a
        ppRange =
          let ppR = \case
                RangeIndex _i -> empty -- handled in ppTyp's
                Range _ l u ->
                  "inrange(" <> integral l <> ", " <> integral u <> ")"
          in maybe empty ppR

ppGepFlags :: Fmt [GEPAttr]
ppGepFlags s =
  let fltr = if GEP_Inbounds `elem` s
             then
               -- inbounds implies nusw, but LLVM stipulates that if
               -- inbounds is present, nusw is not also printed.
               filter (/= GEP_NUSW)
             else id
      ppF = \case
        GEP_Inbounds -> "inbounds"
        GEP_NUSW -> "nusw"
        GEP_NUW -> "nuw"
  in foldl (\o f -> o <+> ppF f) empty $ fltr $ nub s

ppConstExpr :: Fmt ConstExpr
ppConstExpr = ppConstExpr' ppLabel

-- DWARF Debug Info ------------------------------------------------------------

ppDebugInfo' :: Fmt i -> Fmt (DebugInfo' i)
ppDebugInfo' pp di = case di of
  DebugInfoBasicType bt         -> ppDIBasicType' pp bt
  DebugInfoCompileUnit cu       -> ppDICompileUnit' pp cu
  DebugInfoCompositeType ct     -> ppDICompositeType' pp ct
  DebugInfoDerivedType dt       -> ppDIDerivedType' pp dt
  DebugInfoEnumerator nm v u    -> ppDIEnumerator nm v u
  DebugInfoExpression e         -> ppDIExpression e
  DebugInfoFile f               -> ppDIFile f
  DebugInfoGlobalVariable gv    -> ppDIGlobalVariable' pp gv
  DebugInfoGlobalVariableExpression gv -> ppDIGlobalVariableExpression' pp gv
  DebugInfoLexicalBlock lb      -> ppDILexicalBlock' pp lb
  DebugInfoLexicalBlockFile lbf -> ppDILexicalBlockFile' pp lbf
  DebugInfoLocalVariable lv     -> ppDILocalVariable' pp lv
  DebugInfoSubprogram sp        -> ppDISubprogram' pp sp
  DebugInfoSubrange sr          -> ppDISubrange' pp sr
  DebugInfoSubroutineType st    -> ppDISubroutineType' pp st
  DebugInfoNameSpace ns         -> ppDINameSpace' pp ns
  DebugInfoTemplateTypeParameter dttp  -> ppDITemplateTypeParameter' pp dttp
  DebugInfoTemplateValueParameter dtvp -> ppDITemplateValueParameter' pp dtvp
  DebugInfoImportedEntity diip         -> ppDIImportedEntity' pp diip
  DebugInfoLabel dil            -> ppDILabel' pp dil
  DebugInfoArgList args         -> ppDIArgList' pp args
  DebugInfoAssignID             -> "!DIAssignID()"
  -- DebugRecordDeclare drd        -> ppDbgRecDeclare' pp drd

-- Prints DebugRecords (introduced in LLVM 19) which replace debug intrinsics and
-- unlike the intrinsics that follow the instruction, the debug records *precede*
-- the Instruction they affect.
ppDebugRecords :: [DebugRecord' BlockLabel] -> Fmt Doc
ppDebugRecords [] = id
ppDebugRecords drs = ((nest 2 $ vcat (ppDebugRecord' ppLabel <$> drs)) $$)

ppDebugRecord' :: Fmt lab -> Fmt (DebugRecord' lab)
ppDebugRecord' pl = \case
  DebugRecordValue drv -> ppDbgRecValue' pl drv
  DebugRecordDeclare drd -> ppDbgRecDeclare' pl drd
  DebugRecordAssign dra -> ppDbgRecAssign' pl dra
  DebugRecordValueSimple dvs -> ppDbgRecValueSimple' pl dvs
  DebugRecordLabel drl -> ppDbgRecLabel' pl drl

ppDbgRecValue' :: Fmt lab -> Fmt (DbgRecValue' lab)
ppDbgRecValue' pl dr =
  "#dbg_value"
  <> parens (commas [ ppValMd' pl $ drvValAsMetadata dr
                    , ppValMd' pl $ drvLocalVariable dr
                    , ppValMd' pl $ drvExpression dr
                    , ppValMd' pl $ drvLocation dr
                    ])

ppDbgRecDeclare' :: Fmt lab -> Fmt (DbgRecDeclare' lab)
ppDbgRecDeclare' pl dr =
  "#dbg_declare"
  <> parens (commas [ ppValMd' pl $ drdValAsMetadata dr
                    , ppValMd' pl $ drdLocalVariable dr
                    , ppValMd' pl $ drdExpression dr
                    , ppValMd' pl $ drdLocation dr
                    ])

ppDbgRecAssign' :: Fmt lab -> Fmt (DbgRecAssign' lab)
ppDbgRecAssign' pl dr =
  "#dbg_assign"
  <> parens (commas [ ppValMd' pl $ draValAsMetadata dr
                    , ppValMd' pl $ draLocalVariable dr
                    , ppValMd' pl $ draExpression dr
                    , ppValMd' pl $ draAssignID dr
                    , ppValMd' pl $ draValAsMetadataAddr dr
                    , ppValMd' pl $ draExpressionAddr dr
                    , ppValMd' pl $ draLocation dr
                    ])

ppDbgRecValueSimple' :: Fmt lab -> Fmt (DbgRecValueSimple' lab)
ppDbgRecValueSimple' pl dr =
  "#dbg_value"
  <> parens (commas [ ppTyped (ppValue' pl) $ drvsValue dr
                    , ppValMd' pl $ drvsLocalVariable dr
                    , ppValMd' pl $ drvsExpression dr
                    , ppValMd' pl $ drvsLocation dr
                    ])

ppDbgRecLabel' :: Fmt lab -> Fmt (DbgRecLabel' lab)
ppDbgRecLabel' pl dr =
  "#dbg_label"
  <> parens (commas [ ppValMd' pl $ drlLabel dr
                    , ppValMd' pl $ drlLocation dr
                    ])

ppDebugInfo :: Fmt DebugInfo
ppDebugInfo = ppDebugInfo' ppLabel

ppDIImportedEntity' :: Fmt i -> Fmt (DIImportedEntity' i)
ppDIImportedEntity' pp ie = "!DIImportedEntity"
  <> parens (mcommas [ pure ("tag:"    <+> integral (diieTag ie))
                     , (("scope:"  <+>) . ppValMd' pp) <$> diieScope ie
                     , (("entity:" <+>) . ppValMd' pp) <$> diieEntity ie
                     , (("file:"   <+>) . ppValMd' pp) <$> diieFile ie
                     , pure ("line:"   <+> integral (diieLine ie))
                     , (("name:"   <+>) . text)        <$> diieName ie
                     ])

ppDIImportedEntity :: Fmt DIImportedEntity
ppDIImportedEntity = ppDIImportedEntity' ppLabel

ppDILabel' :: Fmt i -> Fmt (DILabel' i)
ppDILabel' pp ie = "!DILabel"
  <> parens (mcommas $
       [ (("scope:"  <+>) . ppValMd' pp) <$> dilScope ie
       , pure ("name:" <+> ppStringLiteral (dilName ie))
       , (("file:"   <+>) . ppValMd' pp) <$> dilFile ie
       , pure ("line:"   <+> integral (dilLine ie))
       ] ++
       when' (llvmVer >= 21)
       [ pure ("column:" <+> integral (dilColumn ie))
       , pure ("isArtificial:" <+> ppBool (dilIsArtificial ie))
       , (("coroSuspendIdx:" <+>) . integral) <$> dilCoroSuspendIdx ie
       ])

ppDILabel :: Fmt DILabel
ppDILabel = ppDILabel' ppLabel

ppDINameSpace' :: Fmt i -> Fmt (DINameSpace' i)
ppDINameSpace' pp ns = "!DINameSpace"
  <> parens (mcommas [ ("name:"   <+>) . text <$> (dinsName ns)
                     , pure ("scope:"  <+> ppValMd' pp (dinsScope ns))
                     , pure ("file:"   <+> ppValMd' pp (dinsFile ns))
                     , pure ("line:"   <+> integral (dinsLine ns))
                     ])

ppDINameSpace :: Fmt DINameSpace
ppDINameSpace = ppDINameSpace' ppLabel

ppDITemplateTypeParameter' :: Fmt i -> Fmt (DITemplateTypeParameter' i)
ppDITemplateTypeParameter' pp tp = "!DITemplateTypeParameter"
  <> parens (mcommas [ ("name:"  <+>) . text        <$> dittpName tp
                     , ("type:"  <+>) . ppValMd' pp <$> dittpType tp
                     ])

ppDITemplateTypeParameter :: Fmt DITemplateTypeParameter
ppDITemplateTypeParameter = ppDITemplateTypeParameter' ppLabel

ppDITemplateValueParameter' :: Fmt i -> Fmt (DITemplateValueParameter' i)
ppDITemplateValueParameter' pp vp = "!DITemplateValueParameter"
  <> parens (mcommas [ pure ("tag:"   <+> integral (ditvpTag vp))
                     , ("name:"  <+>) . text        <$> ditvpName vp
                     , ("type:"  <+>) . ppValMd' pp <$> ditvpType vp
                     , pure ("value:" <+> ppValMd' pp (ditvpValue vp))
                     ])

ppDITemplateValueParameter :: Fmt DITemplateValueParameter
ppDITemplateValueParameter = ppDITemplateValueParameter' ppLabel

ppDIBasicType' :: Fmt i -> Fmt (DIBasicType' i)
ppDIBasicType' pp bt = "!DIBasicType"
  <> parens (mcommas
       [ pure ("tag:"      <+> integral (dibtTag bt))
       , pure ("name:"     <+> doubleQuotes (text (dibtName bt)))
       ,     (("size:"     <+>) . ppSizeOrOffsetValMd' pp) <$> dibtSize bt
       , pure ("align:"    <+> integral (dibtAlign bt))
       , pure ("encoding:" <+> integral (dibtEncoding bt))
       ,     (("flags:"    <+>) . integral)
             <$> dibtFlags bt
       , if dibtNumExtraInhabitants bt > 0
         then pure ("numExtraInhabitants:" <+> integral (dibtNumExtraInhabitants bt))
         else Nothing
       ])

ppDICompileUnit' :: Fmt i -> Fmt (DICompileUnit' i)
ppDICompileUnit' pp cu = "!DICompileUnit"
  <> parens (mcommas $
       [ pure ("language:"              <+> integral (dicuLanguage cu))
       ,     (("file:"                  <+>) . ppValMd' pp) <$> (dicuFile cu)
       ,     (("producer:"              <+>) . doubleQuotes . text)
             <$> (dicuProducer cu)
       , pure ("isOptimized:"           <+> ppBool (dicuIsOptimized cu))
       , pure ("flags:"                 <+> ppFlags (dicuFlags cu))
       , pure ("runtimeVersion:"        <+> integral (dicuRuntimeVersion cu))
       ,     (("splitDebugFilename:"    <+>) . doubleQuotes . text)
             <$> (dicuSplitDebugFilename cu)
       , pure ("emissionKind:"          <+> integral (dicuEmissionKind cu))
       ,     (("enums:"                 <+>) . ppValMd' pp) <$> (dicuEnums cu)
       ,     (("retainedTypes:"         <+>) . ppValMd' pp) <$> (dicuRetainedTypes cu)
       ,     (("subprograms:"           <+>) . ppValMd' pp) <$> (dicuSubprograms cu)
       ,     (("globals:"               <+>) . ppValMd' pp) <$> (dicuGlobals cu)
       ,     (("imports:"               <+>) . ppValMd' pp) <$> (dicuImports cu)
       ,     (("macros:"                <+>) . ppValMd' pp) <$> (dicuMacros cu)
       , pure ("dwoId:"                 <+> integral (dicuDWOId cu))
       , pure ("splitDebugInlining:"    <+> ppBool (dicuSplitDebugInlining cu))
       , pure ("debugInfoForProfiling:" <+> ppBool (dicuDebugInfoForProf cu))
       , pure ("nameTableKind:"         <+> integral (dicuNameTableKind cu))
       ]
       ++
       when' (llvmVer >= 11)
       [ pure ("rangesBaseAddress:"     <+> ppBool (dicuRangesBaseAddress cu))
       ,     (("sysroot:"               <+>) . doubleQuotes . text)
             <$> (dicuSysRoot cu)
       ,     (("sdk:"                   <+>) . doubleQuotes . text)
             <$> (dicuSDK cu)
       ]
       )


ppDICompileUnit :: Fmt DICompileUnit
ppDICompileUnit = ppDICompileUnit' ppLabel

ppFlags :: Fmt (Maybe String)
ppFlags mb = doubleQuotes (maybe empty text mb)

ppDICompositeType' :: Fmt i -> Fmt (DICompositeType' i)
ppDICompositeType' pp ct = "!DICompositeType"
  <> parens (mcommas
       [ pure ("tag:"            <+> integral (dictTag ct))
       ,     (("name:"           <+>) . doubleQuotes . text) <$> (dictName ct)
       ,     (("file:"           <+>) . ppValMd' pp) <$> (dictFile ct)
       , pure ("line:"           <+> integral (dictLine ct))
       ,     (("baseType:"       <+>) . ppValMd' pp) <$> (dictBaseType ct)
       ,     (("size:"           <+>) . ppSizeOrOffsetValMd' pp) <$> dictSize ct
       , pure ("align:"          <+> integral (dictAlign ct))
       ,     (("offset:"         <+>) . ppSizeOrOffsetValMd' pp) <$> dictOffset ct
       , pure ("flags:"          <+> integral (dictFlags ct))
       ,     (("elements:"       <+>) . ppValMd' pp) <$> (dictElements ct)
       , pure ("runtimeLang:"    <+> integral (dictRuntimeLang ct))
       ,     (("vtableHolder:"   <+>) . ppValMd' pp) <$> (dictVTableHolder ct)
       ,     (("templateParams:" <+>) . ppValMd' pp) <$> (dictTemplateParams ct)
       ,     (("identifier:"     <+>) . doubleQuotes . text)
             <$> (dictIdentifier ct)
       ,     (("discriminator:"  <+>) . ppValMd' pp) <$> (dictDiscriminator ct)
       ,     (("associated:"     <+>) . ppValMd' pp) <$> (dictAssociated ct)
       ,     (("allocated:"      <+>) . ppValMd' pp) <$> (dictAllocated ct)
       ,     (("rank:"           <+>) . ppValMd' pp) <$> (dictRank ct)
       ,     (("annotations:"    <+>) . ppValMd' pp) <$> (dictAnnotations ct)
       , if dictNumExtraInhabitants ct > 0
         then pure ("numExtraInhabitants:" <+> integral (dictNumExtraInhabitants ct))
         else Nothing
       ,     (("specification:"  <+>) . ppValMd' pp) <$> (dictSpecification ct)
       ,     (("enumKind:"       <+>) . integral) <$> (dictEnumKind ct)
       ,     (("bitStride:"      <+>) . ppValMd' pp) <$> (dictBitStride ct)
       ])

ppDICompositeType :: Fmt DICompositeType
ppDICompositeType = ppDICompositeType' ppLabel

ppDIDerivedType' :: Fmt i -> Fmt (DIDerivedType' i)
ppDIDerivedType' pp dt = "!DIDerivedType"
  <> parens (mcommas
       [ pure ("tag:"       <+> integral (didtTag dt))
       ,     (("name:"      <+>) . doubleQuotes . text) <$> (didtName dt)
       ,     (("file:"      <+>) . ppValMd' pp) <$> (didtFile dt)
       , pure ("line:"      <+> integral (didtLine dt))
       ,     (("scope:"     <+>) . ppValMd' pp) <$> (didtScope dt)
       ,      ("baseType:"  <+>) <$> (ppValMd' pp <$> didtBaseType dt <|> Just "null")
       ,     (("size:"      <+>) . ppSizeOrOffsetValMd' pp) <$> didtSize dt
       , pure ("align:"     <+> integral (didtAlign dt))
       ,     (("offset:"    <+>) . ppSizeOrOffsetValMd' pp) <$> didtOffset dt
       , pure ("flags:"     <+> integral (didtFlags dt))
       ,     (("extraData:" <+>) . ppValMd' pp) <$> (didtExtraData dt)
       ,     (("dwarfAddressSpace:" <+>) . integral) <$> didtDwarfAddressSpace dt
       ,     (("annotations:" <+>) . ppValMd' pp) <$> (didtAnnotations dt)
       ])

ppDIDerivedType :: Fmt DIDerivedType
ppDIDerivedType = ppDIDerivedType' ppLabel

ppDIEnumerator :: String -> Integer -> Fmt Bool
ppDIEnumerator n v u = "!DIEnumerator"
  <> parens (commas [ "name:"  <+> doubleQuotes (text n)
                    , "value:" <+> integral v
                    , "isUnsigned:" <+> ppBool u
                    ])

ppDIExpression :: Fmt DIExpression
ppDIExpression e = "!DIExpression"
  <> parens (commas (map integral (dieElements e)))

ppDIFile :: Fmt DIFile
ppDIFile f = "!DIFile"
  <> parens (commas [ "filename:"  <+> doubleQuotes (text (difFilename f))
                    , "directory:" <+> doubleQuotes (text (difDirectory f))
                    ])

ppDIGlobalVariable' :: Fmt i -> Fmt (DIGlobalVariable' i)
ppDIGlobalVariable' pp gv = "!DIGlobalVariable"
  <> parens (mcommas
       [      (("scope:"       <+>) . ppValMd' pp) <$> (digvScope gv)
       ,      (("name:"        <+>) . doubleQuotes . text) <$> (digvName gv)
       ,      (("linkageName:" <+>) . doubleQuotes . text)
              <$> (digvLinkageName gv)
       ,      (("file:"        <+>) . ppValMd' pp) <$> (digvFile gv)
       , pure ("line:"         <+> integral (digvLine gv))
       ,      (("type:"        <+>) . ppValMd' pp) <$> (digvType gv)
       , pure ("isLocal:"      <+> ppBool (digvIsLocal gv))
       , pure ("isDefinition:" <+> ppBool (digvIsDefinition gv))
       ,      (("variable:"    <+>) . ppValMd' pp) <$> (digvVariable gv)
       ,      (("declaration:" <+>) . ppValMd' pp) <$> (digvDeclaration gv)
       ,      (("align:"       <+>) . integral) <$> digvAlignment gv
       ,      (("annotations:" <+>) . ppValMd' pp) <$> (digvAnnotations gv)
       ])

ppDIGlobalVariable :: Fmt DIGlobalVariable
ppDIGlobalVariable = ppDIGlobalVariable' ppLabel

ppDIGlobalVariableExpression' :: Fmt i -> Fmt (DIGlobalVariableExpression' i)
ppDIGlobalVariableExpression' pp gve = "!DIGlobalVariableExpression"
  <> parens (mcommas
       [      (("var:"  <+>) . ppValMd' pp) <$> (digveVariable gve)
       ,      (("expr:" <+>) . ppValMd' pp) <$> (digveExpression gve)
       ])

ppDIGlobalVariableExpression :: Fmt DIGlobalVariableExpression
ppDIGlobalVariableExpression = ppDIGlobalVariableExpression' ppLabel

ppDILexicalBlock' :: Fmt i -> Fmt (DILexicalBlock' i)
ppDILexicalBlock' pp ct = "!DILexicalBlock"
  <> parens (mcommas
       [     (("scope:"  <+>) . ppValMd' pp) <$> (dilbScope ct)
       ,     (("file:"   <+>) . ppValMd' pp) <$> (dilbFile ct)
       , pure ("line:"   <+> integral (dilbLine ct))
       , pure ("column:" <+> integral (dilbColumn ct))
       ])

ppDILexicalBlock :: Fmt DILexicalBlock
ppDILexicalBlock = ppDILexicalBlock' ppLabel

ppDILexicalBlockFile' :: Fmt i -> Fmt (DILexicalBlockFile' i)
ppDILexicalBlockFile' pp lbf = "!DILexicalBlockFile"
  <> parens (mcommas
       [ pure ("scope:"         <+> ppValMd' pp (dilbfScope lbf))
       ,     (("file:"          <+>) . ppValMd' pp) <$> (dilbfFile lbf)
       , pure ("discriminator:" <+> integral (dilbfDiscriminator lbf))
       ])

ppDILexicalBlockFile :: Fmt DILexicalBlockFile
ppDILexicalBlockFile = ppDILexicalBlockFile' ppLabel

ppDILocalVariable' :: Fmt i -> Fmt (DILocalVariable' i)
ppDILocalVariable' pp lv = "!DILocalVariable"
  <> parens (mcommas
       [      (("scope:" <+>) . ppValMd' pp) <$> (dilvScope lv)
       ,      (("name:"  <+>) . doubleQuotes . text) <$> (dilvName lv)
       ,      (("file:"  <+>) . ppValMd' pp) <$> (dilvFile lv)
       , pure ("line:"   <+> integral (dilvLine lv))
       ,      (("type:"  <+>) . ppValMd' pp) <$> (dilvType lv)
       , pure ("arg:"    <+> integral (dilvArg lv))
       , pure ("flags:"  <+> integral (dilvFlags lv))
       ,      (("align:" <+>) . integral) <$> dilvAlignment lv
       ,      (("annotations:" <+>) . ppValMd' pp) <$> (dilvAnnotations lv)
       ])

ppDILocalVariable :: Fmt DILocalVariable
ppDILocalVariable = ppDILocalVariable' ppLabel

-- | See @writeDISubprogram@ in the LLVM source, in the file @AsmWriter.cpp@
--
-- Note that the textual syntax changed in LLVM 7, as the @retainedNodes@ field
-- was called @variables@ in previous LLVM versions.
ppDISubprogram' :: Fmt i -> Fmt (DISubprogram' i)
ppDISubprogram' pp sp = "!DISubprogram"
  <> parens (mcommas
       [      (("scope:"          <+>) . ppValMd' pp) <$> (dispScope sp)
       ,      (("name:"           <+>) . doubleQuotes . text) <$> (dispName sp)
       ,      (("linkageName:"    <+>) . doubleQuotes . text)
              <$> (dispLinkageName sp)
       ,      (("file:"           <+>) . ppValMd' pp) <$> (dispFile sp)
       , pure ("line:"            <+> integral (dispLine sp))
       ,      (("type:"           <+>) . ppValMd' pp) <$> (dispType sp)
       , pure ("isLocal:"         <+> ppBool (dispIsLocal sp))
       , pure ("isDefinition:"    <+> ppBool (dispIsDefinition sp))
       , pure ("scopeLine:"       <+> integral (dispScopeLine sp))
       ,      (("containingType:" <+>) . ppValMd' pp) <$> (dispContainingType sp)
       , pure ("virtuality:"      <+> integral (dispVirtuality sp))
       , pure ("virtualIndex:"    <+> integral (dispVirtualIndex sp))
       , pure ("flags:"           <+> integral (dispFlags sp))
       , pure ("isOptimized:"     <+> ppBool (dispIsOptimized sp))
       ,      (("unit:"           <+>) . ppValMd' pp) <$> (dispUnit sp)
       ,      (("templateParams:" <+>) . ppValMd' pp) <$> (dispTemplateParams sp)
       ,      (("declaration:"    <+>) . ppValMd' pp) <$> (dispDeclaration sp)
       ,      (("retainedNodes:"  <+>) . ppValMd' pp) <$> (dispRetainedNodes sp)
       ,      (("thrownTypes:"    <+>) . ppValMd' pp) <$> (dispThrownTypes sp)
       ,      (("annotations:"    <+>) . ppValMd' pp) <$> (dispAnnotations sp)
       ])

ppDISubprogram :: Fmt DISubprogram
ppDISubprogram = ppDISubprogram' ppLabel

ppDISubrange' :: Fmt i -> Fmt (DISubrange' i)
ppDISubrange' pp sr =
  "!DISubrange"
  <> parens
  (mcommas
   -- LLVM < 7: count and lowerBound as signed int 64
   -- LLVM < 11: count as ValMd, lowerBound as signed in 64
   -- LLVM >= 11: ValMd of count, lowerBound, upperBound, and stride
   -- Valid through LLVM 17.
   -- See AST.hs description for more details on the structure.
   -- See https://github.com/llvm/llvm-project/blob/431969e/llvm/lib/IR/AsmWriter.cpp#L1888-L1927
   -- for more details on output generation.
   [
     (("count:" <+>) . ppInt64ValMd' (llvmVer >= 7) pp) <$> disrCount sr
   , (("lowerBound:" <+>) . ppInt64ValMd' (llvmVer >= 11) pp) <$> disrLowerBound sr
   , when' (llvmVer >= 11)
     $ (("upperBound:" <+>) . ppInt64ValMd' True pp) <$> disrUpperBound sr
   , when' (llvmVer >= 11)
     $ (("stride:" <+>) . ppInt64ValMd' True pp) <$> disrStride sr
   ])

ppDISubrange :: Fmt DISubrange
ppDISubrange = ppDISubrange' ppLabel

ppDISubroutineType' :: Fmt i -> Fmt (DISubroutineType' i)
ppDISubroutineType' pp st = "!DISubroutineType"
  <> parens (commas
       [ "flags:" <+> integral (distFlags st)
       , "types:" <+> fromMaybe "null" (ppValMd' pp <$> (distTypeArray st))
       ])

ppDISubroutineType :: Fmt DISubroutineType
ppDISubroutineType = ppDISubroutineType' ppLabel

ppDIArgList' :: Fmt i -> Fmt (DIArgList' i)
ppDIArgList' pp args = "!DIArgList"
  <> parens (commas (map (ppValMd' pp) (dialArgs args)))

ppDIArgList :: Fmt DIArgList
ppDIArgList = ppDIArgList' ppLabel

-- Utilities -------------------------------------------------------------------

ppBool :: Fmt Bool
ppBool b | b         = "true"
         | otherwise = "false"

-- | Build a variable-argument argument list.
ppArgList :: Bool -> Fmt [Doc]
ppArgList True  ds = parens (commas (ds ++ ["..."]))
ppArgList False ds = parens (commas ds)

integral :: Integral i => Fmt i
integral  = integer . fromIntegral

hex :: (Integral i, Show i) => Fmt i
hex i = text (showHex i "0x")

opt :: Bool -> Fmt Doc
opt True  = id
opt False = const empty

-- | Print a ValMd' value as a plain signed integer (Int64) if possible.  If the
-- ValMd' is not representable as an Int64, defer to ValMd' printing (if
-- canFallBack is True) or print nothing (for when a ValMd is not a valid
-- representation).

ppInt64ValMd' :: Bool -> Fmt i -> Fmt (ValMd' i)
ppInt64ValMd' canFallBack pp = go
  where go = \case
          ValMdValue tv
            | PrimType (Integer _) <- typedType tv
            , ValInteger i <- typedValue tv
              -> integer i  -- 64 bits is the largest Int, so no conversion needed
          o@(ValMdDebugInfo (DebugInfoGlobalVariable gv)) ->
            case digvVariable gv of
              Nothing -> when' canFallBack $ ppValMd' pp o
              Just v -> go v
          o@(ValMdDebugInfo (DebugInfoGlobalVariableExpression expr)) ->
            case digveExpression expr of
              Nothing -> when' canFallBack $ ppValMd' pp o
              Just e -> go e
          ValMdDebugInfo (DebugInfoLocalVariable lv) ->
            integer $ fromIntegral $ dilvArg lv  -- ??
          -- ValMdRef _idx -> mempty -- no table here to look this up...
          o -> when' canFallBack $ ppValMd' pp o

-- | Print the size or offset of a type-related metadata node. This value can
-- either be an integer literal (in which case the bare literal is printed), or
-- in LLVM 21 or later, it can be a more complicated metadata expression (in
-- which case the metadata is pretty-printed).
ppSizeOrOffsetValMd' :: Fmt i -> Fmt (ValMd' i)
ppSizeOrOffsetValMd' pp = \case
  ValMdValue tv
    | ValInteger i <- typedValue tv
      -> integer i
  o -> when' (llvmVer >= 21) $ ppValMd' pp o


commas :: Fmt [Doc]
commas  = fsep . punctuate comma

-- | Helpful for all of the optional fields that appear in the
-- metadata values
mcommas :: Fmt [Maybe Doc]
mcommas = commas . catMaybes

angles :: Fmt Doc
angles d = char '<' <> d <> char '>'

structBraces :: Fmt Doc
structBraces body = char '{' <+> body <+> char '}'

ppMaybe :: Fmt a -> Fmt (Maybe a)
ppMaybe  = maybe empty

-- | Throw an error if the @?config@ version is older than the given version. The
-- String indicates which constructor is unavailable in the error message.
onlyOnLLVM :: (?config :: Config) => LLVMVer -> String -> a -> a
onlyOnLLVM fromVer name
  | llvmVer >= fromVer = id
  | otherwise          = error $ name ++ " is supported only on LLVM >= "
                                 ++ llvmVerToString fromVer

-- | Throw an error if the @?config@ version is older than the given version. The
-- String indicates which constructor is unavailable in the error message.
droppedInLLVM :: (?config :: Config) => LLVMVer -> String -> a -> a
droppedInLLVM fromVer name
  | llvmVer >= fromVer = error $ name ++ " is supported only up to LLVM >= "
                                 ++ llvmVerToString fromVer
  | otherwise          = id
