{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module LLVM.AST.ToLLVMPretty (
  llvmverFromTuple,
  toLLVMPretty,
) where

import qualified LLVM.AST as A
import qualified LLVM.AST.AddrSpace as A
import qualified LLVM.AST.CallingConvention as A
import qualified LLVM.AST.Constant as AC
import qualified LLVM.AST.Float as AF
import qualified LLVM.AST.FunctionAttribute as AFA
import qualified LLVM.AST.Global as AG
import qualified LLVM.AST.IntegerPredicate as AIP
import qualified LLVM.AST.Linkage as A
import qualified LLVM.AST.ParameterAttribute as A
import qualified LLVM.AST.Visibility as A
import qualified Text.LLVM as P
import qualified Text.LLVM.PP as P
import qualified Text.LLVM.Triple.Parse as P

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Bifunctor (first, second)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Char8 as SBS8
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import GHC.Generics (Generic, Generically(..))


llvmverFromTuple :: NonEmpty Int -> Maybe P.LLVMVer
llvmverFromTuple (3 NE.:| 5 : _) = Just P.llvmV3_5
llvmverFromTuple (3 NE.:| 6 : _) = Just P.llvmV3_6
llvmverFromTuple (3 NE.:| 7 : _) = Just P.llvmV3_7
llvmverFromTuple (3 NE.:| 8 : _) = Just P.llvmV3_8
llvmverFromTuple (n NE.:| _) | n >= 4, n <= P.llvmVlatest = Just n
llvmverFromTuple _ = Nothing

newtype M a = M (StateT (Map Word P.Ident, Map Word P.Symbol) (Except String) a)
  deriving newtype (Functor, Applicative, Monad, MonadState (Map Word P.Ident, Map Word P.Symbol), MonadError String)

runM :: M a -> Either String a
runM (M act) = runExcept (evalStateT act mempty)

mkNameI :: Word -> M P.Ident
mkNameI n = do
  mp <- gets fst
  case Map.lookup n mp of
    Just name -> return name
    Nothing -> do
      -- Let's hope this doesn't clash with anything...
      let name = P.Ident $ "tollpr_i_" ++ show (Map.size mp)
      modify (first (Map.insert n name))
      return name

mkNameS :: Word -> M P.Symbol
mkNameS n = do
  mp <- gets snd
  case Map.lookup n mp of
    Just name -> return name
    Nothing -> do
      -- Let's hope this doesn't clash with anything...
      let name = P.Symbol $ "tollpr_s_" ++ show (Map.size mp)
      modify (second (Map.insert n name))
      return name

-- Definitions in llvm-pretty's world.
-- TODO: support more of the module components
data PDefs = PDefs
  { pdDefines :: [P.Define]
  , pdDeclares :: [P.Declare]
  , pdGlobals :: [P.Global] }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically PDefs)

toLLVMPretty :: A.Module -> Either String P.Module
toLLVMPretty m = do
  pdefs <- runM $ mconcat <$> mapM toPDef (A.moduleDefinitions m)
  return $ P.Module
    { P.modSourceName =
        if SBS.length (A.moduleSourceFileName m) > 0
          then Just (SBS8.unpack (A.moduleSourceFileName m))
          else Nothing
    , P.modTriple =
        case A.moduleTargetTriple m of
          Just s -> P.parseTriple (SBS8.unpack s)
          Nothing -> error "toLLVMPretty: no module target triple"
    , P.modDataLayout =
        case A.moduleDataLayout m of
          Nothing -> []
          Just _ -> error "toLLVMPretty: TODO data layout"
    , P.modTypes = []
    , P.modNamedMd = []
    , P.modUnnamedMd = []
    , P.modComdat = mempty
    , P.modGlobals = pdGlobals pdefs
    , P.modDeclares = pdDeclares pdefs
    , P.modDefines = pdDefines pdefs
    , P.modInlineAsm = []
    , P.modAliases = []
    }

toPDef :: A.Definition -> M PDefs
toPDef (A.GlobalDefinition A.Function
    { AG.linkage
    , AG.visibility
    , AG.dllStorageClass = Nothing
    , AG.callingConvention = A.C
    , AG.returnAttributes = []
    , AG.returnType
    , AG.name = A.Name name
    , AG.parameters = (arguments, varargs)
    , AG.functionAttributes
    , AG.section
    , AG.comdat
    , AG.alignment = 0
    , AG.garbageCollectorName
    , AG.prefix = Nothing
    , AG.basicBlocks = basicBlocks@(_:_)  -- if there are basic blocks, it's a definition
    , AG.personalityFunction = Nothing
    , AG.metadata
    }) = do
  defAttrs <- mapM cvtFunAttr functionAttributes
  defMetadata <- case metadata of
                   [] -> return mempty
                   _ -> throwError "TODO metadata"
  defBody <- mapM cvtBB basicBlocks
  defArgs <- mapM cvtParameter arguments
  return $ mempty { pdDefines = [P.Define
    { P.defLinkage = Just (cvtLinkage linkage)
    , P.defVisibility = Just (cvtVisibility visibility)
    , P.defRetType = cvtType returnType
    , P.defName = P.Symbol (SBS8.unpack name)
    , P.defArgs = defArgs
    , P.defVarArgs = varargs
    , P.defAttrs = defAttrs
    , P.defSection = SBS8.unpack <$> section
    , P.defGC = (P.GC . SBS8.unpack) <$> garbageCollectorName
    , P.defBody = defBody
    , P.defMetadata = defMetadata
    , P.defComdat = SBS8.unpack <$> comdat
    }] }

toPDef (A.GlobalDefinition A.Function
    { AG.linkage
    , AG.visibility
    , AG.dllStorageClass = Nothing
    , AG.callingConvention = A.C
    , AG.returnAttributes = []
    , AG.returnType
    , AG.name = A.Name name
    , AG.parameters = (arguments, varargs)
    , AG.functionAttributes
    , AG.section = Nothing
    , AG.comdat
    , AG.alignment = 0
    , AG.garbageCollectorName = Nothing
    , AG.prefix = Nothing
    , AG.basicBlocks = []  -- if there are NO basic blocks, it's a declaration
    , AG.personalityFunction = Nothing
    , AG.metadata = []
    }) = do
  defAttrs <- mapM cvtFunAttr functionAttributes
  return $ mempty { pdDeclares = [P.Declare
    { P.decLinkage = Just (cvtLinkage linkage)
    , P.decVisibility = Just (cvtVisibility visibility)
    , P.decRetType = cvtType returnType
    , P.decName = P.Symbol (SBS8.unpack name)
    , P.decArgs = [cvtType t | A.Parameter t _ _ <- arguments]
    , P.decVarArgs = varargs
    , P.decAttrs = defAttrs
    , P.decComdat = SBS8.unpack <$> comdat
    }] }

toPDef (A.GlobalDefinition A.GlobalVariable
    { AG.name
    , AG.linkage
    , AG.visibility
    , AG.dllStorageClass = Nothing
    , AG.threadLocalMode = Nothing
    , AG.unnamedAddr = _  -- TODO: attribute not represented by llvm-pretty, but not terribly important?
    , AG.isConstant
    , AG.type'
    , AG.addrSpace = A.AddrSpace 0
    , AG.initializer
    , AG.section = Nothing
    , AG.comdat = Nothing
    , AG.alignment
    , AG.metadata
    }) = do
  name' <- cvtNameS name
  globalValue <- traverse uConst initializer
  globalMetadata <- case metadata of
                      [] -> return mempty
                      _ -> throwError "TODO metadata"
  return $ mempty { pdGlobals = [P.Global
    { P.globalSym = name'
    , P.globalAttrs = P.GlobalAttrs
        { P.gaLinkage = Just (cvtLinkage linkage)
        , P.gaVisibility = Just (cvtVisibility visibility)
        , P.gaConstant = isConstant }
    , P.globalType = cvtType type'
    , P.globalValue = globalValue
    , P.globalAlign = cvtAlignment alignment
    , P.globalMetadata = globalMetadata
    }] }

toPDef def = error $ "toLLVMPretty: Unsupported definition in module: " ++ show def

cvtLinkage :: A.Linkage -> P.Linkage
cvtLinkage A.Private = P.Private
cvtLinkage A.Internal = P.Internal
cvtLinkage A.AvailableExternally = P.AvailableExternally
cvtLinkage A.LinkOnce = P.Linkonce
cvtLinkage A.Weak = P.Weak
cvtLinkage A.Common = P.Common
cvtLinkage A.Appending = P.Appending
cvtLinkage A.ExternWeak = P.ExternWeak
cvtLinkage A.LinkOnceODR = P.LinkonceODR
cvtLinkage A.WeakODR = P.WeakODR
cvtLinkage A.External = P.External

cvtVisibility :: A.Visibility -> P.Visibility
cvtVisibility A.Default = P.DefaultVisibility
cvtVisibility A.Hidden = P.HiddenVisibility
cvtVisibility A.Protected = P.ProtectedVisibility

cvtType :: A.Type -> P.Type
cvtType A.VoidType = P.PrimType P.Void
cvtType (A.IntegerType b) = P.PrimType (P.Integer b)
cvtType (A.PointerType (A.AddrSpace 0)) = P.PtrOpaque
cvtType (A.PointerType (A.AddrSpace _)) = error "address spaces?"
cvtType (A.FloatingPointType t) = P.PrimType (P.FloatType (cvtFloatingType t))
cvtType (A.FunctionType r as var) = P.FunTy (cvtType r) (map cvtType as) var
cvtType (A.VectorType n t) = P.Vector (fromIntegral @Word32 @Word64 n) (cvtType t)
cvtType (A.StructureType False ts) = P.Struct (map cvtType ts)
cvtType (A.StructureType True ts) = P.PackedStruct (map cvtType ts)
cvtType (A.ArrayType n t) = P.Array n (cvtType t)
cvtType (A.NamedTypeReference _) = error "TODO NamedTypeReference"
cvtType A.MetadataType = error "TODO MetadataType"
cvtType A.LabelType = error "TODO LabelType"
cvtType A.TokenType = error "TODO TokenType"

cvtFloatingType :: A.FloatingPointType -> P.FloatType
cvtFloatingType A.HalfFP = P.Half
cvtFloatingType A.FloatFP = P.Float
cvtFloatingType A.DoubleFP = P.Double
cvtFloatingType A.FP128FP = P.Fp128
cvtFloatingType A.X86_FP80FP = P.X86_fp80
cvtFloatingType A.PPC_FP128FP = P.PPC_fp128

cvtParameter :: AG.Parameter -> M (P.Typed P.Ident)
cvtParameter (AG.Parameter t name _attribs) = do
  name' <- cvtName name
  -- TODO attributes! llvm-pretty doesn't seem to support them, but we put
  -- [NoAlias, NoCapture] on pointer types.
  -- TODO: Should check if these parameters are necessary (by benchmarking the old backend with llvm-hs), and if so, should send a PR to llvm-prettyt
  return $ P.Typed (cvtType t) name'

cvtBB :: AG.BasicBlock -> M P.BasicBlock
cvtBB (A.BasicBlock name inss term) =
  P.BasicBlock
    <$> (Just . P.Named <$> cvtName name)
    <*> ((++) <$> mapM (cvtNamed cvtIns) inss <*> fmap pure (cvtNamed cvtTerm term))

cvtName :: A.Name -> M P.Ident
cvtName (A.Name name) = return (P.Ident (SBS8.unpack name))
cvtName (A.UnName n) = mkNameI n

cvtNameS :: A.Name -> M P.Symbol
cvtNameS (A.Name name) = return (P.Symbol (SBS8.unpack name))
cvtNameS (A.UnName n) = mkNameS n

cvtNamed :: (a -> M (P.Instr, [(String, P.ValMd)]))
         -> A.Named a -> M P.Stmt
cvtNamed f (name A.:= ins) = do
  (ins', md) <- f ins
  name' <- cvtName name
  return $ P.Result name' ins' md
cvtNamed f (A.Do ins) = do
  (ins', md) <- f ins
  return $ P.Effect ins' md

cvtIns :: A.Instruction -> M (P.Instr, [(String, P.ValMd)])
cvtIns = \case
  -- TODO: FastMathFlags! We use them, but llvm-pretty doesn't support them.
  A.FNeg _fmf o0 md -> md <! P.UnaryArith P.FNeg <$> tOp o0
  A.Add nsw nuw o0 o1 md -> md <! P.Arith (P.Add nuw nsw) <$> tOp o0 <*> uOp o1
  A.FAdd _fmf o0 o1 md -> md <! P.Arith P.FAdd <$> tOp o0 <*> uOp o1
  A.Sub nsw nuw o0 o1 md -> md <! P.Arith (P.Sub nuw nsw) <$> tOp o0 <*> uOp o1
  A.FSub _fmf o0 o1 md -> md <! P.Arith P.FSub <$> tOp o0 <*> uOp o1
  A.Mul nsw nuw o0 o1 md -> md <! P.Arith (P.Sub nuw nsw) <$> tOp o0 <*> uOp o1
  A.FMul _fmf o0 o1 md -> md <! P.Arith P.FMul <$> tOp o0 <*> uOp o1
  A.UDiv exact o0 o1 md -> md <! P.Arith (P.UDiv exact) <$> tOp o0 <*> uOp o1
  A.SDiv exact o0 o1 md -> md <! P.Arith (P.SDiv exact) <$> tOp o0 <*> uOp o1
  A.FDiv _fmf o0 o1 md -> md <! P.Arith P.FDiv <$> tOp o0 <*> uOp o1
  A.URem o0 o1 md -> md <! P.Arith P.URem <$> tOp o0 <*> uOp o1
  A.SRem o0 o1 md -> md <! P.Arith P.SRem <$> tOp o0 <*> uOp o1
  -- A.FRem _fmf o0 o1 md -> undefined
  -- A.Shl nsw nuw o0 o1 md -> undefined
  -- A.LShr exact o0 o1 md -> undefined
  -- A.AShr exact o0 o1 md -> undefined
  -- A.And o0 o1 md -> undefined
  -- A.Or o0 o1 md -> undefined
  -- A.Xor o0 o1 md -> undefined
  -- A.Alloca allocatedType numElements alignment md -> undefined
  A.Load False t addr matom align md -> md <! P.Load (cvtType t) <$> tOp addr <*> cvtAtomicity matom <*> pure (cvtAlignment align)
  A.Store False addr value matom align md -> md <! P.Store <$> tOp value <*> tOp addr <*> cvtAtomicity matom <*> pure (cvtAlignment align)
  A.GetElementPtr inbounds t addr indices md -> md <! P.GEP inbounds (cvtType t) <$> tOp addr <*> mapM tOp indices
  -- A.Fence atomicity md -> undefined
  -- A.CmpXchg volatile address expected replacement alignment atomicity failureMemoryOrdering md -> undefined
  -- A.AtomicRMW volatile rmwOperation address value alignment atomicity md -> undefined
  -- A.Trunc o0 type' md -> undefined
  -- A.ZExt o0 type' md -> undefined
  -- A.SExt o0 type' md -> undefined
  -- A.FPToUI o0 type' md -> undefined
  -- A.FPToSI o0 type' md -> undefined
  -- A.UIToFP o0 type' md -> undefined
  -- A.SIToFP o0 type' md -> undefined
  -- A.FPTrunc o0 type' md -> undefined
  -- A.FPExt o0 type' md -> undefined
  -- A.PtrToInt o0 type' md -> undefined
  -- A.IntToPtr o0 type' md -> undefined
  -- A.BitCast o0 type' md -> undefined
  -- A.AddrSpaceCast o0 type' md -> undefined
  A.ICmp ipred o0 o1 md -> md <! P.ICmp (cvtIPred ipred) <$> tOp o0 <*> uOp o1
  -- A.FCmp fpred o0 o1 md -> undefined
  A.Phi t ls md -> md <! P.Phi (cvtType t) <$> mapM (\(op, name) -> (,) <$> uOp op <*> (P.Named <$> cvtName name)) ls
  -- A.Freeze o0 type' md -> undefined
  -- A.Select condition' trueValue falseValue md -> undefined
  A.Call tailCallKind A.C [] t (Right o0) arguments _fattrs md ->
    -- TODO: function attributes!
    md <! P.Call (cvtTail tailCallKind) (cvtType t) <$> uOp o0 <*> mapM cvtArg arguments
  ins@A.Call{} -> throwError $ "Unsupported Call: " ++ show ins
  -- A.VAArg argList type' md -> undefined
  -- A.ExtractElement vector index md -> undefined
  -- A.InsertElement vector element index md -> undefined
  -- A.ShuffleVector o0 o1 mask md -> undefined
  -- A.ExtractValue aggregate indices' md -> undefined
  -- A.InsertValue aggregate element indices' md -> undefined
  -- A.LandingPad type' cleanup clauses md -> undefined
  -- A.CatchPad catchSwitch args md -> undefined
  -- A.CleanupPad parentPad args md -> undefined
  ins -> throwError $ "TODO instruction: " ++ show ins

cvtTerm :: A.Terminator -> M (P.Instr, [(String, P.ValMd)])
cvtTerm = \case
  A.Ret Nothing md -> md <! pure P.RetVoid
  A.Ret (Just o0) md -> md <! P.Ret <$> tOp o0
  A.CondBr cond l1 l2 md -> md <! P.Br <$> tOp cond <*> (P.Named <$> cvtName l1) <*> (P.Named <$> cvtName l2)
  A.Br dest md -> md <! P.Jump . P.Named <$> cvtName dest
  -- A.Switch o0 defaultDest dests md -> undefined
  -- A.IndirectBr o0 possibleDests md -> undefined
  -- A.Invoke callingConvention' returnAttributes' type'' function' arguments' functionAttributes' returnDest exceptionDest md -> undefined
  -- A.Resume o0 md -> undefined
  -- A.Unreachable md -> undefined
  -- A.CleanupRet cleanupPad unwindDest md -> undefined
  -- A.CatchRet catchPad successor md -> undefined
  -- A.CatchSwitch parentPad' catchHandlers defaultUnwindDest md -> undefined
  term -> throwError $ "TODO terminator: " ++ show term

(<!) :: A.InstructionMetadata -> M a -> M (a, [(String, P.ValMd)])
[] <! x = (,[]) <$> x
md <! _ = throwError $ "TODO instruction metadata: " ++ show md
infix 3 <!

tOp :: A.Operand -> M (P.Typed P.Value)
tOp (A.LocalReference t name) = P.Typed (cvtType t) <$> (P.ValIdent <$> cvtName name)
tOp (A.ConstantOperand c) = tConst c
tOp (A.MetadataOperand _) = throwError "TODO metadata operands"

uOp :: A.Operand -> M P.Value
uOp (A.ConstantOperand (AC.GlobalReference name)) = P.ValSymbol <$> cvtNameS name
uOp op = (\(P.Typed _ val) -> val) <$> tOp op

tConst :: AC.Constant -> M (P.Typed P.Value)
tConst = \case
  AC.Int b v -> return $ P.Typed (P.PrimType (P.Integer b)) (P.ValInteger v)
  AC.Float (AF.Single f) -> return $ P.Typed (P.PrimType (P.FloatType P.Float)) (P.ValFloat f)
  AC.Float (AF.Double f) -> return $ P.Typed (P.PrimType (P.FloatType P.Double)) (P.ValDouble f)
  AC.Null t -> return $ P.Typed (cvtType t) P.ValNull
  AC.GetElementPtr inbounds t addr [idx] ->
    -- Special-case for single index here because then we know the result type,
    -- which llvm-pretty requires.
    -- Note: the second field of ConstGEP seems to be unused (?).
    -- Note: we don't tConst the addr because we _can_ figure out the type here
    -- and the thing being indexed is typically a global reference, for which
    -- tConst would fail anyway.
    P.Typed (P.PtrTo (cvtType t)) . P.ValConstExpr <$>
      (P.ConstGEP inbounds Nothing (cvtType t)
        <$> (P.Typed (P.PtrTo (cvtType t)) <$> uConst addr)
        <*> fmap pure (tConst idx))
  AC.Array t vs -> P.Typed (cvtType t) <$> (P.ValArray (cvtType t) <$> mapM uConst vs)
  c -> throwError $ "TODO all the constants: " ++ show c

uConst :: AC.Constant -> M P.Value
uConst = \case
  AC.GlobalReference name -> P.ValSymbol <$> cvtNameS name
  c -> (\(P.Typed _ val) -> val) <$> tConst c

cvtTail :: Maybe A.TailCallKind -> Bool
cvtTail Nothing = False
cvtTail (Just A.NoTail) = False
cvtTail (Just A.Tail) = True
cvtTail (Just A.MustTail) = True  -- TODO: not accurate

cvtArg :: (A.Operand, [A.ParameterAttribute]) -> M (P.Typed P.Value)
cvtArg (op, []) = tOp op
cvtArg _ = throwError "Call parameter attributes are unsupported"

cvtIPred :: AIP.IntegerPredicate -> P.ICmpOp
cvtIPred AIP.EQ = P.Ieq
cvtIPred AIP.NE = P.Ine
cvtIPred AIP.UGT = P.Iugt
cvtIPred AIP.UGE = P.Iuge
cvtIPred AIP.ULT = P.Iult
cvtIPred AIP.ULE = P.Iule
cvtIPred AIP.SGT = P.Isgt
cvtIPred AIP.SGE = P.Isge
cvtIPred AIP.SLT = P.Islt
cvtIPred AIP.SLE = P.Isle

cvtAtomicity :: Maybe A.Atomicity -> M (Maybe P.AtomicOrdering)
cvtAtomicity Nothing = return Nothing
cvtAtomicity _ = throwError "TODO atomicity"

cvtAlignment :: Word32 -> Maybe Int
cvtAlignment align = if align > 0 then Just (fromIntegral align) else Nothing

cvtFunAttr :: Either AFA.GroupID AFA.FunctionAttribute -> M P.FunAttr
cvtFunAttr (Left _) = throwError "Group IDs in function attributes unsupported"
cvtFunAttr (Right attr) = case attr of
  AFA.AlwaysInline -> return P.Alwaysinline
  AFA.Builtin -> return P.Builtin
  AFA.Cold -> return P.Cold
  AFA.InlineHint -> return P.Inlinehint
  AFA.JumpTable -> return P.Jumptable
  AFA.MinimizeSize -> return P.Minsize
  AFA.Naked -> return P.Naked
  AFA.NoBuiltin -> return P.Nobuiltin
  AFA.NoDuplicate -> return P.Noduplicate
  AFA.NoImplicitFloat -> return P.Noimplicitfloat
  AFA.NoInline -> return P.Noinline
  AFA.NoRedZone -> return P.Noredzone
  AFA.NoReturn -> return P.Noreturn
  AFA.NoUnwind -> return P.Nounwind
  AFA.NonLazyBind -> return P.Nonlazybind
  AFA.OptimizeForSize -> return P.Optsize
  AFA.OptimizeNone -> return P.Optnone
  AFA.ReadNone -> return P.Readnone
  AFA.ReadOnly -> return P.Readonly
  AFA.ReturnsTwice -> return P.ReturnsTwice
  AFA.SanitizeAddress -> return P.SanitizeAddress
  AFA.SanitizeMemory -> return P.SanitizeMemory
  AFA.SanitizeThread -> return P.SanitizeThread
  AFA.StackAlignment n -> return (P.AlignStack (fromIntegral n))
  AFA.StackProtect -> return P.SSP
  AFA.StackProtectReq -> return P.SSPreq
  AFA.StackProtectStrong -> return P.SSPstrong
  AFA.UWTable -> return P.UWTable
  _ -> throwError $ "Unsupported function attribute: " ++ show attr
