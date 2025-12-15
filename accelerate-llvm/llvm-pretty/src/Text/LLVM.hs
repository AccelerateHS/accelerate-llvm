{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.LLVM (
    -- * LLVM Monad
    LLVM()
  , runLLVM
  , emitTypeDecl
  , emitGlobal
  , emitDeclare
  , emitDefine

    -- * Alias Introduction
  , alias

    -- * Function Definition
  , freshSymbol
  , (:>)(..)
  , define, defineFresh, DefineArgs()
  , define'
  , declare
  , global
  , FunAttrs(..), emptyFunAttrs
    -- * Types
  , iT, ptrT, voidT, arrayT
  , (=:), (-:)

    -- * Values
  , IsValue(..)
  , int
  , integer
  , struct
  , array
  , string

    -- * Basic Blocks
  , BB()
  , runBB
  , freshLabel
  , label
  , comment
  , assign

    -- * Terminator Instructions
  , ret
  , retVoid
  , jump
  , br
  , unreachable
  , unwind

    -- * Binary Operations
  , add, fadd
  , sub, fsub
  , mul, fmul
  , udiv, sdiv, fdiv
  , urem, srem, frem

    -- * Bitwise Binary Operations
  , shl
  , lshr, ashr
  , band, bor, bxor

    -- * Conversion Operations
  , trunc
  , zext
  , sext
  , fptrunc
  , fpext
  , fptoui, fptosi
  , uitofp, sitofp
  , ptrtoint, inttoptr
  , bitcast

    -- * Aggregate Operations
  , extractValue
  , insertValue

    -- * Memory Access and Addressing Operations
  , alloca
  , load
  , store
  , getelementptr
  , nullPtr

    -- * Other Operations
  , icmp
  , fcmp
  , phi, PhiArg, from
  , select
  , call, call_
  , invoke
  , switch
  , shuffleVector

    -- * Re-exported
  , module Text.LLVM.AST
  ) where

import Text.LLVM.AST

import Control.Monad.Fix (MonadFix)
import Data.Char (ord)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Word (Word32, Word64)
import Data.Maybe (maybeToList)
import Data.String (IsString(..))
import MonadLib hiding (jump,Label)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map


-- Fresh Names -----------------------------------------------------------------

type Names = Map.Map String Int

-- | Avoid generating the provided name.  When the name already exists, return
-- Nothing.
avoid :: String -> Names -> Maybe Names
avoid name ns =
  case Map.lookup name ns of
    Nothing -> Just (Map.insert name 0 ns)
    Just _  -> Nothing

nextName :: String -> Names -> (String,Names)
nextName pfx ns =
  case Map.lookup pfx ns of
    Nothing -> (fmt (0 :: Int),  Map.insert pfx 1 ns)
    Just ix -> (fmt ix, Map.insert pfx (ix+1) ns)
  where
  fmt i = showString pfx (shows i "")


-- LLVM Monad ------------------------------------------------------------------

newtype LLVM a = LLVM
  { unLLVM :: WriterT Module (StateT Names Id) a
  } deriving (Functor,Applicative,Monad,MonadFix)

freshNameLLVM :: String -> LLVM String
freshNameLLVM pfx = LLVM $ do
  ns <- get
  let (n,ns') = nextName pfx ns
  set ns'
  return n

runLLVM :: LLVM a -> (a,Module)
runLLVM  = fst . runId . runStateT Map.empty . runWriterT . unLLVM

emitTypeDecl :: TypeDecl -> LLVM ()
emitTypeDecl td = LLVM (put emptyModule { modTypes = [td] })

emitGlobal :: Global -> LLVM (Typed Value)
emitGlobal g =
  do LLVM (put emptyModule { modGlobals = [g] })
     return (ptrT (globalType g) -: globalSym g)

emitDefine :: Define -> LLVM (Typed Value)
emitDefine d =
  do LLVM (put emptyModule { modDefines = [d] })
     return (defFunType d -: defName d)

emitDeclare :: Declare -> LLVM (Typed Value)
emitDeclare d =
  do LLVM (put emptyModule { modDeclares = [d] })
     return (decFunType d -: decName d)

alias :: Ident -> Type -> LLVM ()
alias i ty = emitTypeDecl (TypeDecl i ty)

freshSymbol :: LLVM Symbol
freshSymbol  = Symbol `fmap` freshNameLLVM "f"

-- | Emit a declaration.
declare :: Type -> Symbol -> [Type] -> Bool -> LLVM (Typed Value)
declare rty sym tys va = emitDeclare Declare
  { decLinkage    = Nothing
  , decVisibility = Nothing
  , decRetType    = rty
  , decName       = sym
  , decArgs       = tys
  , decVarArgs    = va
  , decAttrs      = []
  , decComdat     = Nothing
  }

-- | Emit a global declaration.
global :: GlobalAttrs -> Symbol -> Type -> Maybe Value -> LLVM (Typed Value)
global attrs sym ty mbVal = emitGlobal Global
  { globalSym      = sym
  , globalType     = ty
  , globalValue    = toValue `fmap` mbVal
  , globalAttrs    = attrs
  , globalAlign    = Nothing
  , globalMetadata = Map.empty
  }

-- | Output a somewhat clunky representation for a string global, that deals
-- well with escaping in the haskell-source string.
string :: Symbol -> String -> LLVM (Typed Value)
string sym str =
  global emptyGlobalAttrs { gaConstant = True } sym (typedType val)
      (Just (typedValue val))
  where
  bytes = [ int (fromIntegral (ord c)) | c <- str ]
  val   = array (iT 8) bytes


-- Function Definition ---------------------------------------------------------

data FunAttrs = FunAttrs
  { funLinkage    :: Maybe Linkage
  , funVisibility :: Maybe Visibility
  , funGC         :: Maybe GC
  } deriving (Show)

emptyFunAttrs :: FunAttrs
emptyFunAttrs  = FunAttrs
  { funLinkage    = Nothing
  , funVisibility = Nothing
  , funGC         = Nothing
  }


-- XXX Do not export
freshArg :: Type -> LLVM (Typed Ident)
freshArg ty = (Typed ty . Ident) `fmap` freshNameLLVM "a"

infixr 0 :>
data a :> b = a :> b
    deriving Show

-- | Types that can be used to define the body of a function.
class DefineArgs a k | a -> k where
  defineBody :: [Typed Ident] -> a -> k -> LLVM ([Typed Ident], [BasicBlock])

instance DefineArgs () (BB ()) where
  defineBody tys () body = return $ runBB $ do
    body
    return (reverse tys)

instance DefineArgs as k => DefineArgs (Type :> as) (Typed Value -> k) where
  defineBody args (ty :> as) f = do
    arg <- freshArg ty
    defineBody (arg:args) as (f (toValue `fmap` arg))

-- helper instances for DefineArgs

instance DefineArgs Type (Typed Value -> BB ()) where
  defineBody tys ty body = defineBody tys (ty :> ()) body

instance DefineArgs (Type,Type) (Typed Value -> Typed Value -> BB ()) where
  defineBody tys (a,b) body = defineBody tys (a :> b :> ()) body

instance DefineArgs (Type,Type,Type)
                    (Typed Value -> Typed Value -> Typed Value -> BB ()) where
  defineBody tys (a,b,c) body = defineBody tys (a :> b :> c :> ()) body

-- | Define a function.
define :: DefineArgs sig k => FunAttrs -> Type -> Symbol -> sig -> k
       -> LLVM (Typed Value)
define attrs rty fun sig k = do
  (args,body) <- defineBody [] sig k
  emitDefine Define
    { defLinkage    = funLinkage attrs
    , defVisibility = funVisibility attrs
    , defName       = fun
    , defRetType    = rty
    , defArgs       = args
    , defVarArgs    = False
    , defAttrs      = []
    , defSection    = Nothing
    , defGC         = funGC attrs
    , defBody       = body
    , defMetadata   = Map.empty
    , defComdat     = Nothing
    }

-- | A combination of define and @freshSymbol@.
defineFresh :: DefineArgs sig k => FunAttrs -> Type -> sig -> k
            -> LLVM (Typed Value)
defineFresh attrs rty args body = do
  sym <- freshSymbol
  define attrs rty sym args body

-- | Function definition when the argument list isn't statically known.  This is
-- useful when generating code.
define' :: FunAttrs -> Type -> Symbol -> [Type] -> Bool
        -> ([Typed Value] -> BB ())
        -> LLVM (Typed Value)
define' attrs rty sym sig va k = do
  args <- mapM freshArg sig
  emitDefine Define
    { defLinkage    = funLinkage attrs
    , defVisibility = funVisibility attrs
    , defName       = sym
    , defRetType    = rty
    , defArgs       = args
    , defVarArgs    = va
    , defAttrs      = []
    , defSection    = Nothing
    , defGC         = funGC attrs
    , defBody       = snd (runBB (k (map (fmap toValue) args)))
    , defMetadata   = Map.empty
    , defComdat     = Nothing
    }

-- Basic Block Monad -----------------------------------------------------------

newtype BB a = BB
  { unBB :: WriterT [BasicBlock] (StateT RW Id) a
  } deriving (Functor,Applicative,Monad,MonadFix)

avoidName :: String -> BB ()
avoidName name = BB $ do
  rw <- get
  case avoid name (rwNames rw) of
    Just ns' -> set rw { rwNames = ns' }
    Nothing  -> error ("avoidName: " ++ name ++ " already registered")

freshNameBB :: String -> BB String
freshNameBB pfx = BB $ do
  rw <- get
  let (n,ns') = nextName pfx (rwNames rw)
  set rw { rwNames = ns' }
  return n

runBB :: BB a -> (a,[BasicBlock])
runBB m =
  case runId (runStateT emptyRW (runWriterT (unBB body))) of
    ((a,bbs),_rw) -> (a,bbs)
  where
  -- make sure that the last block is terminated
  body = do
    res <- m
    terminateBasicBlock
    return res

data RW = RW
  { rwNames :: Names
  , rwLabel :: Maybe BlockLabel
  , rwStmts :: Seq.Seq Stmt
  } deriving Show

emptyRW :: RW
emptyRW  = RW
  { rwNames = Map.empty
  , rwLabel = Nothing
  , rwStmts = Seq.empty
  }

rwBasicBlock :: RW -> (RW,Maybe BasicBlock)
rwBasicBlock rw
  | Seq.null (rwStmts rw) = (rw,Nothing)
  | otherwise             =
      let rw' = rw { rwLabel = Nothing, rwStmts = Seq.empty }
          bb  = BasicBlock (rwLabel rw) (F.toList (rwStmts rw))
       in (rw',Just bb)

emitStmt :: Stmt -> BB ()
emitStmt stmt = do
  BB $ do
    rw <- get
    set $! rw { rwStmts = rwStmts rw Seq.|> stmt }
  when (isTerminator (stmtInstr stmt)) terminateBasicBlock

effect :: Instr -> BB ()
effect i = emitStmt (Effect i mempty [])

observe :: Type -> Instr -> BB (Typed Value)
observe ty i = do
  name <- freshNameBB "r"
  let res = Ident name
  emitStmt (Result res i mempty [])
  return (Typed ty (ValIdent res))


-- Basic Blocks ----------------------------------------------------------------

freshLabel :: BB Ident
freshLabel  = Ident `fmap` freshNameBB "L"

-- | Force termination of the current basic block, and start a new one with the
-- given label.  If the previous block had no instructions defined, it will just
-- be thrown away.
label :: Ident -> BB ()
label l = do
  terminateBasicBlock
  BB $ do
    rw <- get
    set $! rw { rwLabel = Just (Named l) }

instance IsString (BB a) where
  fromString l = do
    label (fromString l)
    return (error ("Label ``" ++ l ++ "'' has no value"))

terminateBasicBlock :: BB ()
terminateBasicBlock  = BB $ do
  rw <- get
  let (rw',bb) = rwBasicBlock rw
  put (maybeToList bb)
  set rw'


-- Type Helpers ----------------------------------------------------------------

iT :: Word32 -> Type
iT  = PrimType . Integer

ptrT :: Type -> Type
ptrT  = \ty -> PtrTo ty defaultAddrSpace

voidT :: Type
voidT  = PrimType Void

arrayT :: Word64 -> Type -> Type
arrayT  = Array


-- Value Helpers ---------------------------------------------------------------

class IsValue a where
  toValue :: a -> Value

instance IsValue Value where
  toValue = id

instance IsValue a => IsValue (Typed a) where
  toValue = toValue . typedValue

instance IsValue Bool where
  toValue = ValBool

instance IsValue Integer where
  toValue = ValInteger

instance IsValue Int where
  toValue = ValInteger . toInteger

instance IsValue Int8 where
  toValue = ValInteger . toInteger

instance IsValue Int16 where
  toValue = ValInteger . toInteger

instance IsValue Int32 where
  toValue = ValInteger . toInteger

instance IsValue Int64 where
  toValue = ValInteger . toInteger

instance IsValue Float where
  toValue = ValFloat

instance IsValue Double where
  toValue = ValDouble

instance IsValue Ident where
  toValue = ValIdent

instance IsValue Symbol where
  toValue = ValSymbol

(-:) :: IsValue a => Type -> a -> Typed Value
ty -: a = ty =: toValue a

(=:) :: Type -> a -> Typed a
ty =: a = Typed
  { typedType  = ty
  , typedValue = a
  }

int :: Int -> Value
int  = toValue

integer :: Integer -> Value
integer  = toValue

struct :: Bool -> [Typed Value] -> Typed Value
struct packed tvs
  | packed    = PackedStruct (map typedType tvs) =: ValPackedStruct tvs
  | otherwise = Struct (map typedType tvs)       =: ValStruct tvs

array :: Type -> [Value] -> Typed Value
array ty vs = Typed (Array (fromIntegral (length vs)) ty) (ValArray ty vs)


-- Instructions ----------------------------------------------------------------

comment :: String -> BB ()
comment str = effect (Comment str)

-- | Emit an assignment that uses the given identifier to name the result of the
-- BB operation.
--
-- WARNING: this can throw errors.
assign :: IsValue a => Ident -> BB (Typed a) -> BB (Typed Value)
assign r@(Ident name) body = do
  avoidName name
  tv <- body
  rw <- BB get
  case Seq.viewr (rwStmts rw) of

    stmts Seq.:> Result _ i d m ->
      do BB (set rw { rwStmts = stmts Seq.|> Result r i d m })
         return (const (ValIdent r) `fmap` tv)

    _ -> error "assign: invalid argument"

-- | Emit the ``ret'' instruction and terminate the current basic block.
ret :: IsValue a => Typed a -> BB ()
ret tv = effect (Ret (toValue `fmap` tv))

-- | Emit ``ret void'' and terminate the current basic block.
retVoid :: BB ()
retVoid  = effect RetVoid

jump :: Ident -> BB ()
jump l = effect (Jump (Named l))

br :: IsValue a => Typed a -> Ident -> Ident -> BB ()
br c t f = effect (Br (toValue `fmap` c) (Named t) (Named f))

unreachable :: BB ()
unreachable  = effect Unreachable

unwind :: BB ()
unwind  = effect Unwind

binop :: (IsValue a, IsValue b)
      => (Typed Value -> Value -> Instr) -> Typed a -> b -> BB (Typed Value)
binop k l r = observe (typedType l) (k (toValue `fmap` l) (toValue r))

add :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
add  = binop (Arith (Add False False))

fadd :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fadd  = binop (Arith (FAdd []))

sub :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
sub  = binop (Arith (Sub False False))

fsub :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fsub  = binop (Arith (FSub []))

mul :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
mul  = binop (Arith (Mul False False))

fmul :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fmul  = binop (Arith (FMul []))

udiv :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
udiv  = binop (Arith (UDiv False))

sdiv :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
sdiv  = binop (Arith (SDiv False))

fdiv :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fdiv  = binop (Arith (FDiv []))

urem :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
urem  = binop (Arith URem)

srem :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
srem  = binop (Arith SRem)

frem :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
frem  = binop (Arith (FRem []))

shl :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
shl  = binop (Bit (Shl False False))

lshr :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
lshr  = binop (Bit (Lshr False))

ashr :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
ashr  = binop (Bit (Ashr False))

band :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
band  = binop (Bit And)

bor :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
bor  = binop (Bit Or)

bxor :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
bxor  = binop (Bit Xor)

-- | Returns the value stored in the member field of an aggregate value.
extractValue :: IsValue a => Typed a -> Int32 -> BB (Typed Value)
extractValue ta i =
  let etp = case typedType ta of
              Struct fl -> fl !! fromIntegral i
              Array _l etp' -> etp'
              _ -> error "extractValue not given a struct or array."
   in observe etp (ExtractValue (toValue `fmap` ta) [i])

-- | Inserts a value into the member field of an aggregate value, and returns
-- the new value.
insertValue :: (IsValue a, IsValue b)
            => Typed a -> Typed b -> Int32 -> BB (Typed Value)
insertValue ta tv i =
  observe (typedType ta)
      (InsertValue (toValue `fmap` ta) (toValue `fmap` tv) [i])

shuffleVector :: (IsValue a, IsValue b, IsValue c)
              => Typed a -> b -> c -> BB (Typed Value)
shuffleVector vec1 vec2 mask =
  case typedType vec1 of
    Vector n _ -> observe (typedType vec1)
                $ ShuffleVector (toValue `fmap` vec1) (toValue vec2)
                $ Typed (Vector n (PrimType (Integer 32))) (toValue mask)
    _          -> error "shuffleVector not given a vector"

alloca :: Type -> Maybe (Typed Value) -> Maybe Int -> BB (Typed Value)
alloca ty mb align = observe (PtrTo ty defaultAddrSpace) (Alloca ty es align)
  where
  es = fmap toValue `fmap` mb

load :: IsValue a => Type -> Typed a -> Maybe Align -> BB (Typed Value)
load ty ptr ma = observe ty (Load False ty (toValue `fmap` ptr) Nothing ma)

store :: (IsValue a, IsValue b) => a -> Typed b -> Maybe Align -> BB ()
store a ptr ma =
  case typedType ptr of
    PtrTo ty _ -> effect (Store False (ty -: a) (toValue `fmap` ptr) Nothing ma)
    _          -> error "store not given a pointer"

nullPtr :: Type -> Typed Value
nullPtr ty = ptrT ty =: ValNull

convop :: IsValue a
       => (Typed Value -> Type -> Instr) -> Typed a -> Type -> BB (Typed Value)
convop k a ty = observe ty (k (toValue `fmap` a) ty)

trunc :: IsValue a => Typed a -> Type -> BB (Typed Value)
trunc  = convop (Conv (Trunc False False))

zext :: IsValue a => Typed a -> Type -> BB (Typed Value)
zext  = convop (Conv (ZExt False))

sext :: IsValue a => Typed a -> Type -> BB (Typed Value)
sext  = convop (Conv SExt)

fptrunc :: IsValue a => Typed a -> Type -> BB (Typed Value)
fptrunc  = convop (Conv FpTrunc)

fpext :: IsValue a => Typed a -> Type -> BB (Typed Value)
fpext  = convop (Conv FpExt)

fptoui :: IsValue a => Typed a -> Type -> BB (Typed Value)
fptoui  = convop (Conv FpToUi)

fptosi :: IsValue a => Typed a -> Type -> BB (Typed Value)
fptosi  = convop (Conv FpToSi)

uitofp :: IsValue a => Typed a -> Type -> BB (Typed Value)
uitofp  = convop (Conv (UiToFp False))

sitofp :: IsValue a => Typed a -> Type -> BB (Typed Value)
sitofp  = convop (Conv SiToFp)

ptrtoint :: IsValue a => Typed a -> Type -> BB (Typed Value)
ptrtoint  = convop (Conv PtrToInt)

inttoptr :: IsValue a => Typed a -> Type -> BB (Typed Value)
inttoptr  = convop (Conv IntToPtr)

bitcast :: IsValue a => Typed a -> Type -> BB (Typed Value)
bitcast  = convop (Conv BitCast)

icmp :: (IsValue a, IsValue b) => ICmpOp -> Typed a -> b -> BB (Typed Value)
icmp op l r = observe (iT 1) (ICmp False op (toValue `fmap` l) (toValue r))

fcmp :: (IsValue a, IsValue b) => FCmpOp -> Typed a -> b -> BB (Typed Value)
fcmp op l r = observe (iT 1) (FCmp [] op (toValue `fmap` l) (toValue r))

data PhiArg = PhiArg Value BlockLabel

from :: IsValue a => a -> BlockLabel -> PhiArg
from a = PhiArg (toValue a)

phi :: Type -> [PhiArg] -> BB (Typed Value)
phi ty vs = observe ty (Phi [] ty [ (v,l) | PhiArg v l <- vs ])

select :: (IsValue a, IsValue b, IsValue c)
       => Typed a -> Typed b -> Typed c -> BB (Typed Value)
select c t f = observe (typedType t)
             $ Select [] (toValue `fmap` c) (toValue `fmap` t) (toValue f)

getelementptr :: IsValue a
              => Type -> Typed a -> [Typed Value] -> BB (Typed Value)
getelementptr ty ptr ixs = observe ty (GEP [] ty (toValue `fmap` ptr) ixs)

-- | Emit a call instruction, and generate a new variable for its result.
call :: IsValue a => Typed a -> [Typed Value] -> BB (Typed Value)
call sym vs = case typedType sym of
  PtrTo ty@(FunTy rty _ _) _ -> observe rty (Call False [] ty (toValue sym) vs)
  _                          -> error "invalid function type given to call"

-- | Emit a call instruction, but don't generate a new variable for its result.
call_ :: IsValue a => Typed a -> [Typed Value] -> BB ()
call_ sym vs = effect (Call False [] (typedType sym) (toValue sym) vs)

-- | Emit an invoke instruction, and generate a new variable for its result.
invoke :: IsValue a =>
          Type -> a -> [Typed Value] -> Ident -> Ident -> BB (Typed Value)
invoke rty sym vs to uw = observe rty
                        $ Invoke rty (toValue sym) vs (Named to) (Named uw)

-- | Emit a call instruction, but don't generate a new variable for its result.
switch :: IsValue a => Typed a -> Ident -> [(Integer, Ident)] -> BB ()
switch idx def dests = effect (Switch (toValue `fmap` idx) (Named def)
                                      (map (\(n, l) -> (n, Named l)) dests))
