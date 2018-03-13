{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Embed
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Embed (

  Embed(..),
  embedAfun, embedOpenAfun,
  embedOpenAcc,

) where

import LLVM.AST.Type.Name

import Data.Array.Accelerate.AST                                    ( liftIdx, liftTupleIdx, liftArrays, liftConst, liftSliceIndex, liftPrimConst, liftPrimFun )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Link

import Data.Typeable
import Data.ByteString.Short                                        ( ShortByteString )
import GHC.Ptr                                                      ( Ptr(..) )
import Language.Haskell.TH                                          ( Q, TExp )
import System.IO.Unsafe
import qualified Data.ByteString.Short.Internal                     as BS
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH

#if MIN_VERSION_containers(0,5,9)
import qualified Data.IntMap.Internal                               as IM
#elif MIN_VERSION_containers(0,5,8)
import qualified Data.IntMap.Base                                   as IM
#else
import qualified Data.IntMap                                        as IM
#endif


class Embed arch where

  -- | Turn the compiled object into a TemplateHaskell expression, suitable for
  -- use in a splice. The splice should evaluate into the backend-specific
  -- executable representation.
  --
  embedForTarget
      :: arch
      -> ObjectR arch
      -> Q (TExp (ExecutableR arch))


-- | Embed the compiled array function into a TemplateHaskell expression,
-- suitable for use in a splice.
--
{-# INLINEABLE embedAfun #-}
embedAfun
    :: (Embed arch, Typeable arch)
    => arch
    -> CompiledAfun arch f
    -> Q (TExp (ExecAfun arch f))
embedAfun = embedOpenAfun

{-# INLINEABLE embedOpenAfun #-}
embedOpenAfun
    :: (Embed arch, Typeable arch, Typeable aenv)
    => arch
    -> CompiledOpenAfun arch aenv f
    -> Q (TExp (ExecOpenAfun arch aenv f))
embedOpenAfun arch (Alam l)  = [|| Alam $$(embedOpenAfun arch l) ||]
embedOpenAfun arch (Abody b) = [|| Abody $$(embedOpenAcc arch b) ||]

{-# INLINEABLE embedOpenAcc #-}
embedOpenAcc
    :: forall arch aenv arrs. (Embed arch, Typeable arch, Typeable aenv, Typeable arrs)
    => arch
    -> CompiledOpenAcc arch aenv arrs
    -> Q (TExp (ExecOpenAcc arch aenv arrs))
embedOpenAcc arch = liftA
  where
    liftA :: (Typeable aenv', Typeable arrs') => CompiledOpenAcc arch aenv' arrs' -> Q (TExp (ExecOpenAcc arch aenv' arrs'))
    liftA (PlainAcc pacc)          = withSigE [|| EvalAcc $$(liftPreOpenAccCommand arch pacc) ||]
    liftA (BuildAcc aenv obj pacc) = withSigE [|| ExecAcc $$(liftGamma aenv) $$(embedForTarget arch obj) $$(liftPreOpenAccSkeleton arch pacc) ||]

    liftGamma :: Gamma aenv' -> Q (TExp (Gamma aenv'))
#if MIN_VERSION_containers(0,5,8)
    liftGamma IM.Nil           = [|| IM.Nil ||]
    liftGamma (IM.Bin p m l r) = [|| IM.Bin p m $$(liftGamma l) $$(liftGamma r) ||]
    liftGamma (IM.Tip k v)     = [|| IM.Tip k $$(liftV v) ||]
#else
    -- O(n) at runtime to reconstruct the set
    liftGamma aenv             = [|| IM.fromAscList $$(liftIM (IM.toAscList aenv)) ||]
      where
        liftIM :: [(Int, (Label, Idx' aenv'))] -> Q (TExp [(Int, (Label, Idx' aenv'))])
        liftIM im = do
          im' <- mapM (\(k,v) -> TH.unTypeQ [|| (k, $$(liftV v)) ||]) im
          TH.unsafeTExpCoerce (return $ TH.ListE im')
#endif
    liftV :: (Label, Idx' aenv') -> Q (TExp (Label, Idx' aenv'))
    liftV (Label n, Idx' ix) = [|| (Label $$(liftSBS n), Idx' $$(liftIdx ix)) ||]

    -- O(n) at runtime to copy from the Addr# to the ByteArray#. We should
    -- be able to do this without copying, but I don't think the definition of
    -- ByteArray# is exported (or it is deeply magical).
    liftSBS :: ShortByteString -> Q (TExp ShortByteString)
    liftSBS bs =
      let bytes = BS.unpack bs
          len   = BS.length bs
      in
      [|| unsafePerformIO $ BS.createFromPtr $$( TH.unsafeTExpCoerce [| Ptr $(TH.litE (TH.StringPrimL bytes)) |]) len ||]


{-# INLINEABLE liftPreOpenAfun #-}
liftPreOpenAfun
    :: (Embed arch, Typeable arch, Typeable aenv)
    => arch
    -> PreOpenAfun (CompiledOpenAcc arch) aenv t
    -> Q (TExp (PreOpenAfun (ExecOpenAcc arch) aenv t))
liftPreOpenAfun arch (Alam f)  = [|| Alam  $$(liftPreOpenAfun arch f) ||]
liftPreOpenAfun arch (Abody b) = [|| Abody $$(embedOpenAcc arch b) ||]

{-# INLINEABLE liftPreOpenAccCommand #-}
liftPreOpenAccCommand
    :: forall arch aenv a. (Embed arch, Typeable arch, Typeable aenv)
    => arch
    -> PreOpenAccCommand CompiledOpenAcc arch aenv a
    -> Q (TExp (PreOpenAccCommand ExecOpenAcc arch aenv a))
liftPreOpenAccCommand arch pacc =
  let
      liftA :: (Typeable aenv', Typeable arrs) => CompiledOpenAcc arch aenv' arrs -> Q (TExp (ExecOpenAcc arch aenv' arrs))
      liftA = embedOpenAcc arch

      liftE :: PreExp (CompiledOpenAcc arch) aenv t -> Q (TExp (PreExp (ExecOpenAcc arch) aenv t))
      liftE = liftPreOpenExp arch

      liftAF :: PreOpenAfun (CompiledOpenAcc arch) aenv f -> Q (TExp (PreOpenAfun (ExecOpenAcc arch) aenv f))
      liftAF = liftPreOpenAfun arch

      liftAtuple :: Atuple (CompiledOpenAcc arch aenv) t -> Q (TExp (Atuple (ExecOpenAcc arch aenv) t))
      liftAtuple NilAtup          = [|| NilAtup ||]
      liftAtuple (SnocAtup tup a) = [|| SnocAtup $$(liftAtuple tup) $$(liftA a) ||]
  in
  case pacc of
    Avar ix           -> [|| Avar $$(liftIdx ix) ||]
    Alet bnd body     -> [|| Alet $$(liftA bnd) $$(liftA body) ||]
    Alloc sh          -> [|| Alloc $$(liftE sh) ||]
    Use a             -> [|| Use $$(liftArrays (arrays (undefined::a)) a) ||]
    Unit e            -> [|| Unit $$(liftE e) ||]
    Atuple tup        -> [|| Atuple $$(liftAtuple tup) ||]
    Aprj tix a        -> [|| Aprj $$(liftTupleIdx tix) $$(liftA a) ||]
    Apply f a         -> [|| Apply $$(liftAF f) $$(liftA a) ||]
    Acond p t e       -> [|| Acond $$(liftE p) $$(liftA t) $$(liftA e) ||]
    Awhile p f a      -> [|| Awhile $$(liftAF p) $$(liftAF f) $$(liftA a) ||]
    Reshape sh ix     -> [|| Reshape $$(liftE sh) $$(liftIdx ix) ||]
    Unzip tix ix      -> [|| Unzip $$(liftTupleIdx tix) $$(liftIdx ix) ||]
    Aforeign{}        -> $internalError "liftPreOpenAcc" "using foreign functions from template-haskell is not supported yet"

{-# INLINEABLE liftPreOpenAccSkeleton #-}
liftPreOpenAccSkeleton
    :: forall arch aenv a. (Embed arch, Typeable arch, Typeable aenv)
    => arch
    -> PreOpenAccSkeleton CompiledOpenAcc arch aenv a
    -> Q (TExp (PreOpenAccSkeleton ExecOpenAcc arch aenv a))
liftPreOpenAccSkeleton arch pacc =
  let
      liftA :: Typeable arrs => CompiledOpenAcc arch aenv arrs -> Q (TExp (ExecOpenAcc arch aenv arrs))
      liftA = embedOpenAcc arch

      liftE :: PreExp (CompiledOpenAcc arch) aenv t -> Q (TExp (PreExp (ExecOpenAcc arch) aenv t))
      liftE = liftPreOpenExp arch
  in
  case pacc of
    Map sh            -> [|| Map $$(liftE sh) ||]
    Generate sh       -> [|| Generate $$(liftE sh) ||]
    Transform sh      -> [|| Transform $$(liftE sh) ||]
    Backpermute sh    -> [|| Backpermute $$(liftE sh) ||]
    Fold sh           -> [|| Fold $$(liftE sh) ||]
    Fold1 sh          -> [|| Fold1 $$(liftE sh) ||]
    FoldSeg sa ss     -> [|| FoldSeg $$(liftE sa) $$(liftE ss) ||]
    Fold1Seg sa ss    -> [|| Fold1Seg $$(liftE sa) $$(liftE ss) ||]
    Scanl sh          -> [|| Scanl $$(liftE sh) ||]
    Scanl1 sh         -> [|| Scanl1 $$(liftE sh) ||]
    Scanl' sh         -> [|| Scanl' $$(liftE sh) ||]
    Scanr sh          -> [|| Scanr $$(liftE sh) ||]
    Scanr1 sh         -> [|| Scanr1 $$(liftE sh) ||]
    Scanr' sh         -> [|| Scanr' $$(liftE sh) ||]
    Permute sh a      -> [|| Permute $$(liftE sh) $$(liftA a) ||]
    Stencil sh        -> [|| Stencil $$(liftE sh) ||]
    Stencil2 sh1 sh2  -> [|| Stencil2 $$(liftE sh1) $$(liftE sh2) ||]

{-# INLINEABLE liftPreOpenFun #-}
liftPreOpenFun
    :: (Embed arch, Typeable arch, Typeable env, Typeable aenv)
    => arch
    -> PreOpenFun (CompiledOpenAcc arch) env aenv t
    -> Q (TExp (PreOpenFun (ExecOpenAcc arch) env aenv t))
liftPreOpenFun arch (Lam f)  = [|| Lam  $$(liftPreOpenFun arch f) ||]
liftPreOpenFun arch (Body b) = [|| Body $$(liftPreOpenExp arch b) ||]

{-# INLINEABLE liftPreOpenExp #-}
liftPreOpenExp
    :: forall arch env aenv t. (Embed arch, Typeable arch, Typeable env, Typeable aenv)
    => arch
    -> PreOpenExp (CompiledOpenAcc arch) env aenv t
    -> Q (TExp (PreOpenExp (ExecOpenAcc arch) env aenv t))
liftPreOpenExp arch pexp =
  let
      liftA :: Typeable arrs => CompiledOpenAcc arch aenv arrs -> Q (TExp (ExecOpenAcc arch aenv arrs))
      liftA = embedOpenAcc arch

      liftE :: PreOpenExp (CompiledOpenAcc arch) env aenv e -> Q (TExp (PreOpenExp (ExecOpenAcc arch) env aenv e))
      liftE = liftPreOpenExp arch

      liftF :: PreOpenFun (CompiledOpenAcc arch) env aenv f -> Q (TExp (PreOpenFun (ExecOpenAcc arch) env aenv f))
      liftF = liftPreOpenFun arch

      liftT :: Tuple (PreOpenExp (CompiledOpenAcc arch) env aenv) e -> Q (TExp (Tuple (PreOpenExp (ExecOpenAcc arch) env aenv) e))
      liftT NilTup          = [|| NilTup ||]
      liftT (SnocTup tup e) = [|| SnocTup $$(liftT tup) $$(liftE e) ||]
  in
  case pexp of
    Let bnd body              -> [|| Let $$(liftPreOpenExp arch bnd) $$(liftPreOpenExp arch body) ||]
    Var ix                    -> [|| Var $$(liftIdx ix) ||]
    Foreign asm f x           -> [|| Foreign $$(liftForeign asm) $$(liftPreOpenFun arch f) $$(liftE x) ||]
    Const c                   -> [|| Const $$(liftConst (eltType (undefined::t)) c) ||]
    Undef                     -> [|| Undef ||]
    Tuple tup                 -> [|| Tuple $$(liftT tup) ||]
    Prj tix e                 -> [|| Prj $$(liftTupleIdx tix) $$(liftE e) ||]
    IndexNil                  -> [|| IndexNil ||]
    IndexCons sh sz           -> [|| IndexCons $$(liftE sh) $$(liftE sz) ||]
    IndexHead sh              -> [|| IndexHead $$(liftE sh) ||]
    IndexTail sh              -> [|| IndexTail $$(liftE sh) ||]
    IndexAny                  -> [|| IndexAny ||]
    IndexSlice slice slix sh  -> withSigE [|| IndexSlice $$(liftSliceIndex slice) $$(withSigE (liftE slix)) $$(withSigE (liftE sh)) ||]
    IndexFull slice slix sl   -> withSigE [|| IndexFull $$(liftSliceIndex slice) $$(withSigE (liftE slix)) $$(withSigE (liftE sl)) ||]
    ToIndex sh ix             -> [|| ToIndex $$(liftE sh) $$(liftE ix) ||]
    FromIndex sh ix           -> [|| FromIndex $$(liftE sh) $$(liftE ix) ||]
    Cond p t e                -> [|| Cond $$(liftE p) $$(liftE t) $$(liftE e) ||]
    While p f x               -> [|| While $$(liftF p) $$(liftF f) $$(liftE x) ||]
    PrimConst t               -> [|| PrimConst $$(liftPrimConst t) ||]
    PrimApp f x               -> [|| PrimApp $$(liftPrimFun f) $$(liftE x) ||]
    Index a ix                -> [|| Index $$(liftA a) $$(liftE ix) ||]
    LinearIndex a ix          -> [|| LinearIndex $$(liftA a) $$(liftE ix) ||]
    Shape a                   -> [|| Shape $$(liftA a) ||]
    ShapeSize ix              -> [|| ShapeSize $$(liftE ix) ||]
    Intersect sh1 sh2         -> [|| Intersect $$(liftE sh1) $$(liftE sh2) ||]
    Union sh1 sh2             -> [|| Union $$(liftE sh1) $$(liftE sh2) ||]
    Coerce x                  -> [|| Coerce $$(liftE x) ||]


-- Utilities
-- ---------

withSigE :: forall e. Typeable e => Q (TExp e) -> Q (TExp e)
withSigE e = e `sigE` typeRepToType (typeOf (undefined::e))

sigE :: Q (TExp t) -> Q TH.Type -> Q (TExp t)
sigE e t = TH.unsafeTExpCoerce $ TH.sigE (TH.unTypeQ e) t

typeRepToType :: TypeRep -> Q TH.Type
typeRepToType trep = do
  let (con, args)     = splitTyConApp trep
      name            = TH.Name (TH.OccName (tyConName con)) (TH.NameG TH.TcClsName (TH.PkgName (tyConPackage con)) (TH.ModName (tyConModule con)))
      --
      appsT x []      = x
      appsT x (y:xs)  = appsT (TH.AppT x y) xs
      --
  resultArgs <- mapM typeRepToType args
  return (appsT (TH.ConT name) resultArgs)

