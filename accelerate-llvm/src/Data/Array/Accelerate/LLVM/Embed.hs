{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Embed
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Embed (

  Embed(..),
  embedAfun, embedOpenAfun,
  embedOpenAcc,

) where

import LLVM.AST.Type.Name

import Data.Array.Accelerate.AST                                    ( liftIdx, liftConst, liftSliceIndex, liftPrimConst, liftPrimFun, liftALhs, liftELhs, liftArray, liftArraysR, liftArrayR, liftScalarType, liftShapeR, liftVecR, liftTupleType, liftIntegralType )
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar                            ( liftForeign )
import Data.Array.Accelerate.Error

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Link

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
    :: Embed arch
    => arch
    -> CompiledAfun arch f
    -> Q (TExp (ExecAfun arch f))
embedAfun = embedOpenAfun

{-# INLINEABLE embedOpenAfun #-}
embedOpenAfun
    :: (Embed arch)
    => arch
    -> CompiledOpenAfun arch aenv f
    -> Q (TExp (ExecOpenAfun arch aenv f))
embedOpenAfun arch (Alam lhs l) = [|| Alam $$(liftALhs lhs) $$(embedOpenAfun arch l) ||]
embedOpenAfun arch (Abody b)    = [|| Abody $$(embedOpenAcc arch b) ||]

{-# INLINEABLE embedOpenAcc #-}
embedOpenAcc
    :: forall arch aenv arrs. Embed arch
    => arch
    -> CompiledOpenAcc arch aenv arrs
    -> Q (TExp (ExecOpenAcc arch aenv arrs))
embedOpenAcc arch = liftA
  where
    liftA :: CompiledOpenAcc arch aenv' arrs' -> Q (TExp (ExecOpenAcc arch aenv' arrs'))
    liftA acc = case acc of
        PlainAcc repr pacc          -> [|| EvalAcc $$(liftArraysR repr) $$(liftPreOpenAccCommand arch pacc) ||]
        BuildAcc repr aenv obj pacc -> [|| ExecAcc $$(liftArraysR repr) $$(liftGamma aenv) $$(embedForTarget arch obj) $$(liftPreOpenAccSkeleton arch pacc) ||]

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
        liftIM im =
          TH.TExp . TH.ListE <$> mapM (\(k,v) -> TH.unTypeQ [|| (k, $$(liftV v)) ||]) im
#endif
    liftV :: (Label, Idx' aenv') -> Q (TExp (Label, Idx' aenv'))
    liftV (Label n, Idx' repr ix) = [|| (Label $$(liftSBS n), Idx' $$(liftArrayR repr) $$(liftIdx ix)) ||]

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
    :: Embed arch
    => arch
    -> PreOpenAfun (CompiledOpenAcc arch) aenv t
    -> Q (TExp (PreOpenAfun (ExecOpenAcc arch) aenv t))
liftPreOpenAfun arch (Alam lhs f) = [|| Alam $$(liftALhs lhs) $$(liftPreOpenAfun arch f) ||]
liftPreOpenAfun arch (Abody b)    = [|| Abody $$(embedOpenAcc arch b) ||]

{-# INLINEABLE liftPreOpenAccCommand #-}
liftPreOpenAccCommand
    :: forall arch aenv a. Embed arch
    => arch
    -> PreOpenAccCommand CompiledOpenAcc arch aenv a
    -> Q (TExp (PreOpenAccCommand ExecOpenAcc arch aenv a))
liftPreOpenAccCommand arch pacc =
  let
      liftA :: CompiledOpenAcc arch aenv' arrs -> Q (TExp (ExecOpenAcc arch aenv' arrs))
      liftA = embedOpenAcc arch

      liftE :: PreExp ArrayVar aenv t -> Q (TExp (PreExp ArrayVar aenv t))
      liftE = liftPreOpenExp arch

      liftAF :: PreOpenAfun (CompiledOpenAcc arch) aenv f -> Q (TExp (PreOpenAfun (ExecOpenAcc arch) aenv f))
      liftAF = liftPreOpenAfun arch
  in
  case pacc of
    Avar v            -> [|| Avar $$(liftArrayVar v) ||]
    Alet lhs bnd body -> [|| Alet $$(liftALhs lhs) $$(liftA bnd) $$(liftA body) ||]
    Alloc repr sh     -> [|| Alloc $$(liftArrayR repr) $$(liftE sh) ||]
    Use repr a        -> [|| Use $$(liftArrayR repr) $$(liftArray repr a) ||]
    Unit tp e         -> [|| Unit $$(liftTupleType tp) $$(liftE e) ||]
    Apair a1 a2       -> [|| Apair $$(liftA a1) $$(liftA a2) ||]
    Anil              -> [|| Anil ||]
    Apply repr f a    -> [|| Apply $$(liftArraysR repr) $$(liftAF f) $$(liftA a) ||]
    Acond p t e       -> [|| Acond $$(liftE p) $$(liftA t) $$(liftA e) ||]
    Awhile p f a      -> [|| Awhile $$(liftAF p) $$(liftAF f) $$(liftA a) ||]
    Reshape shr sh v  -> [|| Reshape $$(liftShapeR shr) $$(liftE sh) $$(liftArrayVar v) ||]
    Unzip tix v       -> [|| Unzip $$(liftUnzipIdx tix) $$(liftArrayVar v) ||]
    Aforeign{}        -> $internalError "liftPreOpenAcc" "using foreign functions from template-haskell is not supported yet"

{-# INLINEABLE liftPreOpenAccSkeleton #-}
liftPreOpenAccSkeleton
    :: forall arch aenv a. Embed arch
    => arch
    -> PreOpenAccSkeleton CompiledOpenAcc arch aenv a
    -> Q (TExp (PreOpenAccSkeleton ExecOpenAcc arch aenv a))
liftPreOpenAccSkeleton arch pacc =
  let
      liftA :: CompiledOpenAcc arch aenv arrs -> Q (TExp (ExecOpenAcc arch aenv arrs))
      liftA = embedOpenAcc arch

      liftD :: DelayedOpenAcc CompiledOpenAcc arch aenv arrs -> Q (TExp (DelayedOpenAcc ExecOpenAcc arch aenv arrs))
      liftD (Delayed repr sh) = [|| Delayed $$(liftArrayR repr) $$(liftE sh) ||]
      liftD (Manifest repr a) = [|| Manifest $$(liftArraysR repr) $$(liftA a) ||]

      liftE :: PreExp ArrayVar aenv t -> Q (TExp (PreExp ArrayVar aenv t))
      liftE = liftPreOpenExp arch

      liftS :: ShapeR sh -> sh -> Q (TExp sh)
      liftS shr sh = [|| $$(liftConst (shapeType shr) sh) ||]
  in
  case pacc of
    Map tp a             -> [|| Map $$(liftTupleType tp) $$(liftA a) ||]
    Generate repr sh     -> [|| Generate $$(liftArrayR repr) $$(liftE sh) ||]
    Transform repr sh a  -> [|| Transform $$(liftArrayR repr) $$(liftE sh) $$(liftA a) ||]
    Backpermute shr sh a -> [|| Backpermute $$(liftShapeR shr) $$(liftE sh) $$(liftA a) ||]
    Fold a               -> [|| Fold $$(liftD a) ||]
    Fold1 a              -> [|| Fold1 $$(liftD a) ||]
    FoldSeg i a s        -> [|| FoldSeg $$(liftIntegralType i) $$(liftD a) $$(liftD s) ||]
    Fold1Seg i a s       -> [|| Fold1Seg $$(liftIntegralType i) $$(liftD a) $$(liftD s) ||]
    Scanl a              -> [|| Scanl $$(liftD a) ||]
    Scanl1 a             -> [|| Scanl1 $$(liftD a) ||]
    Scanl' a             -> [|| Scanl' $$(liftD a) ||]
    Scanr a              -> [|| Scanr $$(liftD a) ||]
    Scanr1 a             -> [|| Scanr1 $$(liftD a) ||]
    Scanr' a             -> [|| Scanr' $$(liftD a) ||]
    Permute d a          -> [|| Permute $$(liftA d) $$(liftD a) ||]
    Stencil1 tp h a      -> [|| Stencil1 $$(liftTupleType tp) $$(liftS (arrayRshape $ arrayRepr a) h) $$(liftD a) ||]
    Stencil2 tp h a b    -> [|| Stencil2 $$(liftTupleType tp) $$(liftS (arrayRshape $ arrayRepr a) h) $$(liftD a) $$(liftD b) ||]

{-# INLINEABLE liftPreOpenFun #-}
liftPreOpenFun
    :: Embed arch
    => arch
    -> PreOpenFun ArrayVar env aenv t
    -> Q (TExp (PreOpenFun ArrayVar env aenv t))
liftPreOpenFun arch (Lam lhs f) = [|| Lam $$(liftELhs lhs) $$(liftPreOpenFun arch f) ||]
liftPreOpenFun arch (Body b)    = [|| Body $$(liftPreOpenExp arch b) ||]

{-# INLINEABLE liftPreOpenExp #-}
liftPreOpenExp
    :: forall arch env aenv t. Embed arch
    => arch
    -> PreOpenExp ArrayVar env aenv t
    -> Q (TExp (PreOpenExp ArrayVar env aenv t))
liftPreOpenExp arch pexp =
  let
      liftA :: ArrayVar aenv arr -> Q (TExp (ArrayVar aenv arr))
      liftA (Var repr ix) = [|| Var $$(liftArrayR repr) $$(liftIdx ix) ||]

      liftE :: PreOpenExp ArrayVar env aenv e -> Q (TExp (PreOpenExp ArrayVar env aenv e))
      liftE = liftPreOpenExp arch

      liftF :: PreOpenFun ArrayVar env aenv f -> Q (TExp (PreOpenFun ArrayVar env aenv f))
      liftF = liftPreOpenFun arch
  in
  case pexp of
    Let lhs bnd body          -> [|| Let $$(liftELhs lhs) $$(liftPreOpenExp arch bnd) $$(liftPreOpenExp arch body) ||]
    Evar (Var tp ix)          -> [|| Evar (Var $$(liftScalarType tp) $$(liftIdx ix)) ||]
    Foreign asm f x           -> [|| Foreign $$(liftForeign asm) $$(liftPreOpenFun arch f) $$(liftE x) ||]
    Const t c                 -> [|| Const $$(liftScalarType t) $$(liftConst (TupRsingle t) c) ||]
    Undef t                   -> [|| Undef $$(liftScalarType t) ||]
    Nil                       -> [|| Nil ||]
    Pair e1 e2                -> [|| Pair $$(liftE e1) $$(liftE e2) ||]
    VecPack   vecr e          -> [|| VecPack   $$(liftVecR vecr) $$(liftE e) ||]
    VecUnpack vecr e          -> [|| VecUnpack $$(liftVecR vecr) $$(liftE e) ||]
    IndexSlice slice slix sh  -> [|| IndexSlice $$(liftSliceIndex slice) $$(liftE slix) $$(liftE sh) ||]
    IndexFull slice slix sl   -> [|| IndexFull $$(liftSliceIndex slice) $$(liftE slix) $$(liftE sl) ||]
    ToIndex shr sh ix         -> [|| ToIndex $$(liftShapeR shr) $$(liftE sh) $$(liftE ix) ||]
    FromIndex shr sh ix       -> [|| FromIndex $$(liftShapeR shr) $$(liftE sh) $$(liftE ix) ||]
    Cond p t e                -> [|| Cond $$(liftE p) $$(liftE t) $$(liftE e) ||]
    While p f x               -> [|| While $$(liftF p) $$(liftF f) $$(liftE x) ||]
    PrimConst t               -> [|| PrimConst $$(liftPrimConst t) ||]
    PrimApp f x               -> [|| PrimApp $$(liftPrimFun f) $$(liftE x) ||]
    Index a ix                -> [|| Index $$(liftA a) $$(liftE ix) ||]
    LinearIndex a ix          -> [|| LinearIndex $$(liftA a) $$(liftE ix) ||]
    Shape a                   -> [|| Shape $$(liftA a) ||]
    ShapeSize shr ix          -> [|| ShapeSize $$(liftShapeR shr) $$(liftE ix) ||]
    Coerce t1 t2 x            -> [|| Coerce $$(liftScalarType t1) $$(liftScalarType t2) $$(liftE x) ||]

liftArrayVar :: ArrayVar aenv v -> Q (TExp (ArrayVar aenv v))
liftArrayVar (Var tp v) = [|| Var $$(liftArrayR tp) $$(liftIdx v) ||]

liftUnzipIdx :: UnzipIdx tup e -> Q (TExp (UnzipIdx tup e))
liftUnzipIdx UnzipId = [|| UnzipId ||]
liftUnzipIdx (UnzipPrj PairIdxLeft  ix) = [|| UnzipPrj PairIdxLeft  $$(liftUnzipIdx ix) ||]
liftUnzipIdx (UnzipPrj PairIdxRight ix) = [|| UnzipPrj PairIdxRight $$(liftUnzipIdx ix) ||]
liftUnzipIdx UnzipUnit = [|| UnzipUnit ||]
liftUnzipIdx (UnzipPair ix1 ix2) = [|| UnzipPair $$(liftUnzipIdx ix1) $$(liftUnzipIdx ix2) ||]
