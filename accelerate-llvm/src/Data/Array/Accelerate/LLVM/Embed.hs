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
-- Copyright   : [2017..2020] The Accelerate Team
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

import Data.Array.Accelerate.AST                                    ( PreOpenAfun(..), ArrayVar, Direction(..), Exp, liftALeftHandSide, liftOpenExp, arrayR )
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

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
    :: (HasCallStack, Embed arch)
    => arch
    -> CompiledOpenAfun arch aenv f
    -> Q (TExp (ExecOpenAfun arch aenv f))
embedOpenAfun arch (Alam lhs l) = [|| Alam $$(liftALeftHandSide lhs) $$(embedOpenAfun arch l) ||]
embedOpenAfun arch (Abody b)    = [|| Abody $$(embedOpenAcc arch b) ||]

{-# INLINEABLE embedOpenAcc #-}
embedOpenAcc
    :: forall arch aenv arrs. (HasCallStack, Embed arch)
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
    :: (HasCallStack, Embed arch)
    => arch
    -> PreOpenAfun (CompiledOpenAcc arch) aenv t
    -> Q (TExp (PreOpenAfun (ExecOpenAcc arch) aenv t))
liftPreOpenAfun arch (Alam lhs f) = [|| Alam $$(liftALeftHandSide lhs) $$(liftPreOpenAfun arch f) ||]
liftPreOpenAfun arch (Abody b)    = [|| Abody $$(embedOpenAcc arch b) ||]

{-# INLINEABLE liftPreOpenAccCommand #-}
liftPreOpenAccCommand
    :: forall arch aenv a. (HasCallStack, Embed arch)
    => arch
    -> PreOpenAccCommand CompiledOpenAcc arch aenv a
    -> Q (TExp (PreOpenAccCommand ExecOpenAcc arch aenv a))
liftPreOpenAccCommand arch pacc =
  let
      liftA :: CompiledOpenAcc arch aenv' arrs -> Q (TExp (ExecOpenAcc arch aenv' arrs))
      liftA = embedOpenAcc arch

      liftE :: Exp aenv t -> Q (TExp (Exp aenv t))
      liftE = liftOpenExp

      liftAF :: PreOpenAfun (CompiledOpenAcc arch) aenv f -> Q (TExp (PreOpenAfun (ExecOpenAcc arch) aenv f))
      liftAF = liftPreOpenAfun arch
  in
  case pacc of
    Avar v            -> [|| Avar $$(liftArrayVar v) ||]
    Alet lhs bnd body -> [|| Alet $$(liftALeftHandSide lhs) $$(liftA bnd) $$(liftA body) ||]
    Alloc repr sh     -> [|| Alloc $$(liftArrayR repr) $$(liftE sh) ||]
    Use repr a        -> [|| Use $$(liftArrayR repr) $$(liftArray repr a) ||]
    Unit tp e         -> [|| Unit $$(liftTypeR tp) $$(liftE e) ||]
    Apair a1 a2       -> [|| Apair $$(liftA a1) $$(liftA a2) ||]
    Anil              -> [|| Anil ||]
    Apply repr f a    -> [|| Apply $$(liftArraysR repr) $$(liftAF f) $$(liftA a) ||]
    Acond p t e       -> [|| Acond $$(liftE p) $$(liftA t) $$(liftA e) ||]
    Awhile p f a      -> [|| Awhile $$(liftAF p) $$(liftAF f) $$(liftA a) ||]
    Reshape shr sh v  -> [|| Reshape $$(liftShapeR shr) $$(liftE sh) $$(liftArrayVar v) ||]
    Unzip tix v       -> [|| Unzip $$(liftUnzipIdx tix) $$(liftArrayVar v) ||]
    Aforeign{}        -> internalError "using foreign functions from template-haskell is not supported yet"

{-# INLINEABLE liftPreOpenAccSkeleton #-}
liftPreOpenAccSkeleton
    :: forall arch aenv a. (HasCallStack, Embed arch)
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

      liftE :: Exp aenv t -> Q (TExp (Exp aenv t))
      liftE = liftOpenExp

      liftS :: ShapeR sh -> sh -> Q (TExp sh)
      liftS shr sh = [|| $$(liftElt (shapeType shr) sh) ||]

      liftZ :: HasInitialValue -> Q (TExp HasInitialValue)
      liftZ True  = [|| True  ||]
      liftZ False = [|| False ||]

      liftDir :: Direction -> Q (TExp Direction)
      liftDir LeftToRight = [|| LeftToRight ||]
      liftDir RightToLeft = [|| RightToLeft ||]
  in
  case pacc of
    Map tp a             -> [|| Map $$(liftTypeR tp) $$(liftA a) ||]
    Generate repr sh     -> [|| Generate $$(liftArrayR repr) $$(liftE sh) ||]
    Transform repr sh a  -> [|| Transform $$(liftArrayR repr) $$(liftE sh) $$(liftA a) ||]
    Backpermute shr sh a -> [|| Backpermute $$(liftShapeR shr) $$(liftE sh) $$(liftA a) ||]
    Fold z a             -> [|| Fold $$(liftZ z) $$(liftD a) ||]
    FoldSeg i z a s      -> [|| FoldSeg $$(liftIntegralType i) $$(liftZ z) $$(liftD a) $$(liftD s) ||]
    Scan d z a           -> [|| Scan $$(liftDir d) $$(liftZ z) $$(liftD a) ||]
    Scan' d a            -> [|| Scan' $$(liftDir d) $$(liftD a) ||]
    Permute d a          -> [|| Permute $$(liftA d) $$(liftD a) ||]
    Stencil1 tp h a      -> [|| Stencil1 $$(liftTypeR tp) $$(liftS (arrayRshape $ arrayR a) h) $$(liftD a) ||]
    Stencil2 tp h a b    -> [|| Stencil2 $$(liftTypeR tp) $$(liftS (arrayRshape $ arrayR a) h) $$(liftD a) $$(liftD b) ||]

liftArrayVar :: ArrayVar aenv v -> Q (TExp (ArrayVar aenv v))
liftArrayVar (Var tp v) = [|| Var $$(liftArrayR tp) $$(liftIdx v) ||]

liftUnzipIdx :: UnzipIdx tup e -> Q (TExp (UnzipIdx tup e))
liftUnzipIdx UnzipId                    = [|| UnzipId ||]
liftUnzipIdx (UnzipPrj PairIdxLeft  ix) = [|| UnzipPrj PairIdxLeft  $$(liftUnzipIdx ix) ||]
liftUnzipIdx (UnzipPrj PairIdxRight ix) = [|| UnzipPrj PairIdxRight $$(liftUnzipIdx ix) ||]
liftUnzipIdx UnzipUnit                  = [|| UnzipUnit ||]
liftUnzipIdx (UnzipPair ix1 ix2)        = [|| UnzipPair $$(liftUnzipIdx ix1) $$(liftUnzipIdx ix2) ||]

