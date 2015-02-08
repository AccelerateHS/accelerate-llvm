{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Exp
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Exp
  where

import Prelude                                                  hiding ( exp, any, uncurry, fst, snd )
import Control.Applicative                                      hiding ( Const )

import Data.Array.Accelerate.AST                                hiding ( Val(..), prj )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                        hiding ( toTuple )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad                 ( CodeGen )
import Data.Array.Accelerate.LLVM.CodeGen.Skeleton
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic  as A


-- | A class covering code generation for a subset of the scalar operations.
-- All operations (except Foreign) have a default instance, but this allows a
-- backend to specialise the implementation for the more complex operations.
--
class Expression arch where
  eforeign      :: (Foreign f, Elt x, Elt y)
                => arch
                -> f x y
                -> IRFun1    arch ()   (x -> y)
                -> IROpenExp arch env aenv x
                -> IROpenExp arch env aenv y
  eforeign = $internalError "eforeign" "default instance not implemented yet"

  while         :: Elt a
                => IROpenFun1 arch env aenv (a -> Bool)
                -> IROpenFun1 arch env aenv (a -> a)
                -> IROpenExp  arch env aenv a
                -> IROpenExp  arch env aenv a
  while = $internalError "while" "default instance not implemented yet"


-- Scalar expressions
-- ==================

llvmOfFun1
    :: (Skeleton arch, Expression arch)
    => arch
    -> DelayedFun aenv (a -> b)
    -> Aval aenv
    -> IRFun1 arch aenv (a -> b)
llvmOfFun1 arch (Lam (Body body)) aenv = IRFun1 $ \x -> llvmOfOpenExp arch body (Empty `Push` x) aenv
llvmOfFun1 _ _ _                       = $internalError "llvmOfFun1" "impossible evaluation"

llvmOfFun2
    :: (Skeleton arch, Expression arch)
    => arch
    -> DelayedFun aenv (a -> b -> c)
    -> Aval aenv
    -> IRFun2 arch aenv (a -> b -> c)
llvmOfFun2 arch (Lam (Lam (Body body))) aenv = IRFun2 $ \x y -> llvmOfOpenExp arch body (Empty `Push` x `Push` y) aenv
llvmOfFun2 _ _ _                             = $internalError "llvmOfFun2" "impossible evaluation"


-- | Convert an open scalar expression into a sequence of LLVM IR instructions.
-- Code is generated in depth first order, and uses a monad to collect the
-- sequence of instructions used to construct basic blocks.
--
llvmOfOpenExp
    :: forall arch env aenv _t. (Skeleton arch, Expression arch)
    => arch
    -> DelayedOpenExp env aenv _t
    -> Val env
    -> Aval aenv
    -> IROpenExp arch env aenv _t
llvmOfOpenExp arch top env aenv = cvtE top
  where
    cvtE :: forall t. DelayedOpenExp env aenv t -> IROpenExp arch env aenv t
    cvtE exp =
      case exp of
        Let bnd body            -> do x <- cvtE bnd
                                      llvmOfOpenExp arch body (env `Push` x) aenv
        Var ix                  -> return $ prj ix env
        Const c                 -> return $ IR (constant (eltType (undefined::t)) c)
        PrimConst c             -> return $ IR (constant (eltType (undefined::t)) (fromElt (primConst c)))
        PrimApp f x             -> llvmOfPrimFun f =<< cvtE x
        IndexNil                -> return indexNil
        IndexAny                -> return indexAny
        IndexCons sh sz         -> indexCons <$> cvtE sh <*> cvtE sz
        IndexHead ix            -> indexHead <$> cvtE ix
        IndexTail ix            -> indexTail <$> cvtE ix
        Prj ix tup              -> prjT ix <$> cvtE tup
        Tuple tup               -> cvtT tup
        Foreign asm native x    -> eforeign arch asm (llvmOfFun1 arch native Empty) (cvtE x)

        IndexSlice _slice _slix _sh     -> error "IndexSlice"
        IndexFull _slice _slix _sh      -> error "IndexFull"
        ToIndex _sh _ix                 -> error "ToIndex"
        FromIndex _sh _ix               -> error "FromIndex"
        Cond _c _t _e                   -> error "Cond"
        While _c _f _x                  -> error "While"
        Index _acc _ix                  -> error "Index"
        LinearIndex _acc _ix            -> error "LinearIndex"
        Shape _acc                      -> error "Shape"
        ShapeSize _sh                   -> error "ShapeSize"
        Intersect _sh1 _sh2             -> error "Intersect"
        Union _sh1 _sh2                 -> error "Union"

    indexNil :: IR Z
    indexNil = IR (constant (eltType Z) (fromElt Z))

    indexAny :: forall sh. Shape sh => IR (Any sh)
    indexAny = let any = Any :: Any sh
               in  IR (constant (eltType any) (fromElt any))

    indexHead :: IR (sh :. sz) -> IR sz
    indexHead (IR (OP_Pair _ sz)) = IR sz

    indexCons :: IR sh -> IR sz -> IR (sh :. sz)
    indexCons (IR sh) (IR sz) = IR (OP_Pair sh sz)

    indexTail :: IR (sh :. sz) -> IR sh
    indexTail (IR (OP_Pair sh _)) = IR sh

    prjT :: forall t e. (Elt t, Elt e) => TupleIdx (TupleRepr t) e -> IR t -> IR e
    prjT tix (IR ops) = IR $ go tix (eltType (undefined::t)) ops
      where
        go :: TupleIdx v e -> TupleType t' -> Operands t' -> Operands (EltRepr e)
        go ZeroTupIdx (PairTuple _ t) (OP_Pair _ v)
          | Just REFL <- matchTupleType t (eltType (undefined :: e))
          = v
        go (SuccTupIdx ix) (PairTuple t _) (OP_Pair tup _)      = go ix t tup
        go _ _ _                                                = $internalError "prjT" "inconsistent valuation"

    cvtT :: forall t. (Elt t, IsTuple t) => Tuple (DelayedOpenExp env aenv) (TupleRepr t) -> CodeGen (IR t)
    cvtT tup = IR <$> go (eltType (undefined::t)) tup
      where
        go :: TupleType t' -> Tuple (DelayedOpenExp env aenv) tup -> CodeGen (Operands t')
        go UnitTuple NilTup
          = return OP_Unit

        go (PairTuple ta tb) (SnocTup a (b :: DelayedOpenExp env aenv b))
          | Just REFL <- matchTupleType tb (eltType (undefined::b))
          = do a'    <- go ta a
               IR b' <- cvtE b
               return $ OP_Pair a' b'

        go _ _ = $internalError "cvtT" "impossible evaluation"


-- Primitive functions
-- ===================

fst :: IR (a, b) -> IR a
fst (IR (OP_Pair (OP_Pair OP_Unit x) _)) = IR x

snd :: IR (a, b) -> IR b
snd (IR (OP_Pair _ y)) = IR y

unpair :: IR (a, b) -> (IR a, IR b)
unpair x = (fst x, snd x)

uncurry :: (IR a -> IR b -> c) -> IR (a, b) -> c
uncurry f (unpair -> (x,y)) = f x y


-- | Generate llvm operations for primitive scalar functions
--
llvmOfPrimFun :: (Elt a, Elt r) => PrimFun (a -> r) -> IR a -> CodeGen (IR r)
llvmOfPrimFun (PrimAdd t)               = uncurry (A.add t)
llvmOfPrimFun (PrimSub t)               = uncurry (A.sub t)
llvmOfPrimFun (PrimMul t)               = uncurry (A.mul t)
llvmOfPrimFun (PrimNeg t)               = A.negate t
llvmOfPrimFun (PrimAbs t)               = A.abs t
llvmOfPrimFun (PrimSig t)               = A.signum t
llvmOfPrimFun (PrimQuot t)              = uncurry (A.quot t)
llvmOfPrimFun (PrimRem t)               = uncurry (A.rem t)
llvmOfPrimFun (PrimQuotRem t)           = uncurry (A.quotRem t)
llvmOfPrimFun (PrimIDiv t)              = uncurry (A.idiv t)
llvmOfPrimFun (PrimMod t)               = uncurry (A.mod t)
llvmOfPrimFun (PrimDivMod t)            = uncurry (A.divMod t)
llvmOfPrimFun (PrimBAnd t)              = uncurry (A.band t)
llvmOfPrimFun (PrimBOr t)               = uncurry (A.bor t)
llvmOfPrimFun (PrimBXor t)              = uncurry (A.xor t)
llvmOfPrimFun (PrimBNot t)              = A.complement t
llvmOfPrimFun (PrimBShiftL t)           = uncurry (A.shiftL t)
llvmOfPrimFun (PrimBShiftR t)           = uncurry (A.shiftR t)
llvmOfPrimFun (PrimBRotateL t)          = uncurry (A.rotateL t)
llvmOfPrimFun (PrimBRotateR t)          = uncurry (A.rotateR t)
llvmOfPrimFun (PrimFDiv t)              = uncurry (A.fdiv t)
llvmOfPrimFun (PrimRecip t)             = A.recip t
llvmOfPrimFun (PrimSin t)               = A.sin t
llvmOfPrimFun (PrimCos t)               = A.cos t
llvmOfPrimFun (PrimTan t)               = A.tan t
llvmOfPrimFun (PrimAsin t)              = A.asin t
llvmOfPrimFun (PrimAcos t)              = A.acos t
llvmOfPrimFun (PrimAtan t)              = A.atan t
llvmOfPrimFun (PrimAsinh t)             = A.asinh t
llvmOfPrimFun (PrimAcosh t)             = A.acosh t
llvmOfPrimFun (PrimAtanh t)             = A.atanh t
llvmOfPrimFun (PrimAtan2 t)             = uncurry (A.atan2 t)
llvmOfPrimFun (PrimExpFloating t)       = A.exp t
llvmOfPrimFun (PrimFPow t)              = uncurry (A.fpow t)
llvmOfPrimFun (PrimSqrt t)              = A.sqrt t
llvmOfPrimFun (PrimLog t)               = A.log t
llvmOfPrimFun (PrimLogBase t)           = uncurry (A.logBase t)
llvmOfPrimFun (PrimTruncate ta tb)      = A.truncate ta tb
llvmOfPrimFun (PrimRound ta tb)         = A.round ta tb
llvmOfPrimFun (PrimFloor ta tb)         = A.floor ta tb
llvmOfPrimFun (PrimCeiling ta tb)       = A.ceiling ta tb
llvmOfPrimFun (PrimIsNaN t)             = A.isNaN t
llvmOfPrimFun (PrimLt t)                = uncurry (A.lt t)
llvmOfPrimFun (PrimGt t)                = uncurry (A.gt t)
llvmOfPrimFun (PrimLtEq t)              = uncurry (A.lte t)
llvmOfPrimFun (PrimGtEq t)              = uncurry (A.gte t)
llvmOfPrimFun (PrimEq t)                = uncurry (A.eq t)
llvmOfPrimFun (PrimNEq t)               = uncurry (A.neq t)
llvmOfPrimFun (PrimMax t)               = uncurry (A.max t)
llvmOfPrimFun (PrimMin t)               = uncurry (A.min t)
llvmOfPrimFun PrimLAnd                  = uncurry A.land
llvmOfPrimFun PrimLOr                   = uncurry A.lor
llvmOfPrimFun PrimLNot                  = A.lnot
llvmOfPrimFun PrimOrd                   = A.ord
llvmOfPrimFun PrimChr                   = A.chr
llvmOfPrimFun PrimBoolToInt             = A.boolToInt
llvmOfPrimFun (PrimFromIntegral ta tb)  = A.fromIntegral ta tb
  -- no missing patterns, whoo!

