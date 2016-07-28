{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Exp
-- Copyright   : [2015..2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Exp
  where

import Control.Applicative                                          hiding ( Const )
import Control.Monad
import Prelude                                                      hiding ( exp, any )
import qualified Data.IntMap                                        as IM

import Data.Array.Accelerate.AST                                    hiding ( Val(..), prj )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                            hiding ( toTuple, shape, intersect, union )
import Data.Array.Accelerate.Array.Representation                   ( SliceIndex(..) )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad                     ( CodeGen )
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import qualified Data.Array.Accelerate.LLVM.CodeGen.Loop            as L
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic      as A


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
  while p f x =
    L.while (app1 p) (app1 f) =<< x


-- Scalar expressions
-- ==================

{-# INLINEABLE llvmOfFun1 #-}
llvmOfFun1
    :: Expression arch
    => arch
    -> DelayedFun aenv (a -> b)
    -> Gamma aenv
    -> IRFun1 arch aenv (a -> b)
llvmOfFun1 arch (Lam (Body body)) aenv = IRFun1 $ \x -> llvmOfOpenExp arch body (Empty `Push` x) aenv
llvmOfFun1 _ _ _                       = $internalError "llvmOfFun1" "impossible evaluation"

{-# INLINEABLE llvmOfFun2 #-}
llvmOfFun2
    :: Expression arch
    => arch
    -> DelayedFun aenv (a -> b -> c)
    -> Gamma aenv
    -> IRFun2 arch aenv (a -> b -> c)
llvmOfFun2 arch (Lam (Lam (Body body))) aenv = IRFun2 $ \x y -> llvmOfOpenExp arch body (Empty `Push` x `Push` y) aenv
llvmOfFun2 _ _ _                             = $internalError "llvmOfFun2" "impossible evaluation"


-- | Convert an open scalar expression into a sequence of LLVM IR instructions.
-- Code is generated in depth first order, and uses a monad to collect the
-- sequence of instructions used to construct basic blocks.
--
{-# INLINEABLE llvmOfOpenExp #-}
llvmOfOpenExp
    :: forall arch env aenv _t. Expression arch
    => arch
    -> DelayedOpenExp env aenv _t
    -> Val env
    -> Gamma aenv
    -> IROpenExp arch env aenv _t
llvmOfOpenExp arch top env aenv = cvtE top
  where
    cvtM :: DelayedOpenAcc aenv (Array sh e) -> IRManifest arch aenv (Array sh e)
    cvtM (Manifest (Avar ix)) = IRManifest ix
    cvtM _                    = $internalError "llvmOfOpenExp" "expected manifest array variable"

    cvtF1 :: DelayedOpenFun env aenv (a -> b) -> IROpenFun1 arch env aenv (a -> b)
    cvtF1 (Lam (Body body)) = IRFun1 $ \x -> llvmOfOpenExp arch body (env `Push` x) aenv
    cvtF1 _                 = $internalError "cvtF1" "impossible evaluation"

    cvtE :: forall t. DelayedOpenExp env aenv t -> IROpenExp arch env aenv t
    cvtE exp =
      case exp of
        Let bnd body                -> do x <- cvtE bnd
                                          llvmOfOpenExp arch body (env `Push` x) aenv
        Var ix                      -> return $ prj ix env
        Const c                     -> return $ IR (constant (eltType (undefined::t)) c)
        PrimConst c                 -> return $ IR (constant (eltType (undefined::t)) (fromElt (primConst c)))
        PrimApp f x                 -> llvmOfPrimFun f =<< cvtE x
        IndexNil                    -> return indexNil
        IndexAny                    -> return indexAny
        IndexCons sh sz             -> indexCons <$> cvtE sh <*> cvtE sz
        IndexHead ix                -> indexHead <$> cvtE ix
        IndexTail ix                -> indexTail <$> cvtE ix
        Prj ix tup                  -> prjT ix <$> cvtE tup
        Tuple tup                   -> cvtT tup
        Foreign asm native x        -> eforeign arch asm (llvmOfFun1 arch native IM.empty) (cvtE x)
        Cond c t e                  -> A.ifThenElse (cvtE c) (cvtE t) (cvtE e)
        IndexSlice slice slix sh    -> indexSlice slice <$> cvtE slix <*> cvtE sh
        IndexFull slice slix sh     -> indexFull slice  <$> cvtE slix <*> cvtE sh
        ToIndex sh ix               -> join $ intOfIndex <$> cvtE sh <*> cvtE ix
        FromIndex sh ix             -> join $ indexOfInt <$> cvtE sh <*> cvtE ix
        Index acc ix                -> index (cvtM acc)       =<< cvtE ix
        LinearIndex acc ix          -> linearIndex (cvtM acc) =<< cvtE ix
        ShapeSize sh                -> shapeSize              =<< cvtE sh
        Shape acc                   -> return $ shape (cvtM acc)
        Intersect sh1 sh2           -> join $ intersect <$> cvtE sh1 <*> cvtE sh2
        Union sh1 sh2               -> join $ union     <$> cvtE sh1 <*> cvtE sh2
        While c f x                 -> while (cvtF1 c) (cvtF1 f) (cvtE x)

    indexNil :: IR Z
    indexNil = IR (constant (eltType Z) (fromElt Z))

    indexAny :: forall sh. Shape sh => IR (Any sh)
    indexAny = let any = Any :: Any sh
               in  IR (constant (eltType any) (fromElt any))

    indexSlice :: (Shape sh, Shape sl, Elt slix)
               => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
               -> IR slix
               -> IR sh
               -> IR sl
    indexSlice slice (IR slix) (IR sh) = IR $ restrict slice slix sh
      where
        restrict :: SliceIndex slix sl co sh -> Operands slix -> Operands sh -> Operands sl
        restrict SliceNil              OP_Unit               OP_Unit          = OP_Unit
        restrict (SliceAll sliceIdx)   (OP_Pair slx OP_Unit) (OP_Pair sl sz)  =
          let sl' = restrict sliceIdx slx sl
          in  OP_Pair sl' sz
        restrict (SliceFixed sliceIdx) (OP_Pair slx _i)      (OP_Pair sl _sz) =
          restrict sliceIdx slx sl

    indexFull :: (Shape sh, Shape sl, Elt slix)
              => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
              -> IR slix
              -> IR sl
              -> IR sh
    indexFull slice (IR slix) (IR sh) = IR $ extend slice slix sh
      where
        extend :: SliceIndex slix sl co sh -> Operands slix -> Operands sl -> Operands sh
        extend SliceNil              OP_Unit               OP_Unit         = OP_Unit
        extend (SliceAll sliceIdx)   (OP_Pair slx OP_Unit) (OP_Pair sl sz) =
          let sh' = extend sliceIdx slx sl
          in  OP_Pair sh' sz
        extend (SliceFixed sliceIdx) (OP_Pair slx sz) sl                   =
          let sh' = extend sliceIdx slx sl
          in  OP_Pair sh' sz

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
          -- We must assert that the reified type 'tb' of 'b' is actually
          -- equivalent to the type of 'b'. This can not fail, but is necessary
          -- because 'tb' observes the representation type of surface type 'b'.
          | Just REFL <- matchTupleType tb (eltType (undefined::b))
          = do a'    <- go ta a
               IR b' <- cvtE b
               return $ OP_Pair a' b'
        go _ _ = $internalError "cvtT" "impossible evaluation"

    linearIndex :: (Shape sh, Elt e) => IRManifest arch aenv (Array sh e) -> IR Int -> CodeGen (IR e)
    linearIndex (IRManifest v) ix =
      readArray (irArray (aprj v aenv)) ix

    index :: (Shape sh, Elt e) => IRManifest arch aenv (Array sh e) -> IR sh -> CodeGen (IR e)
    index (IRManifest v) ix =
      let arr = irArray (aprj v aenv)
      in  readArray arr =<< intOfIndex (irArrayShape arr) ix

    shape :: (Shape sh, Elt e) => IRManifest arch aenv (Array sh e) -> IR sh
    shape (IRManifest v) = irArrayShape (irArray (aprj v aenv))

    shapeSize :: forall sh. Shape sh => IR sh -> CodeGen (IR Int)
    shapeSize (IR extent) = go (eltType (undefined::sh)) extent
      where
        go :: TupleType t -> Operands t -> CodeGen (IR Int)
        go UnitTuple OP_Unit
          = return $ IR (constant (eltType (undefined :: Int)) 1)
        go (PairTuple tsh t) (OP_Pair sh sz)
          | Just REFL <- matchTupleType t (eltType (undefined::Int))
          = do
               a <- go tsh sh
               b <- A.mul numType a (IR sz)
               return b
        go (SingleTuple t) (op' t -> i)
          | Just REFL <- matchScalarType t (scalarType :: ScalarType Int)
          = return $ ir t i
        go _ _
          = $internalError "shapeSize" "expected shape with Int components"

    intersect :: forall sh. Shape sh => IR sh -> IR sh -> CodeGen (IR sh)
    intersect (IR extent1) (IR extent2) = IR <$> go (eltType (undefined::sh)) extent1 extent2
      where
        go :: TupleType t -> Operands t -> Operands t -> CodeGen (Operands t)
        go UnitTuple OP_Unit OP_Unit
          = return OP_Unit
        go (SingleTuple t) sh1 sh2
          | Just REFL <- matchScalarType t (scalarType :: ScalarType Int)       -- TLM: GHC hang if this is omitted
          = do IR x <- A.min t (IR sh1) (IR sh2)
               return x
        go (PairTuple tsh tsz) (OP_Pair sh1 sz1) (OP_Pair sh2 sz2)
          = do
               sz' <- go tsz sz1 sz2
               sh' <- go tsh sh1 sh2
               return $ OP_Pair sh' sz'
        go _ _ _
          = $internalError "intersect" "expected shape with Int components"

    union :: forall sh. Shape sh => IR sh -> IR sh -> CodeGen (IR sh)
    union (IR extent1) (IR extent2) = IR <$> go (eltType (undefined::sh)) extent1 extent2
      where
        go :: TupleType t -> Operands t -> Operands t -> CodeGen (Operands t)
        go UnitTuple OP_Unit OP_Unit
          = return OP_Unit
        go (SingleTuple t) sh1 sh2
          | Just REFL <- matchScalarType t (scalarType :: ScalarType Int)       -- TLM: GHC hang if this is omitted
          = do IR x <- A.max t (IR sh1) (IR sh2)
               return x
        go (PairTuple tsh tsz) (OP_Pair sh1 sz1) (OP_Pair sh2 sz2)
          = do
               sz' <- go tsz sz1 sz2
               sh' <- go tsh sh1 sh2
               return $ OP_Pair sh' sz'
        go _ _ _
          = $internalError "union" "expected shape with Int components"


-- | Extract the head of an index
--
indexHead :: IR (sh :. sz) -> IR sz
indexHead (IR (OP_Pair _ sz)) = IR sz

-- | Extract the tail of an index
--
indexTail :: IR (sh :. sz) -> IR sh
indexTail (IR (OP_Pair sh _)) = IR sh

-- | Construct an index from the head and tail
--
indexCons :: IR sh -> IR sz -> IR (sh :. sz)
indexCons (IR sh) (IR sz) = IR (OP_Pair sh sz)


-- | Convert a multidimensional array index into a linear index
--
intOfIndex :: forall sh. Shape sh => IR sh -> IR sh -> CodeGen (IR Int)
intOfIndex (IR extent) (IR index) = cvt (eltType (undefined::sh)) extent index
  where
    cvt :: TupleType t -> Operands t -> Operands t -> CodeGen (IR Int)
    cvt UnitTuple OP_Unit OP_Unit
      = return $ IR (constant (eltType (undefined :: Int)) 0)

    cvt (PairTuple tsh t) (OP_Pair sh sz) (OP_Pair ix i)
      | Just REFL <- matchTupleType t (eltType (undefined::Int))
      -- If we short-circuit the last dimension, we can avoid inserting
      -- a multiply by zero and add of the result.
      = case matchTupleType tsh (eltType (undefined::Z)) of
          Just REFL -> return (IR i)
          Nothing   -> do
            a <- cvt tsh sh ix
            b <- A.mul numType a (IR sz)
            c <- A.add numType b (IR i)
            return c

    cvt (SingleTuple t) _ (op' t -> i)
      | Just REFL <- matchScalarType t (scalarType :: ScalarType Int)
      = return $ ir t i

    cvt _ _ _
      = $internalError "intOfIndex" "expected shape with Int components"


-- | Convert a linear index into into a multidimensional index
--
indexOfInt :: forall sh. Shape sh => IR sh -> IR Int -> CodeGen (IR sh)
indexOfInt (IR extent) index = IR <$> cvt (eltType (undefined::sh)) extent index
  where
    cvt :: TupleType t -> Operands t -> IR Int -> CodeGen (Operands t)
    cvt UnitTuple OP_Unit _
      = return OP_Unit

    cvt (PairTuple tsh tsz) (OP_Pair sh sz) i
      | Just REFL <- matchTupleType tsz (eltType (undefined::Int))
      = do
           i'    <- A.quot integralType i (IR sz)
           -- If we assume the index is in range, there is no point computing
           -- the remainder of the highest dimension since (i < sz) must hold
           IR r  <- case matchTupleType tsh (eltType (undefined::Z)) of
                      Just REFL -> return i     -- TODO: in debug mode assert (i < sz)
                      Nothing   -> A.rem  integralType i (IR sz)
           sh'   <- cvt tsh sh i'
           return $ OP_Pair sh' r

    cvt (SingleTuple t) _ (IR i)
      | Just REFL <- matchScalarType t (scalarType :: ScalarType Int)
      = return i

    cvt _ _ _
      = $internalError "indexOfInt" "expected shape with Int components"


-- Primitive functions
-- ===================

-- | Generate llvm operations for primitive scalar functions
--
llvmOfPrimFun :: (Elt a, Elt r) => PrimFun (a -> r) -> IR a -> CodeGen (IR r)
llvmOfPrimFun (PrimAdd t)               = A.uncurry (A.add t)
llvmOfPrimFun (PrimSub t)               = A.uncurry (A.sub t)
llvmOfPrimFun (PrimMul t)               = A.uncurry (A.mul t)
llvmOfPrimFun (PrimNeg t)               = A.negate t
llvmOfPrimFun (PrimAbs t)               = A.abs t
llvmOfPrimFun (PrimSig t)               = A.signum t
llvmOfPrimFun (PrimQuot t)              = A.uncurry (A.quot t)
llvmOfPrimFun (PrimRem t)               = A.uncurry (A.rem t)
llvmOfPrimFun (PrimQuotRem t)           = A.uncurry (A.quotRem t)
llvmOfPrimFun (PrimIDiv t)              = A.uncurry (A.idiv t)
llvmOfPrimFun (PrimMod t)               = A.uncurry (A.mod t)
llvmOfPrimFun (PrimDivMod t)            = A.uncurry (A.divMod t)
llvmOfPrimFun (PrimBAnd t)              = A.uncurry (A.band t)
llvmOfPrimFun (PrimBOr t)               = A.uncurry (A.bor t)
llvmOfPrimFun (PrimBXor t)              = A.uncurry (A.xor t)
llvmOfPrimFun (PrimBNot t)              = A.complement t
llvmOfPrimFun (PrimBShiftL t)           = A.uncurry (A.shiftL t)
llvmOfPrimFun (PrimBShiftR t)           = A.uncurry (A.shiftR t)
llvmOfPrimFun (PrimBRotateL t)          = A.uncurry (A.rotateL t)
llvmOfPrimFun (PrimBRotateR t)          = A.uncurry (A.rotateR t)
llvmOfPrimFun (PrimFDiv t)              = A.uncurry (A.fdiv t)
llvmOfPrimFun (PrimRecip t)             = A.recip t
llvmOfPrimFun (PrimSin t)               = A.sin t
llvmOfPrimFun (PrimCos t)               = A.cos t
llvmOfPrimFun (PrimTan t)               = A.tan t
llvmOfPrimFun (PrimSinh t)              = A.sinh t
llvmOfPrimFun (PrimCosh t)              = A.cosh t
llvmOfPrimFun (PrimTanh t)              = A.tanh t
llvmOfPrimFun (PrimAsin t)              = A.asin t
llvmOfPrimFun (PrimAcos t)              = A.acos t
llvmOfPrimFun (PrimAtan t)              = A.atan t
llvmOfPrimFun (PrimAsinh t)             = A.asinh t
llvmOfPrimFun (PrimAcosh t)             = A.acosh t
llvmOfPrimFun (PrimAtanh t)             = A.atanh t
llvmOfPrimFun (PrimAtan2 t)             = A.uncurry (A.atan2 t)
llvmOfPrimFun (PrimExpFloating t)       = A.exp t
llvmOfPrimFun (PrimFPow t)              = A.uncurry (A.fpow t)
llvmOfPrimFun (PrimSqrt t)              = A.sqrt t
llvmOfPrimFun (PrimLog t)               = A.log t
llvmOfPrimFun (PrimLogBase t)           = A.uncurry (A.logBase t)
llvmOfPrimFun (PrimTruncate ta tb)      = A.truncate ta tb
llvmOfPrimFun (PrimRound ta tb)         = A.round ta tb
llvmOfPrimFun (PrimFloor ta tb)         = A.floor ta tb
llvmOfPrimFun (PrimCeiling ta tb)       = A.ceiling ta tb
llvmOfPrimFun (PrimIsNaN t)             = A.isNaN t
llvmOfPrimFun (PrimLt t)                = A.uncurry (A.lt t)
llvmOfPrimFun (PrimGt t)                = A.uncurry (A.gt t)
llvmOfPrimFun (PrimLtEq t)              = A.uncurry (A.lte t)
llvmOfPrimFun (PrimGtEq t)              = A.uncurry (A.gte t)
llvmOfPrimFun (PrimEq t)                = A.uncurry (A.eq t)
llvmOfPrimFun (PrimNEq t)               = A.uncurry (A.neq t)
llvmOfPrimFun (PrimMax t)               = A.uncurry (A.max t)
llvmOfPrimFun (PrimMin t)               = A.uncurry (A.min t)
llvmOfPrimFun PrimLAnd                  = A.uncurry A.land'     -- TLM: wrong!
llvmOfPrimFun PrimLOr                   = A.uncurry A.lor'      -- TLM: wrong!
llvmOfPrimFun PrimLNot                  = A.lnot
llvmOfPrimFun PrimOrd                   = A.ord
llvmOfPrimFun PrimChr                   = A.chr
llvmOfPrimFun PrimBoolToInt             = A.boolToInt
llvmOfPrimFun (PrimFromIntegral ta tb)  = A.fromIntegral ta tb
llvmOfPrimFun (PrimToFloating ta tb)    = A.toFloating ta tb
llvmOfPrimFun (PrimCoerce ta tb)        = A.coerce ta tb
  -- no missing patterns, whoo!

