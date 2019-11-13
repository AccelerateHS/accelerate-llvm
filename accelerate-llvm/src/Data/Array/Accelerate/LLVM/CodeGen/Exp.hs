{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Exp
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Exp
  where

import Control.Applicative                                          hiding ( Const )
import Control.Monad
import Data.Typeable
import Text.Printf
import Prelude                                                      hiding ( exp, any )
import qualified Data.IntMap                                        as IM

import Data.Array.Accelerate.AST                                    hiding ( Val(..), prj )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                            hiding ( Foreign, toTuple, shape, intersect, union )
import Data.Array.Accelerate.Array.Representation                   ( SliceIndex(..) )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Sugar                  as A

import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Operand

import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Foreign
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic      as A
import qualified Data.Array.Accelerate.LLVM.CodeGen.Loop            as L


-- Scalar expressions
-- ==================

{-# INLINEABLE llvmOfFun1 #-}
llvmOfFun1
    :: Foreign arch
    => DelayedFun aenv (a -> b)
    -> Gamma aenv
    -> IRFun1 arch aenv (a -> b)
llvmOfFun1 (Lam (Body body)) aenv = IRFun1 $ \x -> llvmOfOpenExp body (Empty `Push` x) aenv
llvmOfFun1  _ _                   = $internalError "llvmOfFun1" "impossible evaluation"

{-# INLINEABLE llvmOfFun2 #-}
llvmOfFun2
    :: Foreign arch
    => DelayedFun aenv (a -> b -> c)
    -> Gamma aenv
    -> IRFun2 arch aenv (a -> b -> c)
llvmOfFun2 (Lam (Lam (Body body))) aenv = IRFun2 $ \x y -> llvmOfOpenExp body (Empty `Push` x `Push` y) aenv
llvmOfFun2 _ _                          = $internalError "llvmOfFun2" "impossible evaluation"


-- | Convert an open scalar expression into a sequence of LLVM IR instructions.
-- Code is generated in depth first order, and uses a monad to collect the
-- sequence of instructions used to construct basic blocks.
--
{-# INLINEABLE llvmOfOpenExp #-}
llvmOfOpenExp
    :: forall arch env aenv _t. Foreign arch
    => DelayedOpenExp env aenv _t
    -> Val env
    -> Gamma aenv
    -> IROpenExp arch env aenv _t
llvmOfOpenExp top env aenv = cvtE top
  where
    cvtM :: DelayedOpenAcc aenv (Array sh e) -> ArrayVar aenv (Array sh e)
    cvtM (Manifest (Avar ix)) = ix
    cvtM _                    = $internalError "llvmOfOpenExp" "expected manifest array variable"

    cvtF1 :: DelayedOpenFun env aenv (a -> b) -> IROpenFun1 arch env aenv (a -> b)
    cvtF1 (Lam (Body body)) = IRFun1 $ \x -> llvmOfOpenExp body (env `Push` x) aenv
    cvtF1 _                 = $internalError "cvtF1" "impossible evaluation"

    cvtE :: forall t. DelayedOpenExp env aenv t -> IROpenExp arch env aenv t
    cvtE exp =
      case exp of
        Let bnd body                -> do x <- cvtE bnd
                                          llvmOfOpenExp body (env `Push` x) aenv
        Var ix                      -> return $ prj ix env
        Const c                     -> return $ IR (constant (eltType @t) c)
        PrimConst c                 -> return $ IR (constant (eltType @t) (fromElt (primConst c)))
        PrimApp f x                 -> primFun f x
        Undef                       -> return undefE
        IndexNil                    -> return indexNil
        IndexAny                    -> return indexAny
        IndexCons sh sz             -> indexCons <$> cvtE sh <*> cvtE sz
        IndexHead ix                -> indexHead <$> cvtE ix
        IndexTail ix                -> indexTail <$> cvtE ix
        Prj ix tup                  -> prjT ix =<< cvtE tup
        Tuple tup                   -> cvtT tup
        Foreign asm f x             -> foreignE asm f =<< cvtE x
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
        Coerce x                    -> coerce =<< cvtE x

    indexNil :: IR Z
    indexNil = A.lift Z

    indexAny :: forall sh. Shape sh => IR (Any sh)
    indexAny = A.lift Any

    undefE :: forall t. Elt t => IR t
    undefE = IR $ go (eltType @t)
      where
        go :: TupleType s -> Operands s
        go TypeRunit       = OP_Unit
        go (TypeRscalar t) = ir' t (undef t)
        go (TypeRpair a b) = OP_Pair (go a) (go b)

    indexSlice :: SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
               -> IR slix
               -> IR sh
               -> IR sl
    indexSlice slice (IR slix) (IR sh) = IR $ restrict slice slix sh
      where
        restrict :: SliceIndex slix sl co sh -> Operands slix -> Operands sh -> Operands sl
        restrict SliceNil              OP_Unit               OP_Unit          = OP_Unit
        restrict (SliceAll sliceIdx)   (OP_Pair slx OP_Unit) (OP_Pair sl sz)  =
          let sl' = restrict sliceIdx slx sl
           in OP_Pair sl' sz
        restrict (SliceFixed sliceIdx) (OP_Pair slx _i)      (OP_Pair sl _sz) =
          restrict sliceIdx slx sl

    indexFull :: SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
              -> IR slix
              -> IR sl
              -> IR sh
    indexFull slice (IR slix) (IR sh) = IR $ extend slice slix sh
      where
        extend :: SliceIndex slix sl co sh -> Operands slix -> Operands sl -> Operands sh
        extend SliceNil              OP_Unit               OP_Unit         = OP_Unit
        extend (SliceAll sliceIdx)   (OP_Pair slx OP_Unit) (OP_Pair sl sz) =
          let sh' = extend sliceIdx slx sl
           in OP_Pair sh' sz
        extend (SliceFixed sliceIdx) (OP_Pair slx sz)      sl              =
          let sh' = extend sliceIdx slx sl
           in OP_Pair sh' sz

    prjT :: forall t e. (Elt t, IsTuple t, Elt e) => TupleIdx (TupleRepr t) e -> IR t -> IROpenExp arch env aenv e
    prjT tix (IR tup) =
      case eltType @t of
        TypeRscalar (VectorScalarType v) -> goV     v tup
        t                                -> goT tix t tup
      where
        -- for unzipped tuples
        goT :: TupleIdx s e -> TupleType t' -> Operands t' -> CodeGen arch (IR e)
        goT (SuccTupIdx ix) (TypeRpair t _) (OP_Pair x _) = goT ix t x
        goT ZeroTupIdx      (TypeRpair _ t) (OP_Pair _ x)
          | Just Refl <- matchTupleType t (eltType @e)
          = return $ IR x
        goT _ _ _
          = $internalError "prjT/tup" "inconsistent valuation"

        -- for SIMD vectors
        goV :: forall n a. VectorType (Vec n a) -> Operands (Vec n a) -> CodeGen arch (IR e)
        goV (VectorType n a) (OP_Vec v)
          | Just Refl <- matchVecT (eltType @e) (TypeRscalar (SingleScalarType a))
          = instr $ ExtractElement (fromIntegral (n - tupleIdxToInt tix - 1)) v
        goV _ _
          = $internalError "prjT/vec" "inconsistent valuation"

        matchVecT :: TupleType (EltRepr e) -> TupleType a -> Maybe (e :~: a)
        matchVecT e v
          | Just Refl <- matchTupleType e v = gcast Refl
          | otherwise                       = Nothing

    cvtT :: forall t. (Elt t, IsTuple t) => Tuple (DelayedOpenExp env aenv) (TupleRepr t) -> IROpenExp arch env aenv t
    cvtT tup =
      case eltType @t of
        TypeRscalar (VectorScalarType v) -> IR <$> goV v tup
        t                                -> IR <$> goT t tup
      where
        -- for unzipped tuples
        goT :: TupleType t' -> Tuple (DelayedOpenExp env aenv) tup -> CodeGen arch (Operands t')
        goT TypeRunit NilTup
          = return OP_Unit
        goT (TypeRpair ta tb) (SnocTup a (b :: DelayedOpenExp env aenv b))
          -- We must assert that the reified type 'tb' of 'b' is actually
          -- equivalent to the type of 'b'. This can not fail, but is necessary
          -- because 'tb' observes the representation type of surface type 'b'.
          | Just Refl <- matchTupleType tb (eltType @b)
          = do a'    <- goT ta a
               IR b' <- cvtE b
               return $ OP_Pair a' b'
        goT _ _
          = $internalError "cvtT/tup"
          $ unlines [ "impossible evaluation"
                    , "  possible solution: ensure that the 'EltRepr' and 'ProdRepr' instances of your data type are consistent." ]

        -- for packed SIMD vectors
        goV :: forall n a. VectorType (Vec n a) -> Tuple (DelayedOpenExp env aenv) (TupleRepr t) -> CodeGen arch (Operands (Vec n a))
        goV v@(VectorType _ a) ts = OP_Vec . snd <$> pack ts
          where
            pack :: Tuple (DelayedOpenExp env aenv) tup -> CodeGen arch (Int32, Operand (Vec n a))
            pack NilTup
              = return (0, undef (VectorScalarType v))
            pack (SnocTup t x)
              | Just Refl <- matchExpType x
              = do
                  x'        <- cvtE x
                  (i, vec)  <- pack t
                  vec'      <- instr' $ InsertElement i vec (op a x')
                  return (i+1, vec')
                where
                  matchExpType :: forall s. Elt s => DelayedOpenExp env aenv s -> Maybe (s :~: a)
                  matchExpType _
                    | Just Refl <- matchTupleType (eltType @s) (TypeRscalar (SingleScalarType a)) = gcast Refl
                    | otherwise                                                                   = Nothing
            pack _
              = $internalError "cvtT/vec" "impossible evaluation"


    linearIndex :: (Shape sh, Elt e) => ArrayVar aenv (Array sh e) -> IR Int -> IROpenExp arch env aenv e
    linearIndex (ArrayVar v) = linearIndexArray (irArray (aprj v aenv))

    index :: (Shape sh, Elt e) => ArrayVar aenv (Array sh e) -> IR sh -> IROpenExp arch env aenv e
    index (ArrayVar v) = indexArray (irArray (aprj v aenv))

    shape :: (Shape sh, Elt e) => ArrayVar aenv (Array sh e) -> IR sh
    shape (ArrayVar v) = irArrayShape (irArray (aprj v aenv))

    intersect :: forall sh. Shape sh => IR sh -> IR sh -> IROpenExp arch env aenv sh
    intersect (IR extent1) (IR extent2) = IR <$> go (eltType @sh) extent1 extent2
      where
        go :: TupleType t -> Operands t -> Operands t -> CodeGen arch (Operands t)
        go TypeRunit OP_Unit OP_Unit
          = return OP_Unit
        go (TypeRscalar t) sh1 sh2
          | Just Refl <- matchScalarType t (scalarType :: ScalarType Int)       -- TLM: GHC hang if this is omitted
          = do IR x <- A.min (singleType :: SingleType Int) (IR sh1) (IR sh2)
               return x
        go (TypeRpair tsh tsz) (OP_Pair sh1 sz1) (OP_Pair sh2 sz2)
          = do
               sz' <- go tsz sz1 sz2
               sh' <- go tsh sh1 sh2
               return $ OP_Pair sh' sz'
        go _ _ _
          = $internalError "intersect" "expected shape with Int components"

    union :: forall sh. Shape sh => IR sh -> IR sh -> IROpenExp arch env aenv sh
    union (IR extent1) (IR extent2) = IR <$> go (eltType @sh) extent1 extent2
      where
        go :: TupleType t -> Operands t -> Operands t -> CodeGen arch (Operands t)
        go TypeRunit OP_Unit OP_Unit
          = return OP_Unit
        go (TypeRscalar t) sh1 sh2
          | Just Refl <- matchScalarType t (scalarType :: ScalarType Int)       -- TLM: GHC hang if this is omitted
          = do IR x <- A.max (singleType :: SingleType Int) (IR sh1) (IR sh2)
               return x
        go (TypeRpair tsh tsz) (OP_Pair sh1 sz1) (OP_Pair sh2 sz2)
          = do
               sz' <- go tsz sz1 sz2
               sh' <- go tsh sh1 sh2
               return $ OP_Pair sh' sz'
        go _ _ _
          = $internalError "union" "expected shape with Int components"

    while :: Elt a
          => IROpenFun1 arch env aenv (a -> Bool)
          -> IROpenFun1 arch env aenv (a -> a)
          -> IROpenExp  arch env aenv a
          -> IROpenExp  arch env aenv a
    while p f x =
      L.while (app1 p) (app1 f) =<< x

    foreignE :: (Elt a, Elt b, Foreign arch, A.Foreign asm)
             => asm           (a -> b)
             -> DelayedFun () (a -> b)
             -> IR a
             -> IRExp arch () b
    foreignE asm no x =
      case foreignExp asm of
        Just f                       -> app1 f x
        Nothing | Lam (Body b) <- no -> llvmOfOpenExp b (Empty `Push` x) IM.empty
        _                            -> error "when a grid's misaligned with another behind / that's a moiré..."

    coerce :: forall a b. (Elt a, Elt b) => IR a -> IROpenExp arch env aenv b
    coerce (IR as) = IR <$> go (eltType @a) (eltType @b) as
      where
        go :: TupleType s -> TupleType t -> Operands s -> CodeGen arch (Operands t)
        go TypeRunit         TypeRunit         OP_Unit         = return OP_Unit
        go (TypeRpair s1 s2) (TypeRpair t1 t2) (OP_Pair x1 x2) = OP_Pair <$> go s1 t1 x1 <*> go s2 t2 x2
        go (TypeRscalar s)   (TypeRscalar t)   x
          | Just Refl <- matchScalarType s t = return x
          | otherwise                        = ir' t <$> instr' (BitCast t (op' s x))
        --
        go (TypeRpair TypeRunit s) t@TypeRscalar{} (OP_Pair OP_Unit x) = go s t x
        go s@TypeRscalar{} (TypeRpair TypeRunit t) x                   = OP_Pair OP_Unit <$> go s t x
        go _ _ _
          = error $ printf "could not coerce type `%s' to `%s'"
                      (show (typeOf (undefined::a)))
                      (show (typeOf (undefined::b)))

    primFun :: Elt r
            => PrimFun (a -> r)
            -> DelayedOpenExp env aenv a
            -> IROpenExp arch env aenv r
    primFun f x =
      let
          -- The Accelerate language and its code generator are hyper-strict.
          -- However, we must not eagerly evaluate the arguments to logical
          -- operations (&&*) and (||*) so that they can short-circuit. Since we
          -- only have unary functions, this is a little tricky for us.
          --
          -- 'inl' and 'inr' attempt to destruct the incoming AST so that we can
          -- evaluate the left or right components of a pair individually. It
          -- should be noted that there are other cases which can evaluate to
          -- pairs; 'Constant', 'Let' and 'Var', for example, but these cases
          -- are (probably) not applicable in this context.
          --
          inl :: (Elt a, Elt b) => DelayedOpenExp env aenv (a,b) -> IROpenExp arch env aenv a
          inl (Tuple (SnocTup (SnocTup NilTup a) _)) = cvtE a
          inl t                                      = cvtE $ Prj (SuccTupIdx ZeroTupIdx) t

          inr :: (Elt a, Elt b) => DelayedOpenExp env aenv (a,b) -> IROpenExp arch env aenv b
          inr (Tuple (SnocTup _ b)) = cvtE b
          inr t                     = cvtE $ Prj ZeroTupIdx t
      in
      case f of
        PrimAdd t                 -> A.uncurry (A.add t)     =<< cvtE x
        PrimSub t                 -> A.uncurry (A.sub t)     =<< cvtE x
        PrimMul t                 -> A.uncurry (A.mul t)     =<< cvtE x
        PrimNeg t                 -> A.negate t              =<< cvtE x
        PrimAbs t                 -> A.abs t                 =<< cvtE x
        PrimSig t                 -> A.signum t              =<< cvtE x
        PrimQuot t                -> A.uncurry (A.quot t)    =<< cvtE x
        PrimRem t                 -> A.uncurry (A.rem t)     =<< cvtE x
        PrimQuotRem t             -> A.uncurry (A.quotRem t) =<< cvtE x
        PrimIDiv t                -> A.uncurry (A.idiv t)    =<< cvtE x
        PrimMod t                 -> A.uncurry (A.mod t)     =<< cvtE x
        PrimDivMod t              -> A.uncurry (A.divMod t)  =<< cvtE x
        PrimBAnd t                -> A.uncurry (A.band t)    =<< cvtE x
        PrimBOr t                 -> A.uncurry (A.bor t)     =<< cvtE x
        PrimBXor t                -> A.uncurry (A.xor t)     =<< cvtE x
        PrimBNot t                -> A.complement t          =<< cvtE x
        PrimBShiftL t             -> A.uncurry (A.shiftL t)  =<< cvtE x
        PrimBShiftR t             -> A.uncurry (A.shiftR t)  =<< cvtE x
        PrimBRotateL t            -> A.uncurry (A.rotateL t) =<< cvtE x
        PrimBRotateR t            -> A.uncurry (A.rotateR t) =<< cvtE x
        PrimPopCount t            -> A.popCount t            =<< cvtE x
        PrimCountLeadingZeros t   -> A.countLeadingZeros t   =<< cvtE x
        PrimCountTrailingZeros t  -> A.countTrailingZeros t  =<< cvtE x
        PrimFDiv t                -> A.uncurry (A.fdiv t)    =<< cvtE x
        PrimRecip t               -> A.recip t               =<< cvtE x
        PrimSin t                 -> A.sin t                 =<< cvtE x
        PrimCos t                 -> A.cos t                 =<< cvtE x
        PrimTan t                 -> A.tan t                 =<< cvtE x
        PrimSinh t                -> A.sinh t                =<< cvtE x
        PrimCosh t                -> A.cosh t                =<< cvtE x
        PrimTanh t                -> A.tanh t                =<< cvtE x
        PrimAsin t                -> A.asin t                =<< cvtE x
        PrimAcos t                -> A.acos t                =<< cvtE x
        PrimAtan t                -> A.atan t                =<< cvtE x
        PrimAsinh t               -> A.asinh t               =<< cvtE x
        PrimAcosh t               -> A.acosh t               =<< cvtE x
        PrimAtanh t               -> A.atanh t               =<< cvtE x
        PrimAtan2 t               -> A.uncurry (A.atan2 t)   =<< cvtE x
        PrimExpFloating t         -> A.exp t                 =<< cvtE x
        PrimFPow t                -> A.uncurry (A.fpow t)    =<< cvtE x
        PrimSqrt t                -> A.sqrt t                =<< cvtE x
        PrimLog t                 -> A.log t                 =<< cvtE x
        PrimLogBase t             -> A.uncurry (A.logBase t) =<< cvtE x
        PrimTruncate ta tb        -> A.truncate ta tb        =<< cvtE x
        PrimRound ta tb           -> A.round ta tb           =<< cvtE x
        PrimFloor ta tb           -> A.floor ta tb           =<< cvtE x
        PrimCeiling ta tb         -> A.ceiling ta tb         =<< cvtE x
        PrimIsNaN t               -> A.isNaN t               =<< cvtE x
        PrimIsInfinite t          -> A.isInfinite t          =<< cvtE x
        PrimLt t                  -> A.uncurry (A.lt t)      =<< cvtE x
        PrimGt t                  -> A.uncurry (A.gt t)      =<< cvtE x
        PrimLtEq t                -> A.uncurry (A.lte t)     =<< cvtE x
        PrimGtEq t                -> A.uncurry (A.gte t)     =<< cvtE x
        PrimEq t                  -> A.uncurry (A.eq t)      =<< cvtE x
        PrimNEq t                 -> A.uncurry (A.neq t)     =<< cvtE x
        PrimMax t                 -> A.uncurry (A.max t)     =<< cvtE x
        PrimMin t                 -> A.uncurry (A.min t)     =<< cvtE x
        PrimLAnd                  -> A.land (inl x) (inr x)  -- short circuit
        PrimLOr                   -> A.lor  (inl x) (inr x)  -- short circuit
        PrimLNot                  -> A.lnot                  =<< cvtE x
        PrimOrd                   -> A.ord                   =<< cvtE x
        PrimChr                   -> A.chr                   =<< cvtE x
        PrimBoolToInt             -> A.boolToInt             =<< cvtE x
        PrimFromIntegral ta tb    -> A.fromIntegral ta tb    =<< cvtE x
        PrimToFloating ta tb      -> A.toFloating ta tb      =<< cvtE x
          -- no missing patterns, whoo!


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

-- | Number of elements contained within a shape
--
shapeSize :: forall arch sh. Shape sh => IR sh -> CodeGen arch (IR Int)
shapeSize (IR extent) = go (eltType @sh) extent
  where
    go :: TupleType t -> Operands t -> CodeGen arch (IR Int)
    go TypeRunit OP_Unit
      = return $ A.lift 1

    go (TypeRscalar t) (op' t -> i)
      | Just Refl <- matchScalarType t (scalarType :: ScalarType Int)
      = return $ ir t i

    go (TypeRpair tsh t) (OP_Pair sh sz)
      | Just Refl <- matchTupleType t (eltType @Int)
      = case matchTupleType tsh (eltType @Z) of
          Just Refl -> return (IR sz)
          Nothing   -> do
            a <- go tsh sh
            b <- A.mul numType a (IR sz)
            return b

    go _ _
      = $internalError "shapeSize" "expected shape with Int components"

-- | Convert a multidimensional array index into a linear index
--
intOfIndex :: forall arch sh. Shape sh => IR sh -> IR sh -> CodeGen arch (IR Int)
intOfIndex (IR extent) (IR index) = cvt (eltType @sh) extent index
  where
    cvt :: TupleType t -> Operands t -> Operands t -> CodeGen arch (IR Int)
    cvt TypeRunit OP_Unit OP_Unit
      = return $ A.lift 0

    cvt (TypeRscalar t) _ (op' t -> i)
      | Just Refl <- matchScalarType t (scalarType :: ScalarType Int)
      = return $ ir t i

    cvt (TypeRpair tsh t) (OP_Pair sh sz) (OP_Pair ix i)
      | Just Refl <- matchTupleType t (eltType @Int)
      -- If we short-circuit the last dimension, we can avoid inserting
      -- a multiply by zero and add of the result.
      = case matchTupleType tsh (eltType @Z) of
          Just Refl -> return (IR i)
          Nothing   -> do
            a <- cvt tsh sh ix
            b <- A.mul numType a (IR sz)
            c <- A.add numType b (IR i)
            return c

    cvt _ _ _
      = $internalError "intOfIndex" "expected shape with Int components"


-- | Convert a linear index into into a multidimensional index
--
indexOfInt :: forall arch sh. Shape sh => IR sh -> IR Int -> CodeGen arch (IR sh)
indexOfInt (IR extent) index = IR <$> cvt (eltType @sh) extent index
  where
    cvt :: TupleType t -> Operands t -> IR Int -> CodeGen arch (Operands t)
    cvt TypeRunit OP_Unit _
      = return OP_Unit

    cvt (TypeRscalar t) _ (IR i)
      | Just Refl <- matchScalarType t (scalarType :: ScalarType Int)
      = return i

    cvt (TypeRpair tsh tsz) (OP_Pair sh sz) i
      | Just Refl <- matchTupleType tsz (eltType @Int)
      = do
           i'    <- A.quot integralType i (IR sz)
           -- If we assume the index is in range, there is no point computing
           -- the remainder of the highest dimension since (i < sz) must hold
           IR r  <- case matchTupleType tsh (eltType @Z) of
                      Just Refl -> return i     -- TODO: in debug mode assert (i < sz)
                      Nothing   -> A.rem  integralType i (IR sz)
           sh'   <- cvt tsh sh i'
           return $ OP_Pair sh' r

    cvt _ _ _
      = $internalError "indexOfInt" "expected shape with Int components"

-- | Read an element at a multidimensional index
--
indexArray :: (Shape sh, Elt e) => IRArray (Array sh e) -> IR sh -> IROpenExp arch env aenv e
indexArray arr ix = linearIndexArray arr =<< intOfIndex (irArrayShape arr) ix

-- | Read an element at a linear index
--
linearIndexArray :: (Shape sh, Elt e) => IRArray (Array sh e) -> IR Int -> IROpenExp arch env aenv e
linearIndexArray = readArray

