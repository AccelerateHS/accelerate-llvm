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
import Prelude                                                      hiding ( exp, any )
import qualified Data.IntMap                                        as IM
import GHC.TypeNats

import Data.Array.Accelerate.AST                                    hiding ( Val(..), prj )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Representation                   hiding ( shape, vecPack, vecUnpack )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Sugar                  as A

import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Operand                                        ( Operand )

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
llvmOfFun1 (Lam lhs (Body body)) aenv = IRFun1 $ \x -> llvmOfOpenExp body (Empty `pushE` (lhs, x)) aenv
llvmOfFun1  _ _ = $internalError "llvmOfFun1" "impossible evaluation"

{-# INLINEABLE llvmOfFun2 #-}
llvmOfFun2
    :: Foreign arch
    => DelayedFun aenv (a -> b -> c)
    -> Gamma aenv
    -> IRFun2 arch aenv (a -> b -> c)
llvmOfFun2 (Lam lhs1 (Lam lhs2 (Body body))) aenv = IRFun2 $ \x y -> llvmOfOpenExp body (Empty `pushE` (lhs1, x) `pushE` (lhs2, y)) aenv
llvmOfFun2 _ _ = $internalError "llvmOfFun2" "impossible evaluation"


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
    cvtF1 (Lam lhs (Body body)) = IRFun1 $ \x -> llvmOfOpenExp body (env `pushE` (lhs, x)) aenv
    cvtF1 _                     = $internalError "cvtF1" "impossible evaluation"

    cvtE :: forall t. DelayedOpenExp env aenv t -> IROpenExp arch env aenv t
    cvtE exp =
      case exp of
        Let lhs bnd body            -> do x <- cvtE bnd
                                          llvmOfOpenExp body (env `pushE` (lhs, x)) aenv
        Evar (Var _ ix)             -> return $ prj ix env
        Const tp c                  -> return $ IR $ ir' tp $ scalar tp c
        PrimConst c                 -> let tp = (SingleScalarType $ primConstType c)
                                       in  return $ IR $ ir' tp $ scalar tp $ primConst c
        PrimApp f x                 -> primFun f x
        Undef tp                    -> return $ IR $ ir' tp $ undef tp
        Nil                         -> return $ IR OP_Unit
        Pair e1 e2                  -> join $ pair <$> cvtE e1 <*> cvtE e2
        VecPack   vecr e            -> vecPack   vecr =<< cvtE e
        VecUnpack vecr e            -> vecUnpack vecr =<< cvtE e
        Foreign tp asm f x          -> foreignE tp asm f =<< cvtE x
        Cond c t e                  -> A.ifThenElse (expType t, cvtE c) (cvtE t) (cvtE e)
        IndexSlice slice slix sh    -> indexSlice slice <$> cvtE slix <*> cvtE sh
        IndexFull slice slix sh     -> indexFull slice  <$> cvtE slix <*> cvtE sh
        ToIndex shr sh ix           -> join $ intOfIndex shr <$> cvtE sh <*> cvtE ix
        FromIndex shr sh ix         -> join $ indexOfInt shr <$> cvtE sh <*> cvtE ix
        Index acc ix                -> index (cvtM acc)       =<< cvtE ix
        LinearIndex acc ix          -> linearIndex (cvtM acc) =<< cvtE ix
        ShapeSize shr sh            -> shapeSize shr          =<< cvtE sh
        Shape acc                   -> return $ shape (cvtM acc)
        While c f x                 -> while (expType x) (cvtF1 c) (cvtF1 f) (cvtE x)
        Coerce t1 t2 x              -> coerce t1 t2 =<< cvtE x

    indexSlice :: SliceIndex slix sl co sh
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

    indexFull :: SliceIndex slix sl co sh
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

    vecPack :: forall n single tuple. KnownNat n => VecR n single tuple -> IR tuple -> CodeGen arch (IR (Vec n single))
    vecPack vecr (IR tuple) = ir tp <$> go vecr n tuple
      where
        go :: VecR n' single tuple' -> Int -> Operands tuple' -> CodeGen arch (Operand (Vec n single))
        go (VecRnil _)      0 OP_Unit        = return $ undef $ VectorScalarType tp
        go (VecRnil _)      _ OP_Unit        = $internalError "vecUnpack" "index mismatch"
        go (VecRsucc vecr') i (OP_Pair xs x) = do
          vec <- go vecr' (i - 1) xs
          instr' $ InsertElement (fromIntegral i - 1) vec (op' singleTp x)

        tp@(VectorType n singleTp) = vecRvector vecr

    vecUnpack :: forall n single tuple. KnownNat n => VecR n single tuple -> IR (Vec n single) -> CodeGen arch (IR tuple)
    vecUnpack vecr (IR (OP_Vec vec)) = IR <$> go vecr n
      where
        go :: VecR n' single tuple' -> Int -> CodeGen arch (Operands tuple')
        go (VecRnil _)      0 = return $ OP_Unit
        go (VecRnil _)      _ = $internalError "vecUnpack" "index mismatch"
        go (VecRsucc vecr') i = do
          xs <- go vecr' (i - 1)
          x  <- instr' $ ExtractElement (fromIntegral i - 1) vec
          return $ OP_Pair xs (ir' singleTp x)

        VectorType n singleTp = vecRvector vecr

    linearIndex :: ArrayVar aenv (Array sh e) -> IR Int -> IROpenExp arch env aenv e
    linearIndex (Var repr v) = linearIndexArray (irArray repr (aprj v aenv))

    index :: ArrayVar aenv (Array sh e) -> IR sh -> IROpenExp arch env aenv e
    index (Var repr v) = indexArray (irArray repr (aprj v aenv))

    shape :: ArrayVar aenv (Array sh e) -> IR sh
    shape (Var repr v) = irArrayShape (irArray repr (aprj v aenv))

    pair :: IR t1 -> IR t2 -> IROpenExp arch env aenv (t1, t2)
    pair (IR a) (IR b) = return $ IR $ OP_Pair a b

    while :: TupleType a
          -> IROpenFun1 arch env aenv (a -> Bool)
          -> IROpenFun1 arch env aenv (a -> a)
          -> IROpenExp  arch env aenv a
          -> IROpenExp  arch env aenv a
    while tp p f x =
      L.while tp (app1 p) (app1 f) =<< x

    foreignE :: A.Foreign asm
             => TupleType b
             -> asm           (a -> b)
             -> DelayedFun () (a -> b)
             -> IR a
             -> IRExp arch () b
    foreignE _ asm no x =
      case foreignExp asm of
        Just f                           -> app1 f x
        Nothing | Lam lhs (Body b) <- no -> llvmOfOpenExp b (Empty `pushE` (lhs, x)) IM.empty
        _                                -> error "when a grid's misaligned with another behind / that's a moiré..."

    coerce :: ScalarType a -> ScalarType b -> IR a -> IROpenExp arch env aenv b
    coerce s t (IR x)
      | Just Refl <- matchScalarType s t = return $ IR x
      | otherwise                        = IR . ir' t <$> instr' (BitCast t (op' s x))

    primFun :: PrimFun (a -> r)
            -> DelayedOpenExp env aenv a
            -> IROpenExp arch env aenv r
    primFun f x =
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
        PrimLAnd                  -> A.uncurry A.land        =<< cvtE x
        PrimLOr                   -> A.uncurry A.lor         =<< cvtE x
        PrimLNot                  -> A.lnot                  =<< cvtE x
        PrimOrd                   -> A.ord                   =<< cvtE x
        PrimChr                   -> A.chr                   =<< cvtE x
        PrimBoolToInt             -> A.boolToInt             =<< cvtE x
        PrimFromIntegral ta tb    -> A.irFromIntegral ta tb  =<< cvtE x
        PrimToFloating ta tb      -> A.toFloating ta tb      =<< cvtE x
          -- no missing patterns, whoo!


-- | Extract the head of an index
--
indexHead :: IR (sh, sz) -> IR sz
indexHead (IR (OP_Pair _ sz)) = IR sz

-- | Extract the tail of an index
--
indexTail :: IR (sh, sz) -> IR sh
indexTail (IR (OP_Pair sh _)) = IR sh

-- | Construct an index from the head and tail
--
indexCons :: IR sh -> IR sz -> IR (sh, sz)
indexCons (IR sh) (IR sz) = IR (OP_Pair sh sz)

-- | Number of elements contained within a shape
--
shapeSize :: ShapeR sh -> IR sh -> CodeGen arch (IR Int)
shapeSize shr (IR extent) = go shr extent
  where
    go :: ShapeR t -> Operands t -> CodeGen arch (IR Int)
    go ShapeRz OP_Unit
      = return $ A.liftInt 1

    go (ShapeRsnoc shr') (OP_Pair sh sz)
      = case shr' of
          ShapeRz -> return (IR sz)
          _       -> do
            a <- go shr' sh
            b <- A.mul numType a (IR sz)
            return b

-- | Convert a multidimensional array index into a linear index
--
intOfIndex :: ShapeR sh -> IR sh -> IR sh -> CodeGen arch (IR Int)
intOfIndex shr (IR extent) (IR index) = cvt shr extent index
  where
    cvt :: ShapeR t -> Operands t -> Operands t -> CodeGen arch (IR Int)
    cvt ShapeRz OP_Unit OP_Unit
      = return $ A.liftInt 0

    cvt (ShapeRsnoc shr') (OP_Pair sh sz) (OP_Pair ix i)
      -- If we short-circuit the last dimension, we can avoid inserting
      -- a multiply by zero and add of the result.
      = case shr' of
          ShapeRz -> return (IR i)
          _       -> do
            a <- cvt shr' sh ix
            b <- A.mul numType a (IR sz)
            c <- A.add numType b (IR i)
            return c


-- | Convert a linear index into into a multidimensional index
--
indexOfInt :: ShapeR sh -> IR sh -> IR Int -> CodeGen arch (IR sh)
indexOfInt shr (IR extent) index = IR <$> cvt shr extent index
  where
    cvt :: ShapeR t -> Operands t -> IR Int -> CodeGen arch (Operands t)
    cvt ShapeRz OP_Unit _
      = return OP_Unit

    cvt (ShapeRsnoc shr') (OP_Pair sh sz) i
      = do
           i'    <- A.quot integralType i (IR sz)
           -- If we assume the index is in range, there is no point computing
           -- the remainder of the highest dimension since (i < sz) must hold
           IR r  <- case shr' of
                      ShapeRz -> return i     -- TODO: in debug mode assert (i < sz)
                      _       -> A.rem  integralType i (IR sz)
           sh'   <- cvt shr' sh i'
           return $ OP_Pair sh' r

-- | Read an element at a multidimensional index
--
indexArray :: IRArray (Array sh e) -> IR sh -> IROpenExp arch env aenv e
indexArray arr ix = linearIndexArray arr =<< intOfIndex (arrayRshape $ irArrayRepr arr) (irArrayShape arr) ix

-- | Read an element at a linear index
--
linearIndexArray :: IRArray (Array sh e) -> IR Int -> IROpenExp arch env aenv e
linearIndexArray = readArray TypeInt

pushE :: Val env -> (ELeftHandSide t env env', IR t) -> Val env'
pushE env (LeftHandSideSingle _  , e)                  = env `Push` e
pushE env (LeftHandSideWildcard _, _)                  = env
pushE env (LeftHandSidePair l1 l2, IR (OP_Pair e1 e2)) = pushE env (l1, IR e1) `pushE` (l2, IR e2)
