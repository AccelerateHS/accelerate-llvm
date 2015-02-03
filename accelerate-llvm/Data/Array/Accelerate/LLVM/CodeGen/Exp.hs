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
import Data.Array.Accelerate.Array.Sugar                        hiding ( toTuple )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Analysis.Match

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad                 ( CodeGen )
import Data.Array.Accelerate.LLVM.CodeGen.Environment


type LLVMAcc acc = forall aenv a. acc aenv a -> Val aenv -> CodeGen (IR a)


-- Scalar expressions
-- ==================

-- | Convert an open scalar expression into a sequence of LLVM IR instructions.
-- Code is generated in depth first order, and uses a monad to collect the
-- sequence of instructions used to construct basic blocks.
--
llvmOfOpenExp
    :: forall acc env aenv _t.
       LLVMAcc acc
    -> PreOpenExp acc env aenv _t
    -> Val env
    -> Val aenv
    -> CodeGen (IR _t)
llvmOfOpenExp cvtA top env aenv = cvtE top
  where
    cvtE :: forall t. PreOpenExp acc env aenv t -> CodeGen (IR t)
    cvtE exp =
      case exp of
        Let bnd body            -> do x <- cvtE bnd
                                      llvmOfOpenExp cvtA body (env `Push` x) aenv
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

        Foreign{}                      -> $internalError "llvmOfOpenExp" "Foreign not supported yet"


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

    cvtT :: forall t. (Elt t, IsTuple t) => Tuple (PreOpenExp acc env aenv) (TupleRepr t) -> CodeGen (IR t)
    cvtT tup = IR <$> go (eltType (undefined::t)) tup
      where
        go :: TupleType t' -> Tuple (PreOpenExp acc env aenv) tup -> CodeGen (Operands t')
        go UnitTuple NilTup
          = return OP_Unit

        go (PairTuple ta tb) (SnocTup a (b :: PreOpenExp acc env aenv b))
          | Just REFL <- matchTupleType tb (eltType (undefined::b))
          = do a'    <- go ta a
               IR b' <- cvtE b
               return $ OP_Pair a' b'

        go _ _ = $internalError "cvtT" "impossible evaluation"


-- Primitive functions
-- ===================

fst :: forall a b. Elt a => IR (a, b) -> IR a
fst (IR (OP_Pair (OP_Pair OP_Unit x) _)) = IR x

snd :: forall a b. Elt b => IR (a, b) -> IR b
snd (IR (OP_Pair _ y)) = IR y

unpair :: (Elt a, Elt b) => IR (a, b) -> (IR a, IR b)
unpair x = (fst x, snd x)

uncurry :: (Elt a, Elt b) => (IR a -> IR b -> c) -> IR (a, b) -> c
uncurry f (unpair -> (x,y)) = f x y


-- | Generate llvm operations for primitive scalar functions
--
llvmOfPrimFun :: forall a r. (Elt a, Elt r) => PrimFun (a -> r) -> IR a -> CodeGen (IR r)
llvmOfPrimFun (PrimAdd t) = uncurry (add t)

