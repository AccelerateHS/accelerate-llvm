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
import Data.Proxy

import LLVM.General.AST.Type.Operand

import Data.Array.Accelerate.AST                                hiding ( Val(..), prj )
import Data.Array.Accelerate.Array.Sugar                        hiding ( toTuple )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Analysis.Match

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad                 ( CodeGen )
import Data.Array.Accelerate.LLVM.CodeGen.Environment

import Data.Typeable
import Debug.Trace


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
          = do
               a'    <- go ta a
               IR b' <- cvtE b
               return $ OP_Pair a' b'

--      = trace ("typeOf (TupleRepr t) = " ++ show (typeOf (undefined :: (TupleRepr t))))
--      = trace ("typeOf (EltRepr t) = " ++ show (typeOf (undefined :: (EltRepr t))))
--      $ trace ("typeOf t = " ++ show (typeOf (undefined :: t)))
--      $ error "cvtT"

{--
    cvtT :: forall t. (Elt t, IsTuple t) => Tuple (PreOpenExp acc env aenv) (TupleRepr t) -> CodeGen (IR t)
    cvtT NilTup
      | UnitTuple <- eltType (undefined::t)
      = return $ IR OP_Unit

    cvtT (SnocTup t e :: Tuple (PreOpenExp acc env aenv) (a,b))
      = do IR e' <- cvtE e
           IR t' <- cvtT t
           return $ IR (OP_Pair t' (convertEltRepr' (undefined::b) e'))
--}
--
-- matchTupleProdRepr :: TupleType s -> ProdR cst t -> Maybe (s :=: t)
-- matchTupleProdRepr UnitTuple        ProdRunit      = Just REFL
-- matchTupleProdRepr (PairTuple t1 _) (ProdRsnoc t2) = matchTupleProdRepr t1 undefined
-- matchTupleProdRepr _                _              = Nothing


{--
      = trace ("eltType t = " ++ show (eltType (undefined :: t)))
      $ trace ("prod t  = " ++ show (tuple (undefined :: t)))
--      $ trace ("prod (TupleRepr t) = " ++ show (tuple (undefined :: (TupleRepr t))))
      $ error "cvtT"
--}
    -- cvtT :: Tuple (PreOpenExp acc env aenv) t -> CodeGen (Tuple IR t)
    -- cvtT NilTup            = return NilTup
    -- cvtT (tup `SnocTup` e) = SnocTup <$> cvtT tup <*> cvtE e

--    cvtT :: Tuple (PreOpenExp acc env aenv) t -> CodeGen (IR t)
--    cvtT NilTup            = return $ IR OP_Unit
--    cvtT ((tup :: Tuple (PreOpenExp acc env aenv) tup) `SnocTup` (e :: PreOpenExp acc env aenv e)) = do
--      IR e'     <- cvtE e
--      IR tup'   <- cvtT tup
--      return . IR $ OP_Pair tup' (convertEltRepr' (undefined::e) e')

{--
    cvtT :: forall tup. (Elt tup, IsTuple tup) => Tuple (PreOpenExp acc env aenv) (TupleRepr tup) -> CodeGen (IR tup)
    cvtT tup =
      let dummy = undefined :: tup
      in
      IR <$> go dummy (eltType dummy) (tuple dummy) tup
      where
        go :: forall t. {- dummy -} t
           -> TupleType (EltRepr t)
           -> TupleR (TupleRepr t)
           -> Tuple (PreOpenExp acc env aenv) (TupleRepr t)
           -> CodeGen (Operands (EltRepr t))
        go _ UnitTuple ProdRunit NilTup
          = return OP_Unit
        go _ (PairTuple ta tb) (ProdRsnoc r) (SnocTup (a :: Tuple (PreOpenExp acc env aenv) a)
                                                      (b :: PreOpenExp acc env aenv b))
          | Just REFL <- matchTupleType ta (eltType (undefined::a))
          , Just REFL <- matchTupleType tb (eltType (undefined::b))
          = do a'    <- go (undefined::a) ta r a
               IR b' <- cvtE b
               return $ OP_Pair a' b'
--}
{--
    cvtT :: forall t. (Elt t, IsTuple t) => Tuple (PreOpenExp acc env aenv) (TupleRepr t) -> CodeGen (IR t)
    cvtT t = IR <$> go (eltType (undefined::t)) (tuple (undefined::t)) t
      where
        go :: TupleType (EltRepr t)
           -> ProdR Elt (TupleRepr t)
           -> Tuple (PreOpenExp acc env aenv) (TupleRepr t)
           -> CodeGen (Operands (EltRepr t))
        go UnitTuple ProdRunit NilTup
          = return OP_Unit
        go (PairTuple ta tb) (ProdRsnoc r) (SnocTup a b :: Tuple (PreOpenExp acc env aenv) (a,b))
          | Just REFL <- matchTupleType ta (eltType (undefined::a))
          , Just REFL <- matchTupleType tb (eltType (undefined::b))
          = do -- a'    <- go ta r a
               IR b' <- cvtE b
               return $ OP_Pair (a' :: Operands (EltRepr a))  b'
--}
{--
    cvtT :: forall t. (Elt t, IsTuple t) => Tuple (PreOpenExp acc env aenv) (TupleRepr t) -> CodeGen (IR t)
    cvtT t = IR <$> go (eltType (undefined::t)) (prod (Proxy :: Proxy Elt) (undefined::t)) t
      where
        go :: TupleType (EltRepr t)
           -> ProdR Elt (TupleRepr t)
           -> Tuple (PreOpenExp acc env aenv) (TupleRepr t)
           -> CodeGen (Operands (EltRepr t))
        go UnitTuple         ProdRunit     NilTup
          = return OP_Unit
        go (PairTuple UnitTuple tb) (ProdRsnoc ProdRunit) (SnocTup a b :: Tuple (PreOpenExp acc env aenv) ((),b))
          | Just REFL <- matchTupleType tb (eltType (undefined::b))
          = do IR b' <- cvtE b
               return $ OP_Pair OP_Unit b'
--}
{--
        go (PairTuple (PairTuple UnitTuple ta) tb) (ProdRsnoc (ProdRsnoc ProdRunit)) (SnocTup a b :: Tuple (PreOpenExp acc env aenv) (TupleRepr (a,b)))
          | Just REFL <- matchTupleType ta (eltType (undefined::a))
          , Just REFL <- matchTupleType tb (eltType (undefined::b))
          = do IR a' <- cvtT a
               IR b' <- cvtE b
               return $ OP_Pair (OP_Pair OP_Unit a') b'
--}
{--
        go (PairTuple ta tb) (ProdRsnoc r) (SnocTup a b :: Tuple (PreOpenExp acc env aenv) (a,b))
          | Just REFL <- matchTupleType ta (eltType (undefined::a))
          , Just REFL <- matchTupleType tb (eltType (undefined::b))
          = do -- a'    <- go ta r a

--               a'       <- case ta of
--                             UnitTuple      -> return OP_Unit
--                             SingleTuple{}  -> do OP_Pair x OP_Unit <- go ta undefined a
--                                                  return x

               a'    <- undefined
               IR b' <- cvtE b
               return $ OP_Pair (a' :: Operands (EltRepr a))  b'
--}


-- IR (TupleRepr t) ~ EltRepr (TupleRepr t)
--
-- IR t ~ EltRepr t
{--
toTuple :: forall tup. (IsTuple tup, Elt tup) => IR (TupleRepr tup) -> IR tup
toTuple (IR t) = IR $ go (eltType (undefined::tup)) (tuple (undefined::tup)) t
  where
    go :: TupleType t -> TupleR (TupleRepr t) -> Operands (TupleRepr t) -> Operands t
    go UnitTuple         ProdRunit      OP_Unit = OP_Unit
--    go (PairTuple ta tb) (ProdRsnoc r)
--}
{--
 -- promising!! Need to eject an Elt constraint though
 --
toTuple :: forall tup. (IsTuple tup, Elt tup) => IR (TupleRepr tup) -> IR tup
toTuple (IR t) =
  let tup = undefined :: tup
  in  IR  $ go tup (eltType tup) (prod (Proxy :: Proxy Elt) tup) t
  where
    go :: {- dummy -} t -> TupleType (EltRepr t) -> ProdR Elt (TupleRepr t) -> Operands (EltRepr (TupleRepr t)) -> Operands (EltRepr t)
    go _ UnitTuple         ProdRunit       OP_Unit       = OP_Unit
    go _ (PairTuple ta tb) (ProdRsnoc tup) (OP_Pair a b) = OP_Pair undefined undefined --(convertEltRepr' undefined b)
--}

--toTuple :: forall t. (Elt t, IsTuple t) => Tuple IR (ProdRepr t) -> IR t
--toTuple =
--  where
--    go :: ProdR Elt (ProdRepr tup) -> TupleType tup -> Tuple IR (ProdRepr tup) -> Operands (EltRepr tup)
--    go  ProdRunit UnitTuple NilTup = OP_Unit


{--
toTuple :: TupleR (TupleRepr t) -> TupleType (EltRepr t) -> Tuple IR (TupleRepr t) -> IR t
toTuple ProdRunit      UnitTuple          NilTup                                 = IR OP_Unit
--toTuple (ProdRsnoc tr) (PairTuple ta tb) (SnocTup (a :: Tuple IR (TupleRepr a)) (b :: IR (TupleRepr b))) =
toTuple (ProdRsnoc tr) (PairTuple ta tb) (SnocTup a b) =
  let IR a' = toTuple tr ta a
      IR b' = b
  in
  undefined
--  IR $ OP_Pair (OP_Pair OP_Unit (convertEltRepr' (undefined::a) a'))
--                                (convertEltRepr' (undefined::b) b')
--}

--toTuple :: forall t. {- dummy -} t -> Tuple IR (ProdRepr t) -> Operands (EltRepr t)
--toTuple _ NilTup
--  = OP_Unit
--toTuple _ -- (PairTuple (PairTuple UnitTuple (SingleTuple ta)) (SingleTuple tb))
--          (NilTup `SnocTup` (IR a :: IR a) `SnocTup` (IR b :: IR b))
--  = OP_Pair (OP_Pair OP_Unit (convertEltRepr' (undefined::a) a))
--                             (convertEltRepr' (undefined::b) b)
--

--toTuple _ (PairTuple ta tb) ((a :: Tuple IR a) `SnocTup` ((IR b) :: IR b))
--  = OP_Pair (toTuple (undefined::a) ta a) (convertEltRepr' (undefined::b) b)


--  = OP_Pair (toTuple (undefined::a) ta a) undefined

--toTuple (SnocTup t (IR e :: IR e)) =
--  let IR t' = toTuple t
--      e'    = convertEltRepr' (undefined::e) e
--  in
--  IR (OP_Pair t' e')

{--
toTuple :: forall t. Tuple IR t -> IR t
toTuple NilTup                     = IR OP_Unit
toTuple (SnocTup t (IR e :: IR e)) =
  let IR t' = toTuple t
      e'    = convertEltRepr' (undefined::e) e
  in
  IR (OP_Pair t' e')
--}

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

