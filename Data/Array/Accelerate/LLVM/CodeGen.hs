{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--


module Data.Array.Accelerate.LLVM.CodeGen
  where

-- llvm-general
import LLVM.General.AST                                         hiding ( nuw, nsw )
import LLVM.General.AST.Global
import LLVM.General.AST.Constant                                ( Constant )
import qualified LLVM.General.AST.Constant                      as C

-- accelerate
import Data.Array.Accelerate.AST                                hiding ( Val(..), prj )
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Array.Sugar                        ( eltType, EltRepr, Z, (:.) )
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Analysis.Type                      ( expType, preExpType, delayedAccType )

-- import Data.Array.Accelerate.LLVM.Target
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic  as A

-- standard library
import Data.IntMap                                              ( IntMap )
import Control.Applicative                                      ( (<$>), (<*>) )
import qualified Data.IntMap                                    as IM
import qualified Data.Sequence                                  as Seq
import qualified Data.Foldable                                  as Seq

#include "accelerate.h"


-- Environments
-- ============

-- | A mapping between the environment index of a free array variable and the
-- Name of that array to be used in the generated code.
--
-- This simply compresses the array indices into a continuous range, rather than
-- directly using the integer equivalent of the de Bruijn index. Thus, the
-- result is still sensitive to the order of let bindings, but not of any
-- intermediate (unused) free array variables.
--
type Aval aenv = IntMap Name

-- | An environment for local scalar expression bindings, encoded at the value
-- level as a heterogenous snoc list, and on the type level as nested tuples.
--
data Val env where
  Empty ::                             Val ()
  Push  :: Val env -> IR env aenv t -> Val (env, t)
                        -- ^ Idx env t

-- Projection of a value from the valuation environment using a de Bruijn index.
--
prj :: Idx env t -> Val env -> IR env aenv t
prj ZeroIdx      (Push _   v) = v
prj (SuccIdx ix) (Push val _) = prj ix val
prj _            _            = INTERNAL_ERROR(error) "prj" "inconsistent valuation"


-- Code Generation
-- ===============

-- | The code generator produces a sequence of operands representing the LLVM
-- instructions needed to execute the expression of type `t` in surrounding
-- environment `env`. These are just phantom types.
--
-- The result consists of a list of operands, each representing the single field
-- of a (flattened) tuple expression down to atomic types.
--
type IR env aenv t = [Operand]


{--
-- | Convert a closed function of one argument into a sequence of LLVM basic
-- blocks.
--
llvmOfFun1
    :: Fun aenv (a -> b) -> Aval aenv -> IR () aenv a -> [BasicBlock] -- TLM: type synonym here?
llvmOfFun1 (Lam (Body f)) aenv xs =
  let
      (_, st)   = generateFunctionCode $ llvmOfOpenExp f (Empty `Push` xs) aenv
      blocks    = cgf_blocks st
                  Seq.|> BasicBlock (Name "end") (Seq.toList (cgf_instructions st)) (Do (Ret Nothing []))
  in
  Seq.toList blocks
--}


-- | Convert an open scalar expression into a sequence of LLVM IR instructions.
-- Code is generated in depth first order, and uses a monad to collect the
-- sequence of instructions to construct basic blocks.
--
llvmOfOpenExp
    :: forall _env aenv _t.
       OpenExp _env aenv _t
    -> Val _env
    -> Aval aenv
    -> LLVM (IR _env aenv _t)
llvmOfOpenExp exp env aenv = cvtE exp env
  where
    cvtE :: forall env t. OpenExp env aenv t -> Val env -> LLVM (IR env aenv t)
    cvtE exp env =
      case exp of
--        Let bnd body            -> elet bnd body env
        Var ix                  -> return $ prj ix env
        PrimConst c             -> return $ [constOp (primConst c)]
        Const c                 -> return $ map constOp (constant (eltType (undefined::t)) c)
        PrimApp f arg           -> cvtE arg env >>= llvmOfPrimFun f >>= return . return
        Tuple t                 -> cvtT t env
        Prj i t                 -> prjT i t exp env
--        Cond p t e              -> cond p t e env
--        While p f x             -> while p f x env

        -- Shapes and indices
        IndexNil                -> return []
        IndexAny                -> return []
        IndexCons sh sz         -> (++) <$> cvtE sh env <*> cvtE sz env
        IndexHead ix            -> indexHead <$> cvtE ix env
        IndexTail ix            -> indexTail <$> cvtE ix env
        IndexSlice ix slix sh   -> indexSlice ix slix sh env
        IndexFull  ix slix sl   -> indexFull  ix slix sl env
--        ToIndex sh ix           -> toIndex   sh ix env
--        FromIndex sh ix         -> fromIndex sh ix env

        -- Arrays and indexing
--        Index acc ix            -> index acc ix env
--        LinearIndex acc ix      -> linearIndex acc ix env
--        Shape acc               -> shape acc env
--        ShapeSize sh            -> shapeSize sh env
--        Intersect sh1 sh2       -> intersect sh1 sh2 env

        --Foreign function
--        Foreign ff _ e          -> foreignE ff e env

    -- The heavy lifting
    -- -----------------

    -- Convert an open expression into a sequence of C expressions. We retain
    -- snoc-list ordering, so the element at tuple index zero is at the end of
    -- the list. Note that nested tuple structures are flattened.
    --
    cvtT :: Tuple (OpenExp env aenv) t -> Val env -> LLVM (IR env aenv t)
    cvtT tup env =
      case tup of
        NilTup          -> return []
        SnocTup t e     -> (++) <$> cvtT t env <*> cvtE e env

    -- Project out a tuple index. Since the nested tuple structure is flattened,
    -- this actually corresponds to slicing out a subset of the list of
    -- expressions, rather than picking out a single element.
    --
    prjT :: TupleIdx (TupleRepr t) e
         -> OpenExp env aenv t
         -> OpenExp env aenv e
         -> Val env
         -> LLVM (IR env aenv t)
    prjT ix t e env =
      let
          llt    = expType t -- preExpType delayedAccType t
          subset = reverse
                 . take (length (llvmOfTupleType llt))
                 . drop (prjToInt ix llt)
                 . reverse      -- as Accelerate expressions use a snoc-list representation
      in
      subset <$> cvtE t env

    -- Convert a tuple index into the corresponding integer. Since the internal
    -- representation is flat, be sure to walk over all sub components when indexing
    -- past nested tuples.
    --
    prjToInt :: TupleIdx t e -> TupleType a -> Int
    prjToInt ZeroTupIdx     _                 = 0
    prjToInt (SuccTupIdx i) (b `PairTuple` a) = sizeTupleType a + prjToInt i b
    prjToInt _              _                 = INTERNAL_ERROR(error) "prjToInt" "inconsistent valuation"

    sizeTupleType :: TupleType a -> Int
    sizeTupleType UnitTuple       = 0
    sizeTupleType (SingleTuple _) = 1
    sizeTupleType (PairTuple a b) = sizeTupleType a + sizeTupleType b

    -- Get the innermost index of a shape/index
    indexHead :: IR env aenv (sh :. sz) -> IR env anv sz
    indexHead = return . last

    -- Get the tail of a shape/index
    indexTail :: IR env aenv (sh :. sz) -> IR env aenv sh
    indexTail = init

    -- Restrict indices based on a slice specification. In the SliceAll case we
    -- elide the presence of IndexAny from the head of slx, as this is not
    -- represented in by any C term (Any ~ [])
    --
    indexSlice :: SliceIndex (EltRepr slix) sl co (EltRepr sh)
               -> OpenExp env aenv slix
               -> OpenExp env aenv sh
               -> Val env
               -> LLVM (IR env aenv sl)
    indexSlice sliceIndex slix sh env =
      let restrict :: SliceIndex slix sl co sh -> IR env aenv slix -> IR env aenv sh -> IR env aenv sl
          restrict SliceNil              _       _       = []
          restrict (SliceAll   sliceIdx) slx     (sz:sl) = sz : restrict sliceIdx slx sl
          restrict (SliceFixed sliceIdx) (_:slx) ( _:sl) =      restrict sliceIdx slx sl
          restrict _ _ _ = INTERNAL_ERROR(error) "IndexSlice" "unexpected shapes"
          --
          slice slix' sh' = reverse $ restrict sliceIndex (reverse slix') (reverse sh')
      in
      slice <$> cvtE slix env <*> cvtE sh env

    -- Extend indices based on a slice specification. In the SliceAll case we
    -- elide the presence of Any from the head of slx.
    --
    indexFull :: SliceIndex (EltRepr slix) (EltRepr sl) co sh
              -> OpenExp env aenv slix
              -> OpenExp env aenv sl
              -> Val env
              -> LLVM (IR env aenv sh)
    indexFull sliceIndex slix sl env =
      let extend :: SliceIndex slix sl co sh -> IR env aenv slix -> IR env aenv sl -> IR env aenv sh
          extend SliceNil              _        _       = []
          extend (SliceAll   sliceIdx) slx      (sz:sh) = sz : extend sliceIdx slx sh
          extend (SliceFixed sliceIdx) (sz:slx) sh      = sz : extend sliceIdx slx sh
          extend _ _ _ = INTERNAL_ERROR(error) "IndexFull" "unexpected shapes"
          --
          replicate slix' sl' = reverse $ extend sliceIndex (reverse slix') (reverse sl')
      in
      replicate <$> cvtE slix env <*> cvtE sl env


-- | Generate llvm operations for primitive scalar functions
--
llvmOfPrimFun :: PrimFun f -> [Operand] -> LLVM Operand
llvmOfPrimFun (PrimAdd t)              [a,b] = A.add t a b
llvmOfPrimFun (PrimSub t)              [a,b] = A.sub t a b
llvmOfPrimFun (PrimMul t)              [a,b] = A.mul t a b
llvmOfPrimFun (PrimNeg t)              [a]   = A.negate t a
llvmOfPrimFun (PrimAbs t)              [a]   = A.abs t a
llvmOfPrimFun (PrimSig t)              [a]   = A.signum t a
llvmOfPrimFun (PrimQuot t)             [a,b] = A.quot t a b
llvmOfPrimFun (PrimRem t)              [a,b] = A.rem t a b
llvmOfPrimFun (PrimIDiv t)             [a,b] = A.idiv t a b
llvmOfPrimFun (PrimMod t)              [a,b] = A.mod t a b
llvmOfPrimFun (PrimBAnd t)             [a,b] = A.band t a b
llvmOfPrimFun (PrimBOr t)              [a,b] = A.bor t a b
llvmOfPrimFun (PrimBXor t)             [a,b] = A.xor t a b
llvmOfPrimFun (PrimBNot t)             [a]   = A.complement t a
llvmOfPrimFun (PrimBShiftL t)          [a,b] = A.shiftL t a b
llvmOfPrimFun (PrimBShiftR t)          [a,b] = A.shiftR t a b
llvmOfPrimFun (PrimBRotateL t)         [a,b] = A.rotateL t a b
llvmOfPrimFun (PrimBRotateR t)         [a,b] = A.rotateR t a b
llvmOfPrimFun (PrimFDiv t)             [a,b] = A.fdiv t a b
llvmOfPrimFun (PrimRecip t)            [a]   = A.recip t a
llvmOfPrimFun (PrimSin t)              [a]   = A.sin t a
llvmOfPrimFun (PrimCos t)              [a]   = A.cos t a
llvmOfPrimFun (PrimTan t)              [a]   = A.tan t a
llvmOfPrimFun (PrimAsin t)             [a]   = A.asin t a
llvmOfPrimFun (PrimAcos t)             [a]   = A.acos t a
llvmOfPrimFun (PrimAtan t)             [a]   = A.atan t a
llvmOfPrimFun (PrimAsinh t)            [a]   = A.asinh t a
llvmOfPrimFun (PrimAcosh t)            [a]   = A.acosh t a
llvmOfPrimFun (PrimAtanh t)            [a]   = A.atanh t a
llvmOfPrimFun (PrimAtan2 t)            [a,b] = A.atan2 t a b
llvmOfPrimFun (PrimExpFloating t)      [a]   = A.exp t a
llvmOfPrimFun (PrimFPow t)             [a,b] = A.fpow t a b
llvmOfPrimFun (PrimSqrt t)             [a]   = A.sqrt t a
llvmOfPrimFun (PrimLog t)              [a]   = A.log t a
llvmOfPrimFun (PrimLogBase t)          [a,b] = A.logBase t a b
llvmOfPrimFun (PrimTruncate ta tb)     [a]   = A.truncate ta tb a
llvmOfPrimFun (PrimRound ta tb)        [a]   = A.round ta tb a
llvmOfPrimFun (PrimFloor ta tb)        [a]   = A.floor ta tb a
llvmOfPrimFun (PrimCeiling ta tb)      [a]   = A.ceiling ta tb a
llvmOfPrimFun (PrimLt t)               [a,b] = A.lt t a b
llvmOfPrimFun (PrimGt t)               [a,b] = A.gt t a b
llvmOfPrimFun (PrimLtEq t)             [a,b] = A.lte t a b
llvmOfPrimFun (PrimGtEq t)             [a,b] = A.gte t a b
llvmOfPrimFun (PrimEq t)               [a,b] = A.eq t a b
llvmOfPrimFun (PrimNEq t)              [a,b] = A.neq t a b
llvmOfPrimFun (PrimMax t)              [a,b] = A.max t a b
llvmOfPrimFun (PrimMin t)              [a,b] = A.min t a b
llvmOfPrimFun PrimLAnd                 [a,b] = A.land a b
llvmOfPrimFun PrimLOr                  [a,b] = A.lor a b
llvmOfPrimFun PrimLNot                 [a]   = A.lnot a
llvmOfPrimFun PrimOrd                  [a]   = A.ord a
llvmOfPrimFun PrimChr                  [a]   = A.chr a
llvmOfPrimFun PrimBoolToInt            [a]   = A.boolToInt a
llvmOfPrimFun (PrimFromIntegral ta tb) [a]   = A.fromIntegral ta tb a

-- If the argument lists are not the correct length
llvmOfPrimFun _ _ =
  INTERNAL_ERROR(error) "llvmOfPrimFun" "inconsistent valuation"

