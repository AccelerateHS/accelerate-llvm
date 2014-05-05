{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Exp
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Exp
  where

-- llvm-general
import LLVM.General.AST

-- accelerate
import Data.Array.Accelerate.AST                                hiding ( Val(..), prj )
import Data.Array.Accelerate.Analysis.Type                      ( preExpType, delayedAccType )
import Data.Array.Accelerate.Array.Representation               hiding ( Shape )
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt, EltRepr, Foreign, (:.), eltType )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic  as A

-- standard library
import Control.Applicative                                      ( (<$>), (<*>) )
import Control.Monad.State


-- Scalar functions
-- ================

-- | Convert a closed function of one argument into a sequence of LLVM basic
-- blocks.
--
llvmOfFun1 :: DelayedFun aenv (a -> b) -> Gamma aenv -> IRFun1 aenv (a -> b)
llvmOfFun1 (Lam (Body f)) aenv xs = llvmOfOpenExp f (Empty `Push` xs) aenv
llvmOfFun1 _              _    _  = error "dooo~ you knoooow~ what it's liiike"

llvmOfFun2 :: DelayedFun aenv (a -> b -> c) -> Gamma aenv -> IRFun2 aenv (a -> b -> c)
llvmOfFun2 (Lam (Lam (Body f))) aenv xs ys = llvmOfOpenExp f (Empty `Push` xs `Push` ys) aenv
llvmOfFun2 _                    _    _  _  = error "when the world seems to chaaaange~ overniiight"


-- Scalar expressions
-- ==================

-- | Convert an open scalar expression into a sequence of LLVM IR instructions.
-- Code is generated in depth first order, and uses a monad to collect the
-- sequence of instructions used to construct basic blocks.
--
llvmOfOpenExp
    :: forall _env aenv _t.
       DelayedOpenExp _env aenv _t
    -> Val _env
    -> Gamma aenv
    -> CodeGen (IR _env aenv _t)
llvmOfOpenExp exp env aenv = cvtE exp env
  where
    cvtE :: forall env t. DelayedOpenExp env aenv t -> Val env -> CodeGen (IR env aenv t)
    cvtE exp env =
      case exp of
        Let bnd body            -> elet bnd body env
        Var ix                  -> return $ prj ix env
        PrimConst c             -> return $ [constOp (primConst c)]
        Const c                 -> return $ map constOp (constant (eltType (undefined::t)) c)
        PrimApp f arg           -> cvtE arg env >>= llvmOfPrimFun f >>= return . return
        Tuple t                 -> cvtT t env
        Prj i t                 -> prjT i t exp env
        Cond p t e              -> cond p t e env
        While p f x             -> while p f x env

        -- Shapes and indices
        IndexNil                -> return []
        IndexAny                -> return []
        IndexCons sh sz         -> (++) <$> cvtE sh env <*> cvtE sz env
        IndexHead ix            -> indexHead <$> cvtE ix env
        IndexTail ix            -> indexTail <$> cvtE ix env
        IndexSlice ix slix sh   -> indexSlice ix slix sh env
        IndexFull  ix slix sl   -> indexFull  ix slix sl env
        ToIndex sh ix           -> toIndex   sh ix env
        FromIndex sh ix         -> fromIndex sh ix env

        -- Arrays and indexing
        Index acc ix            -> index acc ix env
        LinearIndex acc ix      -> linearIndex acc ix env
        Shape acc               -> shape acc
        ShapeSize sh            -> shapeSize sh env
        Intersect sh1 sh2       -> intersect sh1 sh2 env

        --Foreign function
        Foreign ff _ e          -> foreignE ff e env

    cvtF1 :: DelayedOpenFun env aenv (a -> b)
          -> Val env
          -> Gamma aenv
          -> IR env aenv a
          -> CodeGen (IR env aenv b)
    cvtF1 (Lam (Body f)) env aenv xs = llvmOfOpenExp f (env `Push` xs) aenv
    cvtF1 _              _   _    _  = error "impossible"

    -- The heavy lifting
    -- -----------------

    -- Scalar let expressions evaluate the binding and store the results into
    -- new variables. These names are added to the environment so they can be
    -- picked out by `Var`.
    --
    -- Note that there is no restriction to the scope of the new binding. Once
    -- something is added to the instruction stream, it remains there forever.
    --
    elet :: DelayedOpenExp env       aenv bnd
         -> DelayedOpenExp (env,bnd) aenv body
         -> Val env
         -> CodeGen (IR env aenv body)
    elet bnd body env = do
      x <- cvtE bnd env
      cvtE body (env `Push` x)

    -- Convert an open expression into a sequence of C expressions. We retain
    -- snoc-list ordering, so the element at tuple index zero is at the end of
    -- the list. Note that nested tuple structures are flattened.
    --
    cvtT :: Tuple (DelayedOpenExp env aenv) t -> Val env -> CodeGen (IR env aenv t)
    cvtT tup env =
      case tup of
        NilTup          -> return []
        SnocTup t e     -> (++) <$> cvtT t env <*> cvtE e env

    -- Project out a tuple index. Since the nested tuple structure is flattened,
    -- this actually corresponds to slicing out a subset of the list of
    -- expressions, rather than picking out a single element.
    --
    prjT :: TupleIdx (TupleRepr t) e
         -> DelayedOpenExp env aenv t
         -> DelayedOpenExp env aenv e
         -> Val env
         -> CodeGen (IR env aenv t)
    prjT ix t e env =
      let subset = reverse
                 . take (length      $ llvmOfTupleType (preExpType delayedAccType e))
                 . drop (prjToInt ix $ preExpType delayedAccType t)
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
    prjToInt _              _                 = $internalError "prjToInt" "inconsistent valuation"

    sizeTupleType :: TupleType a -> Int
    sizeTupleType UnitTuple       = 0
    sizeTupleType (SingleTuple _) = 1
    sizeTupleType (PairTuple a b) = sizeTupleType a + sizeTupleType b

    -- Evaluate scalar conditions. We create three new basic blocks: one for
    -- each side of the branch (true/false) and a new block both branches jump
    -- to after evaluating their part.
    --
    -- The branch instructions 'br' and 'cbr' return the name of the basic block
    -- that it terminates (branches from). This is because evaluation of the
    -- branches can lead to new block labels being created as we walk the AST.
    --
    -- See note: [Basic blocks]
    --
    cond :: forall env t. Elt t
         => DelayedOpenExp env aenv Bool
         -> DelayedOpenExp env aenv t
         -> DelayedOpenExp env aenv t
         -> Val env
         -> CodeGen (IR env aenv t)
    cond test t e env = do
      ifThen <- newBlock "if.then"
      ifElse <- newBlock "if.else"
      ifExit <- newBlock "if.exit"
      beginGroup "if"

      -- Compute the conditional
      p  <- single "cond" `fmap` cvtE test env
      _  <- cbr p ifThen ifElse

      -- Compute the true and false branches, then jump to the bottom
      setBlock ifThen
      tv   <- cvtE t env
      true <- br ifExit

      setBlock ifElse
      fv    <- cvtE e env
      false <- br ifExit

      -- Select the right value using the phi node
      setBlock ifExit
      zipWithM phi' (llvmOfTupleType (eltType (undefined::t))) [ [(t,true), (f,false)] | t <- tv | f <- fv ]

    -- Value recursion iterates a function while a conditional on that variable
    -- remains true. See note: [Basic blocks]
    while :: forall env a. Elt a
          => DelayedOpenFun env aenv (a -> Bool)
          -> DelayedOpenFun env aenv (a -> a)
          -> DelayedOpenExp env aenv a
          -> Val env
          -> CodeGen (IR env aenv a)
    while p f x env = do
      let ty = llvmOfTupleType (eltType (undefined::a))
      loop <- newBlock "while.top"
      exit <- newBlock "while.exit"
      beginGroup "while"

      -- Generate the seed value
      seed <-                       cvtE  x env
      c    <- single "while" `fmap` cvtF1 p env aenv seed
      top  <- cbr c loop exit

      -- Create some temporary names. These will be used to store the operands
      -- resulting from the phi node we will add to the top of the loop. We
      -- can't use recursive do because the monadic effects are recursive.
      ns   <- mapM (const freshName) ty
      let prev = map local ns

      -- Now generate the loop body. Afterwards, we insert a phi node at the
      -- head of the instruction stream, which selects the input value depending
      -- on which edge we entered the loop from (top or bottom).
      setBlock loop
      next <-                       cvtF1 f env aenv prev
      c    <- single "while" `fmap` cvtF1 p env aenv next
      bot  <- cbr c loop exit
      _    <- sequence $ zipWith3 (phi loop) ns ty [ [(t,top), (b,bot)] | t <- seed | b <- next ]

      -- Now the loop exit
      setBlock exit
      zipWithM phi' ty [ [(t,top), (b,bot)] | t <- seed | b <- next ]


    -- Get the innermost index of a shape/index
    indexHead :: IR env aenv (sh :. sz) -> IR env anv sz
    indexHead = return . last

    -- Get the tail of a shape/index
    indexTail :: IR env aenv (sh :. sz) -> IR env aenv sh
    indexTail = init

    -- Restrict indices based on a slice specification. In the SliceAll case we
    -- elide the presence of IndexAny from the head of slx, as this is not
    -- represented (we use Any ~ [])
    --
    indexSlice :: SliceIndex (EltRepr slix) sl co (EltRepr sh)
               -> DelayedOpenExp env aenv slix
               -> DelayedOpenExp env aenv sh
               -> Val env
               -> CodeGen (IR env aenv sl)
    indexSlice sliceIndex slix sh env =
      let restrict :: SliceIndex slix sl co sh -> IR env aenv slix -> IR env aenv sh -> IR env aenv sl
          restrict SliceNil              _       _       = []
          restrict (SliceAll   sliceIdx) slx     (sz:sl) = sz : restrict sliceIdx slx sl
          restrict (SliceFixed sliceIdx) (_:slx) ( _:sl) =      restrict sliceIdx slx sl
          restrict _ _ _ = $internalError "IndexSlice" "unexpected shapes"
          --
          slice slix' sh' = reverse $ restrict sliceIndex (reverse slix') (reverse sh')
      in
      slice <$> cvtE slix env <*> cvtE sh env

    -- Extend indices based on a slice specification. In the SliceAll case we
    -- elide the presence of Any from the head of slx.
    --
    indexFull :: SliceIndex (EltRepr slix) (EltRepr sl) co sh
              -> DelayedOpenExp env aenv slix
              -> DelayedOpenExp env aenv sl
              -> Val env
              -> CodeGen (IR env aenv sh)
    indexFull sliceIndex slix sl env =
      let extend :: SliceIndex slix sl co sh -> IR env aenv slix -> IR env aenv sl -> IR env aenv sh
          extend SliceNil              _        _       = []
          extend (SliceAll   sliceIdx) slx      (sz:sh) = sz : extend sliceIdx slx sh
          extend (SliceFixed sliceIdx) (sz:slx) sh      = sz : extend sliceIdx slx sh
          extend _ _ _ = $internalError "IndexFull" "unexpected shapes"
          --
          replicate slix' sl' = reverse $ extend sliceIndex (reverse slix') (reverse sl')
      in
      replicate <$> cvtE slix env <*> cvtE sl env

    -- Some terms demand we extract only simple scalar expressions
    single :: String -> [a] -> a
    single _   [x] = x
    single loc _   = $internalError loc "expected single expression"

    -- Generate the linear index of a multidimensional index and array shape
    --
    toIndex :: DelayedOpenExp env aenv sh       -- array extent
            -> DelayedOpenExp env aenv sh       -- index
            -> Val env
            -> CodeGen (IR env aenv Int)
    toIndex sh ix env = do
      sh' <- cvtE sh env
      ix' <- cvtE ix env
      return `fmap` intOfIndex sh' ix'

    -- Generate a multidimensional index from a linear index and array shape
    --
    fromIndex :: DelayedOpenExp env aenv sh     -- array extent
              -> DelayedOpenExp env aenv Int    -- index
              -> Val env
              -> CodeGen (IR env aenv sh)
    fromIndex sh ix env = do
      sh' <-                           cvtE sh env
      ix' <- single "fromIndex" `fmap` cvtE ix env
      indexOfInt sh' ix'

    -- Project out a single scalar element from an array. The array expression
    -- does not contain any free scalar variables (strictly flat data
    -- parallelism) and has been floated out by sharing recovery/array fusion to
    -- be replaced by an array index.
    --
    index :: forall sh e env. (Shape sh, Elt e)
          => DelayedOpenAcc     aenv (Array sh e)
          -> DelayedOpenExp env aenv sh
          -> Val env
          -> CodeGen (IR env aenv e)
    index (Manifest (Avar v)) ix env = do
      let name  = aprj v aenv
          ad    = arrayData  (undefined::Array sh e) name
          sh    = arrayShape (undefined::Array sh e) name
      --
      ix' <- cvtE ix env
      i   <- intOfIndex (map local sh) ix'
      readArray ad i
    index _ _ _ =
      $internalError "index" "expected array variable"

    linearIndex :: forall sh e env. Elt e
                => DelayedOpenAcc     aenv (Array sh e)
                -> DelayedOpenExp env aenv Int
                -> Val env
                -> CodeGen (IR env aenv e)
    linearIndex (Manifest (Avar v)) ix env = do
      let name  = aprj v aenv
          ad    = arrayData  (undefined::Array sh e) name
      --
      i   <- single "linearIndex" `fmap` cvtE ix env
      readArray ad i
    linearIndex _ _ _ =
      $internalError "linearIndex" "expected array variable"

    -- Array shapes created in this method refer to the shape of a free array
    -- variable, and are always passed as arguments to the function.
    --
    shape :: forall env sh e. Shape sh
          => DelayedOpenAcc aenv (Array sh e)
          -> CodeGen (IR env aenv sh)
    shape (Manifest (Avar v)) =
      let name  = aprj v aenv
          sh    = arrayShape (undefined::Array sh e) name
      in
      return (map local sh)
    shape _ =
      $internalError "shape" "expected array variable"

    shapeSize :: DelayedOpenExp env aenv sh
              -> Val env
              -> CodeGen (IR env aenv sh)
    shapeSize sh env = do
      let int           = numType :: NumType Int
          size []       = return $ constOp (num int 1)
          size [x]      = return x
          size (x:xs)   = foldM (A.mul int) x xs
      --
      sh' <- cvtE sh env
      sz  <- size sh'
      return [sz]

    -- Intersection of two shapes, taken as the minimum in each dimension.
    --
    intersect :: DelayedOpenExp env aenv sh
              -> DelayedOpenExp env aenv sh
              -> Val env
              -> CodeGen (IR env aenv sh)
    intersect sh1 sh2 env = do
      sh1' <- cvtE sh1 env
      sh2' <- cvtE sh2 env
      zipWithM (A.min (scalarType :: ScalarType Int)) sh1' sh2'

    -- Foreign scalar functions.
    --
    foreignE :: (Foreign f, Elt a, Elt b)
             => f a b
             -> DelayedOpenExp env aenv a
             -> Val env
             -> CodeGen (IR env aenv b)
    foreignE = error "todo: codegen/foreign expressions"


-- Helper functions
-- ----------------

-- Read a value from an array.
-- TODO: attach metedata node "!invariant.load" ?
--
readArray :: [Name] -> Operand -> CodeGen [Operand]
readArray = readArray' False

readVolatileArray :: [Name] -> Operand -> CodeGen [Operand]
readVolatileArray = readArray' True

readArray' :: Bool -> [Name] -> Operand -> CodeGen [Operand]
readArray' volatile arr i =
  forM arr $ \a -> do
    p <- instr $ GetElementPtr False (local a) [i] []
    v <- instr $ Load volatile p Nothing 0 []
    return v

-- Write elements into an array.
--
writeArray :: [Name] -> Operand -> [Operand] -> CodeGen ()
writeArray = writeArray' False

writeVolatileArray :: [Name] -> Operand -> [Operand] -> CodeGen ()
writeVolatileArray = writeArray' True

writeArray' :: Bool -> [Name] -> Operand -> [Operand] -> CodeGen ()
writeArray' volatile arr i val =
  zipWithM_ (\a v -> do
    p <- instr $ GetElementPtr False (local a) [i] []
    do_        $ Store volatile p v Nothing 0 []) arr val

-- Convert a multidimensional array index into a linear index
--
intOfIndex :: [Operand] -> [Operand] -> CodeGen Operand
intOfIndex extent idx = cvt (reverse extent) (reverse idx)
  where
    cvt []      []     = return (constOp $ num int 0)
    cvt [_]     [i]    = return i
    cvt (sz:sh) (i:ix) = do
      a <- cvt sh ix
      b <- A.mul (numType :: NumType Int) a sz
      c <- A.add (numType :: NumType Int) b i
      return c
    cvt _       _      =
      $internalError "cvt" "argument mismatch"

-- Convert a linear array index into a multidimensional array
--
indexOfInt :: [Operand] -> Operand -> CodeGen [Operand]
indexOfInt extent idx = reverse `fmap` cvt (reverse extent) idx
  where
    cvt []      _ = return [constOp $ num int 0]
    cvt [_]     i = return [i]  -- assert( i >= 0 && i < sh )
    cvt (sz:sh) i = do
      r  <- A.rem  int i sz
      i' <- A.quot int i sz
      rs <- cvt sh i'
      return (r:rs)


-- Primitive functions
-- ===================

-- | Generate llvm operations for primitive scalar functions
--
llvmOfPrimFun :: PrimFun f -> [Operand] -> CodeGen Operand
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
  $internalError "llvmOfPrimFun" "inconsistent valuation"


