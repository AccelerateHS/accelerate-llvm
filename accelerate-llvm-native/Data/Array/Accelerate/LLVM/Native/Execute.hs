{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute (

  executeAcc, executeAfun1,

) where

-- llvm-general
import LLVM.General.Module
import LLVM.General.PassManager
import LLVM.General.ExecutionEngine
import LLVM.General.AST.Name
import qualified LLVM.General.AST                               as AST

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Representation               ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Interpreter                        ( evalPrim, evalPrimConst, evalPrj )
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Monad                 ()
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.Native.Array.Data
import Data.Array.Accelerate.LLVM.Native.Execute.Environment
import Data.Array.Accelerate.LLVM.Native.Execute.Fill
import Data.Array.Accelerate.LLVM.Native.Execute.Gang
import Data.Array.Accelerate.LLVM.Native.Execute.Marshal
import Data.Array.Accelerate.LLVM.Native.Target

-- library
import Prelude                                                  hiding ( exp )
import Control.Applicative                                      hiding ( Const )
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader

import Foreign.Ptr
import Foreign.LibFFI                                           as FFI

#if !MIN_VERSION_llvm_general(3,3,0)
import Data.Word
import Data.Maybe
import qualified LLVM.General.Context                           as LLVM
#endif

#include "accelerate.h"


-- Array expression evaluation
-- ---------------------------

-- Computations are evaluated by traversing the AST bottom up, and for each node
-- distinguishing between three cases:
--
--  1. If it is a Use node, we return a reference to the array data.
--      a) Even though we execute with multiple cores, we assume a shared memory
--         multiprocessor machine.
--      b) However, if we are executing in a heterogeneous setup, this may
--         require coping data back from the GPU.
--
--  2. If it is a non-skeleton node, such as a let binding or shape conversion,
--     then execute directly by updating the environment or similar.
--
--  3. If it is a skeleton node, then we need to execute the generated LLVM
--     code. This entails:
--      a) lowering the LLVM AST into C++ objects
--      b) building an executable module with MCJIT
--      c) linking the returned function pointer into the running code
--      d) evaluate the function with the thread gang.
--
executeAcc :: Arrays a => ExecAcc Native a -> LLVM Native a
executeAcc acc = executeOpenAcc acc Aempty

executeAfun1 :: (Arrays a, Arrays b) => ExecAfun Native (a -> b) -> a -> LLVM Native b
executeAfun1 afun arrs = executeOpenAfun1 afun Aempty arrs


executeOpenAfun1
    :: PreOpenAfun (ExecOpenAcc Native) aenv (a -> b)
    -> Aval aenv
    -> a
    -> LLVM Native b
executeOpenAfun1 (Alam (Abody f)) aenv a = executeOpenAcc f (aenv `Apush` a)
executeOpenAfun1 _                _    _ = error "boop!"


-- Execute an open array computation
--
executeOpenAcc
    :: forall aenv arrs.
       ExecOpenAcc Native aenv arrs
    -> Aval aenv
    -> LLVM Native arrs
executeOpenAcc EmbedAcc{} _ =
  INTERNAL_ERROR(error) "execute" "unexpected delayed array"
executeOpenAcc (ExecAcc kernel gamma pacc) aenv =
  case pacc of

    -- Array introduction
    Use arr                     -> return (toArr arr)
    Unit x                      -> newArray Z . const <$> travE x

    -- Environment manipulation
    Avar ix                     -> return (aprj ix aenv)
    Alet bnd body               -> travA bnd >>= \x -> executeOpenAcc body (aenv `Apush` x)
    Apply f a                   -> travA a   >>= \x -> executeOpenAfun1 f aenv x
    Atuple tup                  -> toTuple <$> travT tup
    Aprj ix tup                 -> evalPrj ix . fromTuple <$> travA tup
    Acond p t e                 -> travE p >>= \x -> if x then travA t else travA e
    Awhile p f a                -> awhile p f =<< travA a

    -- Foreign function
    Aforeign _ff _afun _a       -> error "todo: execute Aforeign"

    -- Producers
    Map _ a                     -> executeOp =<< extent a
    Generate sh _               -> executeOp =<< travE sh
    Transform sh _ _ _          -> executeOp =<< travE sh
    Backpermute sh _ _          -> executeOp =<< travE sh
    Reshape sh a                -> reshapeOp <$> travE sh <*> travA a

    -- Consumers
    Fold _ _ a                  -> foldOp  =<< extent a
    Fold1 _ a                   -> fold1Op =<< extent a

    -- Removed by fusion
    Replicate _ _ _             -> fusionError
    Slice _ _ _                 -> fusionError
    ZipWith _ _ _               -> fusionError

  where
    fusionError = INTERNAL_ERROR(error) "execute" "unexpected fusible matter"

    -- term traversals
    travA :: ExecOpenAcc Native aenv a -> LLVM Native a
    travA acc = executeOpenAcc acc aenv

    travE :: ExecExp Native aenv t -> LLVM Native t
    travE exp = executeExp exp aenv

    travT :: Atuple (ExecOpenAcc Native aenv) t -> LLVM Native t
    travT NilAtup        = return ()
    travT (SnocAtup t a) = (,) <$> travT t <*> travA a

    awhile :: PreOpenAfun (ExecOpenAcc Native) aenv (a -> Scalar Bool)
           -> PreOpenAfun (ExecOpenAcc Native) aenv (a -> a)
           -> a
           -> LLVM Native a
    awhile p f a = do
      r   <- executeOpenAfun1 p aenv a
      if indexArray r 0
         then awhile p f =<< executeOpenAfun1 f aenv a
         else return a

    -- get the extent of an embedded array
    extent :: Shape sh => ExecOpenAcc Native aenv (Array sh e) -> LLVM Native sh
    extent ExecAcc{}     = INTERNAL_ERROR(error) "executeOpenAcc" "expected delayed array"
    extent (EmbedAcc sh) = travE sh

    -- Skeleton implementation
    -- -----------------------

    -- Execute a skeleton that has no special requirements: thread decomposition
    -- is based on the given shape.
    --
    executeOp :: (Shape sh, Elt e) => sh -> LLVM Native (Array sh e)
    executeOp = executeOpWith ()

    executeOpWith :: (Shape sh, Elt e, Marshalable args) => args -> sh -> LLVM Native (Array sh e)
    executeOpWith args sh = do
      let out = allocateArray sh
      execute kernel gamma aenv (size sh) (args,out)
      return out

    -- Change the shape of an array without altering its contents. This does not
    -- execute any kernel programs.
    --
    reshapeOp :: Shape sh => sh -> Array sh' e -> Array sh e
    reshapeOp sh (Array sh' adata)
      = BOUNDS_CHECK(check) "reshape" "shape mismatch" (size sh == R.size sh')
      $ Array (fromElt sh) adata

    -- Execute fold operations. There are two flavours:
    --
    --   1. If we are collapsing to a single value, then the threads compute an
    --   individual partial sum, then a single thread adds the results.
    --
    --   2. If this is a multidimensional reduction, then threads reduce the
    --   inner dimensions sequentially.
    --
    fold1Op :: (Shape sh, Elt e) => (sh :. Int) -> LLVM Native (Array sh e)
    fold1Op sh@(_ :. sz)
      = BOUNDS_CHECK(check) "fold1" "empty array" (sz > 0)
      $ foldCore sh

    -- Make space for the neutral element
    foldOp :: (Shape sh, Elt e) => (sh :. Int) -> LLVM Native (Array sh e)
    foldOp (sh :. sz)
      = foldCore ((listToShape . map (max 1) . shapeToList $ sh) :. sz)

    foldCore :: forall sh e. (Shape sh, Elt e) => (sh :. Int) -> LLVM Native (Array sh e)
    foldCore (sh :. sz)
      -- Either (1) multidimensional reduction; or
      --        (2) sequential reduction
      | dim sh > 0 || gangSize theGang == 1
      = do executeOpWith sz sh

      -- Parallel reduction
      | otherwise
      = do  let chunks  = gangSize theGang
                tmp     = allocateArray (sh :. chunks)  :: Array (sh:.Int) e
                out     = allocateArray sh
                n       = sz `min` chunks
            --
            compile kernel $ \ee -> do
              link ee "fold1" $ \f ->
                fillP (size sh * sz) $ \start end tid ->
                  callFFI f retVoid (marshal (start,end,tid,tmp,(gamma,aenv)))
              link ee "foldAll" $ \f ->
                  callFFI f retVoid (marshal (0::Int,n,tmp,out,(gamma,aenv)))

            return out


-- Scalar expression evaluation
-- ----------------------------

executeExp :: ExecExp Native aenv t -> Aval aenv -> LLVM Native t
executeExp exp aenv = executeOpenExp exp Empty aenv

executeOpenExp
    :: forall env aenv exp.
       ExecOpenExp Native env aenv exp
    -> Val env
    -> Aval aenv
    -> LLVM Native exp
executeOpenExp rootExp env aenv = travE rootExp
  where
    travE :: ExecOpenExp Native env aenv t -> LLVM Native t
    travE exp = case exp of
      Var ix                    -> return (prj ix env)
      Let bnd body              -> travE bnd >>= \x -> executeOpenExp body (env `Push` x) aenv
      Const c                   -> return (toElt c)
      PrimConst c               -> return (evalPrimConst c)
      PrimApp f x               -> evalPrim f <$> travE x
      Tuple t                   -> toTuple <$> travT t
      Prj ix e                  -> evalPrj ix . fromTuple <$> travE e
      Cond p t e                -> travE p >>= \x -> if x then travE t else travE e
      While p f x               -> while p f =<< travE x
      IndexAny                  -> return Any
      IndexNil                  -> return Z
      IndexCons sh sz           -> (:.) <$> travE sh <*> travE sz
      IndexHead sh              -> (\(_  :. ix) -> ix) <$> travE sh
      IndexTail sh              -> (\(ix :.  _) -> ix) <$> travE sh
      IndexSlice ix slix sh     -> indexSlice ix <$> travE slix <*> travE sh
      IndexFull ix slix sl      -> indexFull  ix <$> travE slix <*> travE sl
      ToIndex sh ix             -> toIndex   <$> travE sh  <*> travE ix
      FromIndex sh ix           -> fromIndex <$> travE sh  <*> travE ix
      Intersect sh1 sh2         -> intersect <$> travE sh1 <*> travE sh2
      ShapeSize sh              -> size  <$> travE sh
      Shape acc                 -> shape <$> travA acc
      Index acc ix              -> index      <$> travA acc <*> travE ix
      LinearIndex acc ix        -> indexArray <$> travA acc <*> travE ix
      Foreign _ f x             -> eforeign f x

    -- Helpers
    -- -------

    travT :: Tuple (ExecOpenExp Native env aenv) t -> LLVM Native t
    travT tup = case tup of
      NilTup            -> return ()
      SnocTup t e       -> (,) <$> travT t <*> travE e

    travA :: ExecOpenAcc Native aenv a -> LLVM Native a
    travA acc = executeOpenAcc acc aenv

    eforeign :: ExecFun Native () (a -> b) -> ExecOpenExp Native env aenv a -> LLVM Native b
    eforeign _ _ = error "todo: execute Foreign"
--    eforeign (Lam (Body f)) x = travE x >>= \e -> executeOpenExp f (Empty `Push` e) Aempty
--    eforeign _              _ = error "I bless the rains down in Africa"

    travF1 :: ExecOpenFun Native env aenv (a -> b) -> a -> LLVM Native b
    travF1 (Lam (Body f)) x = executeOpenExp f (env `Push` x) aenv
    travF1 _              _ = error "hayoooo~"

    while :: ExecOpenFun Native env aenv (a -> Bool) -> ExecOpenFun Native env aenv (a -> a) -> a -> LLVM Native a
    while p f x = do
      ok <- travF1 p x
      if ok then while p f =<< travF1 f x
            else return x

    indexSlice :: (Elt slix, Elt sh, Elt sl)
               => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
               -> slix
               -> sh
               -> sl
    indexSlice ix slix sh = toElt $ restrict ix (fromElt slix) (fromElt sh)
      where
        restrict :: SliceIndex slix sl co sh -> slix -> sh -> sl
        restrict SliceNil              ()        ()       = ()
        restrict (SliceAll   sliceIdx) (slx, ()) (sl, sz) = (restrict sliceIdx slx sl, sz)
        restrict (SliceFixed sliceIdx) (slx,  _) (sl,  _) = restrict sliceIdx slx sl

    indexFull :: (Elt slix, Elt sh, Elt sl)
              => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
              -> slix
              -> sl
              -> sh
    indexFull ix slix sl = toElt $ extend ix (fromElt slix) (fromElt sl)
      where
        extend :: SliceIndex slix sl co sh -> slix -> sl -> sh
        extend SliceNil              ()        ()       = ()
        extend (SliceAll sliceIdx)   (slx, ()) (sh, sz) = (extend sliceIdx slx sh, sz)
        extend (SliceFixed sliceIdx) (slx, sz) sh       = (extend sliceIdx slx sh, sz)

    index :: (Shape sh, Elt e) => Array sh e -> sh -> e
    index arr ix = indexArray arr (toIndex (shape arr) ix)


-- Skeleton execution
-- ------------------

-- JIT compile the LLVM code representing this kernel, link to the running
-- executable, and execute the main function using the 'fillP' method to
-- distribute work evenly amongst the threads.
--
execute
    :: Marshalable args
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Int
    -> args
    -> LLVM Native ()
execute kernel@(NativeR ast) gamma aenv n args =
  let main = Name (AST.moduleName ast) in
  compile kernel $ \ee ->
  link ee main   $ \f  ->
  fillP n        $ \start end _ ->
    callFFI f retVoid (marshal (start, end, args, (gamma,aenv)))


link :: ExecutableModule MCJIT -> Name -> (FunPtr () -> IO a) -> IO a
link exe main run =
  maybe (INTERNAL_ERROR(error) "link" "function not found") run =<< getFunction exe main


compile :: ExecutableR Native -> (ExecutableModule MCJIT -> IO a) -> LLVM Native a
compile (NativeR ast) cont = do
  ctx   <- asks llvmContext
  liftIO . runError $
    withModuleFromAST ctx ast            $ \mdl   ->
    withMCJIT ctx opt model ptrelim fast $ \mcjit ->
    withPassManager passes               $ \pm    -> do
      void $ runPassManager pm mdl
      withModuleInEngine mcjit mdl       $ \ee    ->
        cont ee
  where
    opt         = Just 3        -- optimisation level
    model       = Nothing       -- code model?
    ptrelim     = Nothing       -- True to disable frame pointer elimination
    fast        = Just True     -- True to enable fast instruction selection
    passes      = defaultCuratedPassSetSpec { optLevel = Just 3 }
    runError e  = either (INTERNAL_ERROR(error) "execute") id `fmap` runErrorT e


#if !MIN_VERSION_llvm_general(3,3,0)
-- Shims to support the older LLVM-3.2, which are required if the user is using
-- libNVVM to optimise NVPTX code.
--
type MCJIT = JIT

withMCJIT
    :: LLVM.Context
    -> Maybe Word
    -> model
    -> fpe
    -> fis
    -> (MCJIT -> IO a)
    -> IO a
withMCJIT ctx opt _ _ _ action =
  withJIT ctx (fromMaybe 0 opt) action
#endif

