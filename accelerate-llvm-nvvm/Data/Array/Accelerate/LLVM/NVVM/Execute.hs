{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Execute
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Execute (

  executeAcc, executeAfun1,

) where

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Representation               ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                        hiding ( newArray, allocateArray )
import Data.Array.Accelerate.Interpreter                        ( evalPrim, evalPrimConst, evalPrj )
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.State

import Data.Array.Accelerate.LLVM.NVVM.Array.Data
import Data.Array.Accelerate.LLVM.NVVM.Execute.Environment
import Data.Array.Accelerate.LLVM.NVVM.Execute.Marshal
import Data.Array.Accelerate.LLVM.NVVM.Target
import qualified Data.Array.Accelerate.LLVM.NVVM.Execute.Event  as Event

import qualified Data.Array.Accelerate.LLVM.NVVM.Debug          as Debug

-- cuda
import qualified Foreign.CUDA.Driver                            as CUDA

-- library
import Prelude                                                  hiding ( exp )
import Control.Applicative                                      hiding ( Const )
import Control.Monad
import Control.Monad.Error
import Text.Printf

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
executeAcc :: Arrays a => ExecAcc NVVM a -> LLVM NVVM a
executeAcc acc = streaming (executeOpenAcc acc Aempty) wait

executeAfun1 :: (Arrays a, Arrays b) => ExecAfun NVVM (a -> b) -> a -> LLVM NVVM b
executeAfun1 afun arrs =
  streaming (useArrays (arrays arrs) (fromArr arrs))
            (\(Async event ()) -> executeOpenAfun1 afun Aempty (Async event arrs))
  where
    useArrays :: ArraysR arrs -> arrs -> Stream -> LLVM NVVM ()
    useArrays ArraysRunit         ()       _  = return ()
    useArrays (ArraysRpair r1 r0) (a1, a0) st = useArrays r1 a1 st >> useArrays r0 a0 st
    useArrays ArraysRarray        arr      st = useArrayAsync arr (Just st)


executeOpenAfun1
    :: PreOpenAfun (ExecOpenAcc NVVM) aenv (a -> b)
    -> Aval aenv
    -> Async a
    -> LLVM NVVM b
executeOpenAfun1 (Alam (Abody f)) aenv a = streaming (executeOpenAcc f (aenv `Apush` a)) wait
executeOpenAfun1 _                _    _ = error "boop!"


-- Execute an open array computation
--
executeOpenAcc
    :: forall aenv arrs.
       ExecOpenAcc NVVM aenv arrs
    -> Aval aenv
    -> Stream
    -> LLVM NVVM arrs
executeOpenAcc EmbedAcc{} _ _ =
  INTERNAL_ERROR(error) "execute" "unexpected delayed array"
executeOpenAcc (ExecAcc nvvm gamma pacc) aenv stream =
  case pacc of

    -- Array introduction
    Use arr                     -> return (toArr arr)
    Unit x                      -> newArray Z . const =<< travE x

    -- Environment manipulation
    Avar ix                     -> after stream (aprj ix aenv)
    Alet bnd body               -> streaming (executeOpenAcc bnd aenv) (\x -> executeOpenAcc body (aenv `Apush` x) stream)
    Apply f a                   -> streaming (executeOpenAcc a aenv)   (executeOpenAfun1 f aenv)
    Atuple tup                  -> toTuple <$> travT tup
    Aprj ix tup                 -> evalPrj ix . fromTuple <$> travA tup
    Acond p t e                 -> travE p >>= \x -> if x then travA t else travA e
    Awhile p f a                -> awhile p f =<< travA a

    -- Foreign
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
    Replicate{}                 -> fusionError
    Slice{}                     -> fusionError
    ZipWith{}                   -> fusionError

  where
    fusionError = INTERNAL_ERROR(error) "execute" "unexpected fusible matter"

    -- Term traversals
    -- ---------------
    travA :: ExecOpenAcc NVVM aenv a -> LLVM NVVM a
    travA acc = executeOpenAcc acc aenv stream

    travE :: ExecExp NVVM aenv t -> LLVM NVVM t
    travE exp = executeExp exp aenv stream

    travT :: Atuple (ExecOpenAcc NVVM aenv) t -> LLVM NVVM t
    travT NilAtup        = return ()
    travT (SnocAtup t a) = (,) <$> travT t <*> travA a

    -- get the extent of an embedded array
    extent :: Shape sh => ExecOpenAcc NVVM aenv (Array sh e) -> LLVM NVVM sh
    extent ExecAcc{}     = INTERNAL_ERROR(error) "executeOpenAcc" "expected delayed array"
    extent (EmbedAcc sh) = travE sh

    -- control flow
    awhile :: PreOpenAfun (ExecOpenAcc NVVM) aenv (a -> Scalar Bool)
           -> PreOpenAfun (ExecOpenAcc NVVM) aenv (a -> a)
           -> a
           -> LLVM NVVM a
    awhile p f a = do
      nop <- liftIO Event.create                -- record event never called, so this is a functional no-op
      r   <- executeOpenAfun1 p aenv (Async nop a)
      ok  <- indexArray r 0
      if ok then awhile p f =<< executeOpenAfun1 f aenv (Async nop a)
            else return a

    -- Skeleton implementation
    -- -----------------------

    -- Execute a skeleton that has no special requirements: thread decomposition
    -- is based on the given shape.
    --
    executeOp :: (Shape sh, Elt e) => sh -> LLVM NVVM (Array sh e)
    executeOp sh = do
      let kernel = case nvvmKernel nvvm of
                      k:_ -> k
                      _   -> INTERNAL_ERROR(error) "executeOp" "kernel not found"
      --
      out <- allocateArray sh
      execute kernel gamma aenv stream (size sh) out
      return out

    -- Change the shape of an array without altering its contents. This does not
    -- execute any kernel programs.
    --
    reshapeOp :: Shape sh => sh -> Array sh' e -> Array sh e
    reshapeOp sh (Array sh' adata)
      = BOUNDS_CHECK(check) "reshape" "shape mismatch" (size sh == R.size sh')
      $ Array (fromElt sh) adata

    -- Executing fold operations depend on whether we are recursively collapsing
    -- to a single value using multiple thread blocks, or a multidimensional
    -- single-pass reduction where there is one block per inner dimension.
    --
    fold1Op :: (Shape sh, Elt e) => (sh :. Int) -> LLVM NVVM (Array sh e)
    fold1Op sh@(_ :. sz)
      = BOUNDS_CHECK(check) "fold1" "empty array" (sz > 0)
      $ foldCore sh

    foldOp :: (Shape sh, Elt e) => (sh :. Int) -> LLVM NVVM (Array sh e)
    foldOp (sh :. sz)
      = foldCore ((listToShape . map (max 1) . shapeToList $ sh) :. sz)

    foldCore :: (Shape sh, Elt e) => (sh :. Int) -> LLVM NVVM (Array sh e)
    foldCore sh'@(sh :. _)
      | dim sh > 0      = executeOp sh
      | otherwise       = foldAllOp sh'

    -- See note: [Marshalling foldAll output arrays]
    --
    foldAllOp :: forall sh e. (Shape sh, Elt e) => (sh :. Int) -> LLVM NVVM (Array sh e)
    foldAllOp sh'
      | k1:k2:_ <- nvvmKernel nvvm
      = let
            foldIntro :: (sh :. Int) -> LLVM NVVM (Array sh e)
            foldIntro (sh:.sz) = do
              let numElements   = size sh * sz
                  numBlocks     = (kernelThreadBlocks k1) numElements

              out <- allocateArray (sh :. numBlocks)
              execute k1 gamma aenv stream numElements out
              foldRec out

            foldRec :: Array (sh :. Int) e -> LLVM NVVM (Array sh e)
            foldRec out@(Array (sh,sz) adata) =
              let numElements   = R.size sh * sz
                  numBlocks     = (kernelThreadBlocks k2) numElements
              in if sz <= 1
                    then do
                      -- We have recursed to a single block already. Trim the
                      -- intermediate working vector to the final scalar array.
                      return $! Array sh adata

                    else do
                      -- Keep cooperatively reducing the output array in-place.
                      -- Note that we must continue to update the tracked size
                      -- so the recursion knows when to stop.
                      execute k2 gamma aenv stream numElements out
                      foldRec $! Array (sh,numBlocks) adata
        in
        foldIntro sh'

      | otherwise
      = INTERNAL_ERROR(error) "foldAllOp" "kernel not found"


-- Scalar expression evaluation
-- ----------------------------

executeExp :: ExecExp NVVM aenv t -> Aval aenv -> Stream -> LLVM NVVM t
executeExp exp aenv stream = executeOpenExp exp Empty aenv stream

executeOpenExp
    :: forall env aenv exp.
       ExecOpenExp NVVM env aenv exp
    -> Val env
    -> Aval aenv
    -> Stream
    -> LLVM NVVM exp
executeOpenExp rootExp env aenv stream = travE rootExp
  where
    travE :: ExecOpenExp NVVM env aenv t -> LLVM NVVM t
    travE exp = case exp of
      Var ix                    -> return (prj ix env)
      Let bnd body              -> travE bnd >>= \x -> executeOpenExp body (env `Push` x) aenv stream
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
      Index acc ix              -> join $ index      <$> travA acc <*> travE ix
      LinearIndex acc ix        -> join $ indexArray <$> travA acc <*> travE ix
      Foreign _ f x             -> eforeign f x

    -- Helpers
    -- -------

    travT :: Tuple (ExecOpenExp NVVM env aenv) t -> LLVM NVVM t
    travT tup = case tup of
      NilTup            -> return ()
      SnocTup t e       -> (,) <$> travT t <*> travE e

    travA :: ExecOpenAcc NVVM aenv a -> LLVM NVVM a
    travA acc = executeOpenAcc acc aenv stream

    eforeign :: ExecFun NVVM () (a -> b) -> ExecOpenExp NVVM env aenv a -> LLVM NVVM b
    eforeign _ _ = error "todo: execute Foreign"
--    eforeign (Lam (Body f)) x = travE x >>= \e -> executeOpenExp f (Empty `Push` e) Aempty
--    eforeign _              _ = error "I bless the rains down in Africa"

    travF1 :: ExecOpenFun NVVM env aenv (a -> b) -> a -> LLVM NVVM b
    travF1 (Lam (Body f)) x = executeOpenExp f (env `Push` x) aenv stream
    travF1 _              _ = error "LANAAAAAAAA!"

    while :: ExecOpenFun NVVM env aenv (a -> Bool) -> ExecOpenFun NVVM env aenv (a -> a) -> a -> LLVM NVVM a
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

    index :: (Shape sh, Elt e) => Array sh e -> sh -> LLVM NVVM e
    index arr ix = indexArray arr (toIndex (shape arr) ix)


-- Skeleton execution
-- ------------------

-- Execute the function implementing this kernel.
--
execute
    :: Marshalable args
    => Kernel
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Int
    -> args
    -> LLVM NVVM ()
execute kernel gamma aenv stream n args =
  launch kernel stream n =<< marshal stream (args, (gamma,aenv))


-- Execute a device function with the given thread configuration and function
-- parameters.
--
launch :: Kernel -> Stream -> Int -> [CUDA.FunParam] -> LLVM NVVM ()
launch Kernel{..} stream n args =
  liftIO $ Debug.timed Debug.dump_exec msg (Just stream)
         $ CUDA.launchKernel kernelFun grid cta smem (Just stream) args
  where
    cta         = (kernelThreadBlockSize, 1, 1)
    grid        = (kernelThreadBlocks n, 1, 1)
    smem        = kernelSharedMemBytes

    fst3 (x,_,_)        = x
    msg gpuTime wallTime =
      printf "exec: %s <<< %d, %d, %d >>> %s"
             kernelName (fst3 grid) (fst3 cta) smem (Debug.elapsed gpuTime wallTime)

