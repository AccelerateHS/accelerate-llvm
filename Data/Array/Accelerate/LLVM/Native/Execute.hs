{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute
  where

-- llvm-general
import LLVM.General.Module
import LLVM.General.PassManager
import LLVM.General.ExecutionEngine
import LLVM.General.AST.Name
import qualified LLVM.General.AST                               as AST
import qualified LLVM.General.AST.Global                        as AST

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Representation               ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Interpreter                        ( evalPrim, evalPrimConst, evalPrj )
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Idx'(..), Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Monad                 ()
import Data.Array.Accelerate.LLVM.Native.Array.Data
import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.Native.Execute.Fill

import Data.Array.Accelerate.LLVM.Debug                         ( dump_exec )
import qualified Data.Array.Accelerate.LLVM.Debug               as Debug

-- library
import Prelude                                                  hiding ( exp )
import Control.Applicative                                      hiding ( Const )
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.Maybe
import Data.DList                                               ( DList )
import qualified Data.DList                                     as DL
import qualified Data.IntMap                                    as IM

import Foreign.Ptr
import Foreign.LibFFI                                           as FFI

#include "accelerate.h"


-- Valuation for an environment of array computations
--
data Aval env where
  Aempty :: Aval ()
  Apush  :: Aval env -> t -> Aval (env, t)


-- Projection of a value from a valuation using a de Bruijn index.
--
aprj :: Idx env t -> Aval env -> t
aprj ZeroIdx       (Apush _   x) = x
aprj (SuccIdx idx) (Apush val _) = aprj idx val
aprj _             _             = INTERNAL_ERROR(error) "aprj" "inconsistent valuation"


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
executeAcc :: Arrays a => ExecAcc Native a -> LLVM a
executeAcc acc = executeOpenAcc acc Aempty

executeAfun1 :: (Arrays a, Arrays b) => ExecAfun Native (a -> b) -> a -> LLVM b
executeAfun1 afun arrs = executeOpenAfun1 afun Aempty arrs


executeOpenAfun1
    :: PreOpenAfun (ExecOpenAcc Native) aenv (a -> b)
    -> Aval aenv
    -> a
    -> LLVM b
executeOpenAfun1 (Alam (Abody f)) aenv a = executeOpenAcc f (aenv `Apush` a)
executeOpenAfun1 _                _    _ = error "boop!"


-- Execute an open array computation
--
executeOpenAcc
    :: forall aenv arrs.
       ExecOpenAcc Native aenv arrs
    -> Aval aenv
    -> LLVM arrs
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
    Map _ a                     -> executeOp "map"       =<< extent a
    Generate sh _               -> executeOp "generate"  =<< travE sh
    Transform sh _ _ _          -> executeOp "transform" =<< travE sh
    Backpermute sh _ _          -> executeOp "transform" =<< travE sh

    -- Consumers

    -- Removed by fusion
    Replicate _ _ _             -> fusionError
    Slice _ _ _                 -> fusionError
    ZipWith _ _ _               -> fusionError

  where
    fusionError = INTERNAL_ERROR(error) "execute" "unexpected fusible matter"

    -- term traversals
    travA :: ExecOpenAcc Native aenv a -> LLVM a
    travA acc = executeOpenAcc acc aenv

    travE :: ExecExp Native aenv t -> LLVM t
    travE exp = executeExp exp aenv

    travT :: Atuple (ExecOpenAcc Native aenv) t -> LLVM t
    travT NilAtup        = return ()
    travT (SnocAtup t a) = (,) <$> travT t <*> travA a

    awhile :: PreOpenAfun (ExecOpenAcc Native) aenv (a -> Scalar Bool)
           -> PreOpenAfun (ExecOpenAcc Native) aenv (a -> a)
           -> a
           -> LLVM a
    awhile p f a = do
      r   <- executeOpenAfun1 p aenv a
      if indexArray r 0
         then awhile p f =<< executeOpenAfun1 f aenv a
         else return a

    -- get the extent of an embedded array
    extent :: Shape sh => ExecOpenAcc Native aenv (Array sh e) -> LLVM sh
    extent ExecAcc{}     = INTERNAL_ERROR(error) "executeOpenAcc" "expected delayed array"
    extent (EmbedAcc sh) = travE sh

    -- Skeleton implementation
    -- -----------------------

    -- Execute a skeleton that has no special requirements: thread decomposition
    -- is based on the given shape.
    --
    executeOp :: (Shape sh, Elt e) => Name -> sh -> LLVM (Array sh e)
    executeOp fn sh = do
      let out = allocateArray sh
      execute fn kernel gamma aenv (size sh) out
      return out


-- Scalar expression evaluation
-- ----------------------------

executeExp :: ExecExp Native aenv t -> Aval aenv -> LLVM t
executeExp exp aenv = executeOpenExp exp Empty aenv

executeOpenExp
    :: forall env aenv exp.
       ExecOpenExp Native env aenv exp
    -> Val env
    -> Aval aenv
    -> LLVM exp
executeOpenExp rootExp env aenv = travE rootExp
  where
    travE :: ExecOpenExp Native env aenv t -> LLVM t
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
      Index acc ix              -> (!)        <$> travA acc <*> travE ix
      LinearIndex acc ix        -> indexArray <$> travA acc <*> travE ix
--      Foreign _ f x             -> eforeign f x

    -- Helpers
    -- -------

    travT :: Tuple (ExecOpenExp Native env aenv) t -> LLVM t
    travT tup = case tup of
      NilTup            -> return ()
      SnocTup t e       -> (,) <$> travT t <*> travE e

    travA :: ExecOpenAcc Native aenv a -> LLVM a
    travA acc = executeOpenAcc acc aenv

--    eforeign :: ExecFun () (a -> b) -> ExecOpenExp Native env aenv a -> LLVM b
--    eforeign (Lam (Body f)) x = travE x >>= \e -> executeOpenExp f (Empty `Push` e) Aempty
--    eforeign _              _ = error "I bless the rains down in Africa"

    travF1 :: ExecOpenFun Native env aenv (a -> b) -> a -> LLVM b
    travF1 (Lam (Body f)) x = executeOpenExp f (env `Push` x) aenv
    travF1 _              _ = error "hayoooo~"

    while :: ExecOpenFun Native env aenv (a -> Bool) -> ExecOpenFun Native env aenv (a -> a) -> a -> LLVM a
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


-- Marshalling data
-- ----------------

-- Data which can be marshalled as function arguments to a kernel invocation.
--
class Marshalable a where
  marshal' :: a -> DList FFI.Arg

instance Marshalable () where
  marshal' () = DL.empty

instance ArrayElt e => Marshalable (ArrayData e) where
  marshal' adata = marshalR arrayElt adata
    where
      marshalR :: ArrayEltR e' -> ArrayData e' -> DList FFI.Arg
      marshalR ArrayEltRunit             _  = DL.empty
      marshalR (ArrayEltRpair aeR1 aeR2) ad =
        marshalR aeR1 (fstArrayData ad) `DL.append`
        marshalR aeR2 (sndArrayData ad)
      --
      marshalR ArrayEltRint     ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint8    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint16   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint32   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRint64   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword8   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword16  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword32  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRword64  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRfloat   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRdouble  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRchar    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcshort  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcushort ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcint    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcuint   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRclong   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRculong  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcllong  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcullong ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcchar   ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcschar  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcuchar  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcfloat  ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRcdouble ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)
      marshalR ArrayEltRbool    ad = DL.singleton $ FFI.argPtr (ptrsOfArrayData ad)

-- instance Shape sh => Marshalable sh where
--   marshal' sh = map FFI.argInt (reverse (shapeToList sh))

instance (Shape sh, Elt e) => Marshalable (Array sh e) where
  marshal' (Array sh adata) = marshal' adata `DL.append`
                              marshal' (reverse (R.shapeToList sh))

instance (Marshalable a, Marshalable b) => Marshalable (a, b) where
  marshal' (a, b) = marshal' a `DL.append` marshal' b

instance (Marshalable a, Marshalable b, Marshalable c) => Marshalable (a, b, c) where
  marshal' (a, b, c)
    = DL.concat [marshal' a, marshal' b, marshal' c]

instance (Marshalable a, Marshalable b, Marshalable c, Marshalable d) => Marshalable (a, b, c, d) where
  marshal' (a, b, c, d)
    = DL.concat [marshal' a, marshal' b, marshal' c, marshal' d]

instance Marshalable Int where
  marshal' x = DL.singleton (FFI.argInt x)

instance Marshalable a => Marshalable [a] where
  marshal' = DL.concat . map marshal'


marshal :: Marshalable a => a -> [FFI.Arg]
marshal = DL.toList . marshal'


-- Skeleton execution
-- ------------------

-- Generate FFI function arguments. The standard calling convention is
--
--   1. Starting index
--   2. Final index
--   3. Free array variables that were used
--   4. Any remaining parameters (typically explicit output arrays)
--
arguments :: Marshalable args => Gamma aenv -> Aval aenv -> args -> Int -> Int -> [FFI.Arg]
arguments gamma aenv a start end
  = FFI.argInt start
  : FFI.argInt end
  : concatMap (\(_, Idx' idx) -> marshal (aprj idx aenv)) (IM.elems gamma) ++ marshal a

-- JIT compile the LLVM code representing this kernel, link to the running
-- executable, and execute.
--
execute
    :: Marshalable args
    => Name
    -> ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Int
    -> args
    -> LLVM ()
execute main (NativeR ast) gamma aenv n a =
  jit main ast $ \f ->
  fillP n      $ \start end ->
    callFFI f retVoid (arguments gamma aenv a start end)


jit :: Name -> AST.Module -> (FunPtr () -> IO a) -> LLVM a
jit main ast run = do
  ctx   <- asks llvmContext
  liftIO . runError $
    withModuleFromAST ctx ast            $ \mdl   ->
    withMCJIT ctx opt model ptrelim fast $ \mcjit ->
    withPassManager passes               $ \pm    -> do
      void $ runPassManager pm mdl
      withModuleInEngine mcjit mdl       $ \exe   ->
        maybe (err "function not found") run =<< getFunction exe main
  where
    opt         = Just 3        -- optimisation level
    model       = Nothing       -- code model?
    ptrelim     = Nothing       -- True to disable frame pointer elimination
    fast        = Just True     -- True to enable fast instruction selection
    passes      = defaultCuratedPassSetSpec { optLevel = Just 3 }

    err msg     = INTERNAL_ERROR(error) "execute" msg
    runError e  = either err id `fmap` runErrorT e

