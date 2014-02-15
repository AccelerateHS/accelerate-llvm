{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Compile
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Compile
  where

-- llvm-general
import LLVM.General                                             as G
import LLVM.General.Analysis
import LLVM.General.PassManager

-- accelerate
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.CodeGen                       ( llvmOfAcc )
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Monad                 as A
import Data.Array.Accelerate.LLVM.State

-- standard library
import Prelude                                                  hiding ( exp )
import Control.Applicative                                      hiding ( Const )
import Control.Monad                                            ( when, void )
import Control.Monad.Error                                      ( runErrorT )
import Control.Monad.Reader                                     ( asks )
import Control.Monad.Trans                                      ( liftIO )
import Data.IntMap                                              ( IntMap )
import Data.Monoid

#include "accelerate.h"


-- | Initialise code generation, compilation, and data transfer (if required)
-- for an array expression. The returned array computation is annotated to be
-- suitable for execution on the target:
--
--   * A list of the array variables embedded within scalar expressions
--
--   * The compiled LLVM code required to execute the kernel
--
compileAcc :: DelayedAcc a -> LLVM (ExecAcc a)
compileAcc = compileOpenAcc

compileAfun :: DelayedAfun f -> LLVM (ExecAfun f)
compileAfun = compileOpenAfun


compileOpenAfun :: DelayedOpenAfun aenv f -> LLVM (PreOpenAfun ExecOpenAcc aenv f)
compileOpenAfun (Alam l)  = Alam  <$> compileOpenAfun l
compileOpenAfun (Abody b) = Abody <$> compileOpenAcc b


compileOpenAcc :: forall _aenv _a. DelayedOpenAcc _aenv _a -> LLVM (ExecOpenAcc _aenv _a)
compileOpenAcc = traverseAcc
  where
    -- Traverse an open array expression in depth-first order. The top-level
    -- function 'traverseAcc' is intended for manifest arrays that we will
    -- generate LLVM code for. Array valued sub-terms, which might be manifest
    -- or delayed, are handled separately.
    --
    -- As the AST is traversed, we also collect a set of the indices of free
    -- array variables that were referred to within scalar sub-expressions.
    -- These will be required during code generation and execution.
    --
    traverseAcc :: forall aenv arrs. DelayedOpenAcc aenv arrs -> LLVM (ExecOpenAcc aenv arrs)
    traverseAcc Delayed{}              = INTERNAL_ERROR(error) "compileOpenAcc" "unexpected delayed array"
    traverseAcc topAcc@(Manifest pacc) =
      case pacc of
        -- Environment and control flow
        Avar ix                 -> node $ pure (Avar ix)
        Alet a b                -> node . pure =<< Alet         <$> traverseAcc a <*> traverseAcc b
        Apply f a               -> node =<< liftA2 Apply        <$> travAF f <*> travA a
        Awhile p f a            -> node =<< liftA3 Awhile       <$> travAF p <*> travAF f <*> travA a
        Acond p t e             -> node =<< liftA3 Acond        <$> travE  p <*> travA  t <*> travA e
        Atuple tup              -> node =<< liftA Atuple        <$> travAtup tup
        Aprj ix tup             -> node =<< liftA (Aprj ix)     <$> travA    tup

        -- Foreign
--        Aforeign ff afun a      -> node =<< foreignA ff afun a

        -- Array injection
        Unit e                  -> node =<< liftA  Unit         <$> travE e
        Use arrs                -> node $ pure (Use arrs)       -- TODO: remote memory management

        -- Index space transforms
        Reshape s a             -> node =<< liftA2 Reshape              <$> travE s <*> travA a
        Replicate slix e a      -> exec =<< liftA2 (Replicate slix)     <$> travE e <*> travA a
        Slice slix a e          -> exec =<< liftA2 (Slice slix)         <$> travA a <*> travE e
        Backpermute e f a       -> exec =<< liftA3 Backpermute          <$> travE e <*> travF f <*> travA a

        -- Producers
        Generate e f            -> exec =<< liftA2 Generate             <$> travE e <*> travF f
        Map f a                 -> exec =<< liftA2 Map                  <$> travF f <*> travA a
        ZipWith f a b           -> exec =<< liftA3 ZipWith              <$> travF f <*> travA a <*> travA b
        Transform e p f a       -> exec =<< liftA4 Transform            <$> travE e <*> travF p <*> travF f <*> travA a

        -- Consumers
        Fold f z a              -> exec =<< liftA3 Fold                 <$> travF f <*> travE z <*> travA a
        Fold1 f a               -> exec =<< liftA2 Fold1                <$> travF f <*> travA a
        FoldSeg f e a s         -> exec =<< liftA4 FoldSeg              <$> travF f <*> travE e <*> travA a <*> travA s
        Fold1Seg f a s          -> exec =<< liftA3 Fold1Seg             <$> travF f <*> travA a <*> travA s
        Scanl f e a             -> exec =<< liftA3 Scanl                <$> travF f <*> travE e <*> travA a
        Scanl' f e a            -> exec =<< liftA3 Scanl'               <$> travF f <*> travE e <*> travA a
        Scanl1 f a              -> exec =<< liftA2 Scanl1               <$> travF f <*> travA a
        Scanr f e a             -> exec =<< liftA3 Scanr                <$> travF f <*> travE e <*> travA a
        Scanr' f e a            -> exec =<< liftA3 Scanr'               <$> travF f <*> travE e <*> travA a
        Scanr1 f a              -> exec =<< liftA2 Scanr1               <$> travF f <*> travA a
        Permute f d g a         -> exec =<< liftA4 Permute              <$> travF f <*> travA d <*> travF g <*> travA a
        Stencil f b a           -> exec =<< liftA2 (flip Stencil b)     <$> travF f <*> travA a
        Stencil2 f b1 a1 b2 a2  -> exec =<< liftA3 stencil2             <$> travF f <*> travA a1 <*> travA a2
          where stencil2 f' a1' a2' = Stencil2 f' b1 a1' b2 a2'

      where
        travA :: DelayedOpenAcc aenv a -> LLVM (IntMap (Idx' aenv), ExecOpenAcc aenv a)
        travA acc = case acc of
          Manifest{}    -> pure                    <$> traverseAcc acc
          Delayed{..}   -> liftA2 (const EmbedAcc) <$> travF indexD <*> travE extentD

        travAF :: DelayedOpenAfun aenv f
               -> LLVM (IntMap (Idx' aenv), PreOpenAfun ExecOpenAcc aenv f)
        travAF afun = pure <$> compileOpenAfun afun

        travAtup :: Atuple (DelayedOpenAcc aenv) a
                 -> LLVM (IntMap (Idx' aenv), Atuple (ExecOpenAcc aenv) a)
        travAtup NilAtup        = return (pure NilAtup)
        travAtup (SnocAtup t a) = liftA2 SnocAtup <$> travAtup t <*> travA a

        travF :: DelayedOpenFun env aenv t
              -> LLVM (IntMap (Idx' aenv), PreOpenFun ExecOpenAcc env aenv t)
        travF (Body b)  = liftA Body <$> travE b
        travF (Lam  f)  = liftA Lam  <$> travF f

        exec :: (IntMap (Idx' aenv), PreOpenAcc ExecOpenAcc aenv arrs)
             -> LLVM (ExecOpenAcc aenv arrs)
        exec (aenv, eacc) = do
          let aval = makeAval aenv
          kernel <- build topAcc aval
          return $! ExecAcc kernel aval eacc

        node :: (IntMap (Idx' aenv'), PreOpenAcc ExecOpenAcc aenv' arrs')
             -> LLVM (ExecOpenAcc aenv' arrs')
        node = fmap snd . wrap

        wrap :: (IntMap (Idx' aenv'), PreOpenAcc ExecOpenAcc aenv' arrs')
             -> LLVM (IntMap (Idx' aenv'), ExecOpenAcc aenv' arrs')
        wrap = return . liftA (ExecAcc noKernel mempty)

        noKernel :: G.Module
        noKernel =  INTERNAL_ERROR(error) "compile" "no kernel module for this node"


    -- Traverse a scalar expression
    --
    travE :: DelayedOpenExp env aenv e
          -> LLVM (IntMap (Idx' aenv), PreOpenExp ExecOpenAcc env aenv e)
    travE exp =
      case exp of
        Var ix                  -> return $ pure (Var ix)
        Const c                 -> return $ pure (Const c)
        PrimConst c             -> return $ pure (PrimConst c)
        IndexAny                -> return $ pure IndexAny
        IndexNil                -> return $ pure IndexNil
--        Foreign ff f x          -> foreignE ff f x
        --
        Let a b                 -> liftA2 Let                   <$> travE a <*> travE b
        IndexCons t h           -> liftA2 IndexCons             <$> travE t <*> travE h
        IndexHead h             -> liftA  IndexHead             <$> travE h
        IndexTail t             -> liftA  IndexTail             <$> travE t
        IndexSlice slix x s     -> liftA2 (IndexSlice slix)     <$> travE x <*> travE s
        IndexFull slix x s      -> liftA2 (IndexFull slix)      <$> travE x <*> travE s
        ToIndex s i             -> liftA2 ToIndex               <$> travE s <*> travE i
        FromIndex s i           -> liftA2 FromIndex             <$> travE s <*> travE i
        Tuple t                 -> liftA  Tuple                 <$> travT t
        Prj ix e                -> liftA  (Prj ix)              <$> travE e
        Cond p t e              -> liftA3 Cond                  <$> travE p <*> travE t <*> travE e
        While p f x             -> liftA3 While                 <$> travF p <*> travF f <*> travE x
        PrimApp f e             -> liftA  (PrimApp f)           <$> travE e
        Index a e               -> liftA2 Index                 <$> travA a <*> travE e
        LinearIndex a e         -> liftA2 LinearIndex           <$> travA a <*> travE e
        Shape a                 -> liftA  Shape                 <$> travA a
        ShapeSize e             -> liftA  ShapeSize             <$> travE e
        Intersect x y           -> liftA2 Intersect             <$> travE x <*> travE y

      where
        travA :: (Shape sh, Elt e)
              => DelayedOpenAcc aenv (Array sh e)
              -> LLVM (IntMap (Idx' aenv), ExecOpenAcc aenv (Array sh e))
        travA a = do
          a'    <- traverseAcc a
          return $ (bind a', a')

        travT :: Tuple (DelayedOpenExp env aenv) t
              -> LLVM (IntMap (Idx' aenv), Tuple (PreOpenExp ExecOpenAcc env aenv) t)
        travT NilTup        = return (pure NilTup)
        travT (SnocTup t e) = liftA2 SnocTup <$> travT t <*> travE e

        travF :: DelayedOpenFun env aenv t
              -> LLVM (IntMap (Idx' aenv), PreOpenFun ExecOpenAcc env aenv t)
        travF (Body b)  = liftA Body <$> travE b
        travF (Lam  f)  = liftA Lam  <$> travF f

        bind :: (Shape sh, Elt e) => ExecOpenAcc aenv (Array sh e) -> IntMap (Idx' aenv)
        bind (ExecAcc _ _ (Avar ix)) = freevar ix
        bind _                       = INTERNAL_ERROR(error) "bind" "expected array variable"


-- Compilation
-- -----------

-- | Generate, compile, and link code that can be used to evaluate an array
-- computation.
--
-- TODO: * actually compile the generated code, with the given target backend
--       * asynchronous compilation
--       * kernel caching
--
build :: forall aenv a.
         DelayedOpenAcc aenv a
      -> Aval aenv
      -> LLVM G.Module
build acc aenv = do
  ctx <- asks llvmContext
  tgt <- asks llvmTarget

  -- Run code generation on the array program
  let ast = llvmOfAcc tgt acc aenv

  -- Lower the Haskell AST into C++ objects. Run verification and optimisation.
  mdl <- runError $ withModuleFromAST ctx (unModule ast) return
  when check $ runError (verify mdl)
  liftIO     $ withPassManager opt (\pm -> void $ runPassManager pm mdl)

  -- Compile the C++ module into something this target expects
--  compileForTarget mdl (kernelsOf ast)
  return mdl
  where
    opt         = defaultCuratedPassSetSpec { optLevel = Just 3 }
    runError e  = liftIO $ either (INTERNAL_ERROR(error) "build") id `fmap` runErrorT e

--    kernelsOf (CG.Module m)     = mapMaybe extract (AST.moduleDefinitions m)
--
--    extract (AST.GlobalDefinition AST.Function{..})
--      | not (null basicBlocks)  = Just name
--    extract _                   = Nothing

#if defined(ACCELERATE_DEBUG) || defined(ACCELERATE_INTERNAL_CHECKS)
    check = True
#else
    check = False
#endif


-- Applicative
-- -----------
--
liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

