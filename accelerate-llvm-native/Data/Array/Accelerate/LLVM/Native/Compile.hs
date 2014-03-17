{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile (

  module Data.Array.Accelerate.LLVM.Compile,
  withOptimisedModuleFromAST,

) where

-- llvm-general
import LLVM.General.AST                                         hiding ( Module )
import LLVM.General.Module                                      as LLVM
import LLVM.General.Context
import LLVM.General.PassManager
import LLVM.General.Target
import LLVM.General.Transforms
import qualified LLVM.General.AST                               as AST

-- accelerate
import Data.Array.Accelerate.Trafo                              ( DelayedOpenAcc )

import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                ( Module(..) )

import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Native.CodeGen                ( )
import qualified Data.Array.Accelerate.LLVM.Native.Debug        as Debug

-- standard library
import Control.Monad.Error
import Control.Monad.Reader
import Data.Maybe

#include "accelerate.h"


instance Compile Native where
  compileForTarget = compileForNativeTarget


-- Compile an Accelerate expression for the native CPU target.
--
-- TODO: We desperately want to return a compiled MCJIT function from here,
--       instead of just embedding the generated llvm-general-pure AST.
--
compileForNativeTarget :: DelayedOpenAcc aenv a -> Gamma aenv -> LLVM Native (ExecutableR Native)
compileForNativeTarget acc aenv = do
  let Module ast = llvmOfAcc Native acc aenv

  -- Only in verbose mode do we dump the LLVM or target ASM to the screen
  Debug.when Debug.verbose $ do
    ctx <- asks llvmContext
    liftIO $ withOptimisedModuleFromAST ctx ast $ \mdl -> do
      let runError = either (INTERNAL_ERROR(error) "compileForNativeTarget") return <=< runErrorT

#if MIN_VERSION_llvm_general(3,3,0)
      Debug.message Debug.dump_llvm =<< LLVM.moduleLLVMAssembly mdl

      runError $ withNativeTargetMachine $ \tm ->
        Debug.message Debug.dump_asm =<< runError (moduleTargetAssembly tm mdl)
#else
      Debug.message Debug.dump_llvm =<< LLVM.moduleString mdl
      -- XXX: Only interface to access the assembly in llvm-general-3.2.* is
      --      via dumping to file. derp.
      --
      -- tmp     <- getTemporaryDirectiroy
      -- (asm,_) <- openTempFile tmp "foo.asm"
      -- runError $ writeAssemblyToFile tm asm
      --      ...
      -- removeFile name
      --
#endif
  return $ NativeR ast


-- Combined lowering and optimisation of a Haskell LLVM AST into C++ objects.
-- This runs the optimisation passes such that LLVM has the necessary
-- information to automatically vectorize loops whenever it deems beneficial.
---
withOptimisedModuleFromAST :: Context -> AST.Module -> (LLVM.Module -> IO a) -> IO a
withOptimisedModuleFromAST ctx ast next =
  runError $ withModuleFromAST ctx ast $ \mdl     ->
  runError $ withNativeTargetMachine   $ \machine ->
    withTargetLibraryInfo triple       $ \libinfo -> do

      let p1 = PassSetSpec prepass datalayout (Just libinfo) (Just machine)
          p2 = PassSetSpec optpass datalayout (Just libinfo) (Just machine)

      _ <- withPassManager p1 $ \pm -> runPassManager pm mdl
      _ <- withPassManager p2 $ \pm -> runPassManager pm mdl

      next mdl
  where
    runError    = either (INTERNAL_ERROR(error) "withOptimisedModuleFromAST") return <=< runErrorT

    triple      = fromMaybe "" (moduleTargetTriple ast)
    datalayout  = moduleDataLayout ast


-- The first gentle optimisation pass. I think this is usually done when loading
-- the module?
--
-- This is the first section of output running 'opt -O3 -debug-pass=Arguments'
--
-- Pass Arguments:
--  -datalayout -notti -basictti -x86tti -no-aa -tbaa -targetlibinfo -basicaa
--  -preverify -domtree -verify -simplifycfg -domtree -sroa -early-cse
--  -lower-expect
--
prepass :: [Pass]
prepass =
  [ SimplifyControlFlowGraph
  , ScalarReplacementOfAggregates { requiresDominatorTree = True }
  , EarlyCommonSubexpressionElimination
  , LowerExpectIntrinsic
  ]

-- The main optimisation pipeline. This mostly matches the process of running
-- 'opt -O3 -debug-pass=Arguments'. We are missing dead argument elimination and
-- in particular, slp-vectorizer (super-word level parallelism).
--
-- Pass Arguments:
--   -targetlibinfo -datalayout -notti -basictti -x86tti -no-aa -tbaa -basicaa
--   -globalopt -ipsccp -deadargelim -instcombine -simplifycfg -basiccg -prune-eh
--   -inline-cost -inline -functionattrs -argpromotion -sroa -domtree -early-cse
--   -lazy-value-info -jump-threading -correlated-propagation -simplifycfg
--   -instcombine -tailcallelim -simplifycfg -reassociate -domtree -loops
--   -loop-simplify -lcssa -loop-rotate -licm -lcssa -loop-unswitch -instcombine
--   -scalar-evolution -loop-simplify -lcssa -indvars -loop-idiom -loop-deletion
--   -loop-unroll -memdep -gvn -memdep -memcpyopt -sccp -instcombine
--   -lazy-value-info -jump-threading -correlated-propagation -domtree -memdep -dse
--   -loops -scalar-evolution -slp-vectorizer -adce -simplifycfg -instcombine
--   -barrier -domtree -loops -loop-simplify -lcssa -scalar-evolution
--   -loop-simplify -lcssa -loop-vectorize -instcombine -simplifycfg
--   -strip-dead-prototypes -globaldce -constmerge -preverify -domtree -verify
--
optpass :: [Pass]
optpass =
  [
    InterproceduralSparseConditionalConstantPropagation                 -- ipsccp
  , InstructionCombining
  , SimplifyControlFlowGraph
  , PruneExceptionHandling
  , FunctionInlining { functionInliningThreshold = 275 }                -- -O2 => 275
  , FunctionAttributes
  , ArgumentPromotion                                                   -- not needed?
  , ScalarReplacementOfAggregates { requiresDominatorTree = True }      -- false?
  , EarlyCommonSubexpressionElimination
  , JumpThreading
  , CorrelatedValuePropagation
  , SimplifyControlFlowGraph
  , InstructionCombining
  , TailCallElimination
  , SimplifyControlFlowGraph
  , Reassociate
  , LoopRotate
  , LoopInvariantCodeMotion
  , LoopClosedSingleStaticAssignment
  , LoopUnswitch { optimizeForSize = False }
  , LoopInstructionSimplify
  , InstructionCombining
  , InductionVariableSimplify
  , LoopIdiom
  , LoopDeletion
  , LoopUnroll { loopUnrollThreshold = Nothing
               , count               = Nothing
               , allowPartial        = Nothing }
  , GlobalValueNumbering { noLoads = False }                             -- loads??
  , SparseConditionalConstantPropagation
  , InstructionCombining
  , JumpThreading
  , CorrelatedValuePropagation
  , DeadStoreElimination
  , defaultVectorizeBasicBlocks                                         -- instead of slp-vectorizer?
  , AggressiveDeadCodeElimination
  , SimplifyControlFlowGraph
  , InstructionCombining
  , LoopVectorize
  , InstructionCombining
  , SimplifyControlFlowGraph
  , GlobalDeadCodeElimination
  , ConstantMerge
  ]

