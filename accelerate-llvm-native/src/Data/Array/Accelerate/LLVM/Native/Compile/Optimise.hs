{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile.Optimise
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile.Optimise (

  optimiseModule

) where

import LLVM.AST.DataLayout
import LLVM.Module
import LLVM.Target

#if MIN_VERSION_llvm_hs(15,0,0)
import LLVM.Passes
#else
import LLVM.PassManager

import Formatting
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug
#endif



-- | Run the standard optimisations on the given module when targeting a
-- specific machine and data layout. Specifically, this will run the
-- optimisation passes such that LLVM has the necessary information to
-- automatically vectorise loops (whenever it deems beneficial to do so).
--
optimiseModule
    :: Maybe DataLayout
    -> Maybe TargetMachine
    -> Maybe TargetLibraryInfo
    -> Module
    -> IO ()
#if MIN_VERSION_llvm_hs(15,0,0)
optimiseModule _ machine _ mdl = do
  let p1 = PassSetSpec
            { passes        = [ CuratedPassSet 3 ]
            , targetMachine = machine
            }
  runPasses p1 mdl
#else
optimiseModule datalayout machine libinfo mdl = do
  let p1 = defaultCuratedPassSetSpec
            { optLevel                           = Just 3
            , dataLayout                         = datalayout
            , targetMachine                      = machine
            , targetLibraryInfo                  = libinfo
            , loopVectorize                      = Just True
            , superwordLevelParallelismVectorize = Just True
            }
  b1 <- withPassManager p1 $ \pm -> runPassManager pm mdl

  Debug.traceM Debug.dump_cc ("llvm: optimisation did work? " % shown) b1
#endif


{--
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
  , GlobalValueNumbering { noLoads = False }    -- True to add memory dependency analysis
  , SparseConditionalConstantPropagation
  , InstructionCombining
  , JumpThreading
  , CorrelatedValuePropagation
  , DeadStoreElimination
  , defaultVectorizeBasicBlocks                 -- instead of slp-vectorizer?
  , AggressiveDeadCodeElimination
  , SimplifyControlFlowGraph
  , InstructionCombining
  , LoopVectorize
  , InstructionCombining
  , SimplifyControlFlowGraph
  , GlobalDeadCodeElimination
  , ConstantMerge
  ]
--}

