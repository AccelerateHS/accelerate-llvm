{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile (

  module Data.Array.Accelerate.LLVM.Compile,
  module Data.Array.Accelerate.LLVM.Native.Compile.Module,
  ExecutableR(..),

) where

-- llvm-general
import LLVM.AST                                                     hiding ( Module )
import LLVM.Module                                                  as LLVM hiding ( Module )
import LLVM.Context
import LLVM.Target
import LLVM.ExecutionEngine

-- accelerate
import Data.Array.Accelerate.Error                                  ( internalError )
import Data.Array.Accelerate.Trafo                                  ( DelayedOpenAcc )

import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.CodeGen.Environment               ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                    ( unModule )

import Data.Array.Accelerate.LLVM.Native.Compile.Link
import Data.Array.Accelerate.LLVM.Native.Compile.Module
import Data.Array.Accelerate.LLVM.Native.Compile.Optimise

import Data.Array.Accelerate.LLVM.Native.CodeGen                    ( )
import Data.Array.Accelerate.LLVM.Native.Foreign                    ( )
import Data.Array.Accelerate.LLVM.Native.Target
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

-- standard library
import Control.Monad.Except                                         ( runExceptT )
import Control.Monad.State
import Data.Maybe


instance Compile Native where
  data ExecutableR Native = NativeR { executableR :: Module }
  compileForTarget        = compileForNativeTarget

instance Intrinsic Native


-- Compile an Accelerate expression for the native CPU target.
--
compileForNativeTarget :: DelayedOpenAcc aenv a -> Gamma aenv -> LLVM Native (ExecutableR Native)
compileForNativeTarget acc aenv = do
  target <- gets llvmTarget

  -- Generate code for this Acc operation
  --
  let ast        = unModule (llvmOfOpenAcc target acc aenv)
      triple     = fromMaybe "" (moduleTargetTriple ast)
      datalayout = moduleDataLayout ast

  -- Lower the generated LLVM to an executable function(s)
  --
  mdl <- liftIO .
    compileModule                         $ \k       ->
    withContext                           $ \ctx     ->
    runExcept $ withModuleFromAST ctx ast $ \mdl     ->
    runExcept $ withNativeTargetMachine   $ \machine ->
      withTargetLibraryInfo triple        $ \libinfo -> do
        optimiseModule datalayout (Just machine) (Just libinfo) mdl

        Debug.when Debug.verbose $ do
          Debug.traceIO Debug.dump_cc  =<< moduleLLVMAssembly mdl
          Debug.traceIO Debug.dump_asm =<< runExcept (moduleTargetAssembly machine mdl)

        withMCJIT ctx opt model ptrelim fast $ \mcjit -> do
          withModuleInEngine mcjit mdl       $ \exe   -> do
            k =<< getGlobalFunctions ast exe

  return $ NativeR mdl

  where
    runExcept   = either ($internalError "compileForNativeTarget") return <=< runExceptT

    opt         = Just 3        -- optimisation level
    model       = Nothing       -- code model?
    ptrelim     = Nothing       -- True to disable frame pointer elimination
    fast        = Just True     -- True to enable fast instruction selection

