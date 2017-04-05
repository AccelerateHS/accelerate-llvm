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
  ExecutableR(..),

) where

-- llvm-general
import LLVM.AST                                                     hiding ( Module )
import LLVM.Module                                                  as LLVM hiding ( Module )
import LLVM.Context
import LLVM.Target

-- accelerate
import Data.Array.Accelerate.Error                                  ( internalError )
import Data.Array.Accelerate.Trafo                                  ( DelayedOpenAcc )

import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.CodeGen.Environment               ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                    ( Module(..) )

import Data.Array.Accelerate.LLVM.Native.CodeGen                    ( )
import Data.Array.Accelerate.LLVM.Native.Compile.Optimise
import Data.Array.Accelerate.LLVM.Native.Foreign                    ( )
import Data.Array.Accelerate.LLVM.Native.Link
import Data.Array.Accelerate.LLVM.Native.Target
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

-- standard library
import Control.Monad.Except                                         ( runExceptT )
import Control.Monad.State
import Data.ByteString                                              ( ByteString )
import Data.Maybe


instance Compile Native where
  type ObjectR     Native = ByteString
  data ExecutableR Native = NativeR { nativeExecutable :: {-# UNPACK #-} !FunctionTable
                                    , nativeObjectCode :: {-# UNPACK #-} !ObjectCode
                                    }
  compileForTarget  = compile
  linkForTarget     = link

instance Intrinsic Native


-- | Compile an Accelerate expression to object code
--
compile :: DelayedOpenAcc aenv a -> Gamma aenv -> LLVM Native (ObjectR Native)
compile acc aenv = do
  target  <- gets llvmTarget

  -- Generate code for this Acc operation
  --
  let Module ast _  = llvmOfOpenAcc target acc aenv
      triple        = fromMaybe "" (moduleTargetTriple ast)
      datalayout    = moduleDataLayout ast
      --
      runExcept     = either ($internalError "compileForNativeTarget") return <=< runExceptT

  -- Lower the generated LLVM and produce an object file.
  --
  liftIO .
    withContext                           $ \ctx     ->
    runExcept $ withModuleFromAST ctx ast $ \mdl     ->
    runExcept $ withNativeTargetMachine   $ \machine ->
      withTargetLibraryInfo triple        $ \libinfo -> do
        optimiseModule datalayout (Just machine) (Just libinfo) mdl

        Debug.when Debug.verbose $ do
          Debug.traceIO Debug.dump_cc  =<< moduleLLVMAssembly mdl
          Debug.traceIO Debug.dump_asm =<< runExcept (moduleTargetAssembly machine mdl)

        runExcept (moduleObject machine mdl)


-- | Load the generated object file into the target address space
--
link :: ObjectR Native -> LLVM Native (ExecutableR Native)
link obj = liftIO $ do
  (nm, vm)  <- loadObject obj
  return    $! NativeR nm vm

