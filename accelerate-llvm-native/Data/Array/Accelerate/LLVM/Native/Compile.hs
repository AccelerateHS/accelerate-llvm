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
  ObjectR(..),

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
import Data.Array.Accelerate.LLVM.Native.Compile.Cache
import Data.Array.Accelerate.LLVM.Native.Compile.Optimise
import Data.Array.Accelerate.LLVM.Native.Foreign                    ( )
import Data.Array.Accelerate.LLVM.Native.Target
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

-- standard library
import Control.Monad.Except                                         ( runExceptT )
import Control.Monad.State
import Data.ByteString                                              ( ByteString )
import Data.Maybe
import System.Directory
import qualified Data.ByteString                                    as B


instance Compile Native where
  data ObjectR Native = ObjectR {-# UNPACK #-} !ByteString
  compileForTarget    = compile

instance Intrinsic Native


-- | Compile an Accelerate expression to object code
--
compile :: DelayedOpenAcc aenv a -> Gamma aenv -> LLVM Native (ObjectR Native)
compile acc aenv = do
  target  <- gets llvmTarget
  cache   <- cacheOfOpenAcc acc

  -- Generate code for this Acc operation
  --
  let Module ast _  = llvmOfOpenAcc target acc aenv
      triple        = fromMaybe "" (moduleTargetTriple ast)
      datalayout    = moduleDataLayout ast
      --
      runExcept     = either ($internalError "compileForNativeTarget") return <=< runExceptT

  -- Lower the generated LLVM and produce an object file. If a cached version is
  -- available, that is returned instead.
  --
  obj <- liftIO $ do
    yes <- doesFileExist cache
    if yes
      then B.readFile cache
      else
        withContext                           $ \ctx     ->
        runExcept $ withModuleFromAST ctx ast $ \mdl     ->
        runExcept $ withNativeTargetMachine   $ \machine ->
          withTargetLibraryInfo triple        $ \libinfo -> do
            optimiseModule datalayout (Just machine) (Just libinfo) mdl

            Debug.when Debug.verbose $ do
              Debug.traceIO Debug.dump_cc  =<< moduleLLVMAssembly mdl
              Debug.traceIO Debug.dump_asm =<< runExcept (moduleTargetAssembly machine mdl)

            obj <- runExcept (moduleObject machine mdl)
            B.writeFile cache obj
            return obj

  return $! ObjectR obj

