{-# LANGUAGE CPP                 #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Target
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Target
  where

-- llvm-general
import qualified LLVM.General.AST                               as AST
import LLVM.General.Target                                      hiding ( Target )

-- accelerate
import Data.Array.Accelerate.Trafo                              ( DelayedOpenAcc )

import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                ( Module(..) )
import Data.Array.Accelerate.LLVM.Native.CodeGen                ( llvmOfAcc )
import Data.Array.Accelerate.LLVM.State                         ( LLVM )
import Data.Array.Accelerate.LLVM.Target

-- standard library
import Control.Monad.Error
import System.IO.Unsafe

-- extra modules need for dumping the llvm code
#ifdef ACCELERATE_DEBUG
import qualified LLVM.General.Module                            as LLVM
import qualified LLVM.General.PassManager                       as LLVM

import Data.Array.Accelerate.LLVM.State                         ( llvmContext )
import Data.Array.Accelerate.LLVM.Debug                         as Debug

import Control.Monad.Reader
#endif


-- | Native machine code JIT execution target
--
data Native

instance Target Native where
  data ExecutableR Native = NativeR { executableR :: AST.Module }
--  data ExecutableR Native = NativeR { executableR :: [FunPtr ()] }

  {-# NOINLINE targetTriple #-}
  targetTriple _ =
    Just $ unsafePerformIO getProcessTargetTriple

  {-# NOINLINE targetDataLayout #-}
  targetDataLayout _ = unsafePerformIO $
    either error Just `fmap` (runErrorT $ withDefaultTargetMachine getTargetMachineDataLayout)

  -- TODO:
  --   * Compile to native JIT code and retain the function pointer
  compileForTarget = compileForNativeTarget


-- Compile an Accelerate expression for the native CPU target
--
compileForNativeTarget :: forall aenv a. DelayedOpenAcc aenv a -> Gamma aenv -> LLVM (ExecutableR Native)
compileForNativeTarget acc aenv = do
  let ast = llvmOfAcc acc aenv          :: Module Native aenv a
#ifdef ACCELERATE_DEBUG
      pss = LLVM.defaultCuratedPassSetSpec { LLVM.optLevel = Just 3 }
  Debug.when dump_llvm $ do
    ctx <- asks llvmContext
    r   <- liftIO . runErrorT $
            LLVM.withModuleFromAST ctx (unModule ast) $ \mdl ->
            LLVM.withPassManager pss                  $ \pm  -> do
              void $ LLVM.runPassManager pm mdl
              Debug.message dump_llvm =<< LLVM.moduleLLVMAssembly mdl
    either error return r
#endif
  return $ NativeR (unModule ast)

