{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Compile (

  module Data.Array.Accelerate.LLVM.Compile,
  ObjectR(..),

) where

-- llvm-hs
import LLVM.AST                                                     hiding ( Module )
import LLVM.Module                                                  as LLVM hiding ( Module )
import LLVM.Context
import LLVM.Target

-- accelerate
import Data.Array.Accelerate.AST                                    ( PreOpenAcc )
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
import Control.Monad.State
import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short                                        ( ShortByteString )
import Data.Maybe
import System.Directory
import System.IO.Unsafe
import Text.Printf
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Char8                              as B8
import qualified Data.ByteString.Short                              as BS
import qualified Data.Map                                           as Map


instance Compile Native where
  data ObjectR Native = ObjectR { objId   :: {-# UNPACK #-} !UID
                                , objSyms :: {- LAZY -} [ShortByteString]
                                , objData :: {- LAZY -} ByteString
                                }
  compileForTarget    = compile

instance Intrinsic Native


-- | Compile an Accelerate expression to object code
--
compile :: PreOpenAcc DelayedOpenAcc aenv a -> Gamma aenv -> LLVM Native (ObjectR Native)
compile pacc aenv = do

  -- Generate code for this Acc operation
  --
  -- We require the metadata result, which will give us the names of the
  -- functions which will be contained in the object code, but the actual
  -- code generation step is executed lazily.
  --
  (uid, cacheFile)  <- cacheOfPreOpenAcc pacc
  Module ast md     <- llvmOfPreOpenAcc uid pacc aenv

  let triple        = fromMaybe BS.empty (moduleTargetTriple ast)
      datalayout    = moduleDataLayout ast
      nms           = [ f | Name f <- Map.keys md ]

  -- Lower the generated LLVM and produce an object file.
  --
  -- The 'objData' field is only lazy evaluated since the object code might
  -- already have been loaded into memory from a different function, in which
  -- case it will be found in the linker cache.
  --
  obj <- liftIO . unsafeInterleaveIO $ do
    exists <- doesFileExist cacheFile
    recomp <- if Debug.debuggingIsEnabled then Debug.getFlag Debug.force_recomp else return False
    if exists && not recomp
      then do
        Debug.traceIO Debug.dump_cc (printf "cc: found cached object code %s" (show uid))
        B.readFile cacheFile

      else
        withContext                  $ \ctx     ->
        withModuleFromAST ctx ast    $ \mdl     ->
        withNativeTargetMachine      $ \machine ->
        withTargetLibraryInfo triple $ \libinfo -> do
          optimiseModule datalayout (Just machine) (Just libinfo) mdl

          Debug.when Debug.verbose $ do
            Debug.traceIO Debug.dump_cc  . B8.unpack =<< moduleLLVMAssembly mdl
            Debug.traceIO Debug.dump_asm . B8.unpack =<< moduleTargetAssembly machine mdl

          obj <- moduleObject machine mdl
          Debug.traceIO Debug.dump_cc (printf "cc: new object code %s" (show uid))
          B.writeFile cacheFile obj
          return obj

  return $! ObjectR uid nms obj

