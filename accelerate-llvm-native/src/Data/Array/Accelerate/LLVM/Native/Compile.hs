{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Compile
-- Copyright   : [2014..2020] The Accelerate Team
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

import Data.Array.Accelerate.AST                                    ( PreOpenAcc )
import Data.Array.Accelerate.Trafo.Delayed

import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.CodeGen.Environment               ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                    ( Module(..) )

import Data.Array.Accelerate.LLVM.Native.CodeGen                    ( )
import Data.Array.Accelerate.LLVM.Native.Compile.Cache
import Data.Array.Accelerate.LLVM.Native.Compile.Optimise
import Data.Array.Accelerate.LLVM.Native.Foreign                    ( )
import Data.Array.Accelerate.LLVM.Native.Link.Util
import Data.Array.Accelerate.LLVM.Native.Target
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

import LLVM.AST                                                     hiding ( Module )
import LLVM.Module                                                  as LLVM hiding ( Module )
import LLVM.Context
import LLVM.Target

import Control.Monad.State
import Data.ByteString.Short                                        ( ShortByteString )
import Data.Maybe
import Data.Text.Encoding
import Formatting
import System.Directory
import System.FilePath                                              ( replaceExtension )
import System.IO.Unsafe
import Data.ByteString                                              ( ByteString )
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Short                              as BS
import qualified Data.HashMap.Strict                                as HashMap


instance Compile Native where
  data ObjectR Native = ObjectR { -- | The entry points defined in the kernel.
                                  objSyms    :: ![ShortByteString]
                                  -- | Points to a shared library file, Used for
                                  -- runtime dynamic linking with
                                  -- @run@/@run1@/@runN@. Since we'll only ever
                                  -- need one or the other, both the shared
                                  -- library and the raw object are returned as
                                  -- monadic actions so we can avoid generating
                                  -- what we don't need.
                                , objDynamic :: {- LAZY -} LLVM Native FilePath
                                  -- | Points to a raw object file, used for
                                  -- static linking with @runQ@.
                                , objStatic  :: {- LAZY -} LLVM Native FilePath
                                }
  compileForTarget    = compile

instance Intrinsic Native


-- | Compile an Accelerate expression to object code, link it to a shared
-- object, link that shared object to this process, and return a handle to the
-- linked library along with its symbols. We also do the same thing for a static
-- archive that can be used in runQ.
--
compile :: PreOpenAcc DelayedOpenAcc aenv a -> Gamma aenv -> LLVM Native (ObjectR Native)
compile pacc aenv = do

  -- Generate code for this Acc operation
  --
  -- We require the metadata result, which will give us the names of the
  -- functions which will be contained in the object code, but the actual
  -- code generation step is executed lazily.
  --
  (uid, dynamicCachePath) <- cacheOfPreOpenAcc pacc
  Module ast md           <- llvmOfPreOpenAcc uid pacc aenv

  let staticCachePath     = replaceExtension dynamicCachePath rawObjectExt
      triple              = fromMaybe BS.empty (moduleTargetTriple ast)
      datalayout          = moduleDataLayout ast
      nms                 = [ f | Name f <- HashMap.keys md ]

  -- After this compilation step we will either dynamically link to a shared
  -- library, or in the case of @runQ@ we will statically link to the compiled
  -- object and generate FFI imports so the compiled kernel can be embedded
  -- directly inside of the resulting binary. Since we'll only ever need one or
  -- the other, both of these options are presented as monadic actions so we
  -- lazily compute the required representation in either the linking or the
  -- embedding phase. The continuation passed to this function receives the raw
  -- object and is responsible for writing the correct representation to the
  -- cache file location.
  let compileObj :: String -> FilePath -> (FilePath -> ByteString -> IO ()) -> LLVM Native FilePath
      compileObj objectType cachePath k = liftIO . unsafeInterleaveIO $ do
        exists <- doesFileExist cachePath
        recomp <- if Debug.debuggingIsEnabled then Debug.getFlag Debug.force_recomp else return False
        if exists && not recomp
          then
            Debug.traceM Debug.dump_cc ("cc: found cached " % string % " " % shown) objectType uid

          else
            withContext                  $ \ctx     ->
            withModuleFromAST ctx ast    $ \mdl     ->
            withNativeTargetMachine      $ \machine ->
            withTargetLibraryInfo triple $ \libinfo -> do
              optimiseModule datalayout (Just machine) (Just libinfo) mdl

              Debug.when Debug.verbose $ do
                Debug.traceM Debug.dump_cc  stext . decodeUtf8 =<< moduleLLVMAssembly mdl
                Debug.traceM Debug.dump_asm stext . decodeUtf8 =<< moduleTargetAssembly machine mdl

              -- XXX: We'll let LLVM generate a relocatable object, and we'll then
              --      manually invoke the system linker to build a shared object out
              --      of it so we can link to it. LLVM doesn't seem to provide a way
              --      to do this for us without having to shell out to the linker.
              obj <- moduleObject machine mdl
              Debug.traceM Debug.dump_cc ("cc: new object code " % shown) uid

              k cachePath obj
              Debug.traceM Debug.dump_cc ("cc: cached " % string % " " % shown) objectType uid

        return cachePath

  return $!
    ObjectR
      { objSyms    = nms
      , objDynamic = compileObj "shared object"      dynamicCachePath linkSharedObject
      , objStatic  = compileObj "relocatable object" staticCachePath  B.writeFile
      }
