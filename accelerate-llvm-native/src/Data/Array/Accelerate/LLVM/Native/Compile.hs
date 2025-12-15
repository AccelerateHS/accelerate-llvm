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
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo.Delayed

import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Target.ClangInfo                  ( hostLLVMVersion, llvmverFromTuple, clangExePath )
import Data.Array.Accelerate.LLVM.CodeGen.Environment               ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                    ( Module(..) )

import Data.Array.Accelerate.LLVM.Native.CodeGen                    ( )
import Data.Array.Accelerate.LLVM.Native.Compile.Cache
import Data.Array.Accelerate.LLVM.Native.Foreign                    ( )
import Data.Array.Accelerate.LLVM.Native.Target
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug

import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty     as P
import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty.PP  as P
import qualified Text.PrettyPrint                                   as P ( render )

import Control.Applicative
import Control.Monad.State
import Data.ByteString.Short                                        ( ShortByteString )
import Data.List                                                    ( intercalate )
import Data.Foldable                                                ( toList )
import Data.Maybe
import Formatting
import System.Directory
import System.Environment
import System.FilePath                                              ( (<.>) )
import qualified System.Info                                        as Info
import System.IO.Unsafe
import System.Process
import qualified Data.ByteString.Short.Char8                        as SBS8
import qualified Data.Map.Strict                                    as Map


instance Compile Native where
  data ObjectR Native = ObjectR
    { objId         :: {-# UNPACK #-} !UID
    , objSyms       :: ![ShortByteString]
    , staticObjPath :: {- LAZY -} FilePath
    , sharedObjPath :: {- LAZY -} FilePath
    }
  compileForTarget    = compile

instance Intrinsic Native


-- | Compile an Accelerate expression to object code.
--
-- This compilation step creates a static object file and a shared object
-- file, on demand. The former is used in the case of @runQ@ to statically
-- link the compiled object into the executable and generate FFI imports so
-- that the compiled kernel can be embedded directly into the resulting
-- executable. The latter will convert the former into a shared object to
-- be loaded into the running executable using the system's dynamic linker.
--
compile :: PreOpenAcc DelayedOpenAcc aenv a -> Gamma aenv -> LLVM Native (ObjectR Native)
compile pacc aenv = do

  -- Generate code for this Acc operation
  --
  -- We require the metadata result, which will give us the names of the
  -- functions which will be contained in the object code, but the actual
  -- code generation step is executed lazily.
  --
  (uid, cachePath) <- cacheOfPreOpenAcc pacc
  Module ast md    <- llvmOfPreOpenAcc uid pacc aenv

  let staticObjFile = cachePath <.> staticObjExt
      sharedObjFile = cachePath <.> sharedObjExt
      -- triple        = fromMaybe BS.empty (moduleTargetTriple ast)
      -- datalayout    = moduleDataLayout ast
      nms           = [ SBS8.pack f | P.Symbol f <- Map.keys md ]

  -- Lower the generated LLVM and produce an object file.
  --
  -- The 'staticObjPath' field is only lazily evaluated since the object
  -- code might already have been loaded into memory from a different
  -- function, in which case it will be found in the linker cache.
  --
  o_file <- liftIO . unsafeInterleaveIO $ do
    force_recomp  <- if Debug.debuggingIsEnabled then Debug.getFlag Debug.force_recomp else return False
    o_file_exists <- doesFileExist staticObjFile
    if o_file_exists && not force_recomp
      then
        Debug.traceM Debug.dump_cc ("cc: found cached object " % shown) uid

      else do
        -- print ast

        -- Detect LLVM version
        -- Note: this LLVM version is incorporated in the cache path, so we're safe detecting it at runtime.
        let prettyHostLLVMVersion = intercalate "." (Prelude.map show (toList hostLLVMVersion))
        llvmver <- case llvmverFromTuple hostLLVMVersion of
                     Just llvmver -> return llvmver
                     Nothing -> internalError ("accelerate-llvm-native: Unsupported LLVM version: " % string)
                                              prettyHostLLVMVersion
        Debug.traceM Debug.dump_cc ("Using Clang at " % string % " version " % shown) clangExePath prettyHostLLVMVersion

        -- Convert module to llvm-pretty format so that we can print it
        let unoptimisedText = P.render (P.ppLLVM llvmver (P.ppModule ast))
        Debug.when Debug.verbose $ do
          Debug.traceM Debug.dump_cc ("Unoptimised LLVM IR:\n" % string) unoptimisedText

        dVerbose <- Debug.getFlag Debug.verbose
        dDumpCC <- Debug.getFlag Debug.dump_cc
        dDumpAsm <- Debug.getFlag Debug.dump_asm

        let clangFlags inputType outputFlags output =
              -- '-O3' is ignored when only assembling; let's avoid clang warning about that
              (if inputType == "assembler" then [] else ["-O3"]) ++
              ["-march=native", "-c", "-o", output, "-x", inputType, "-"
              -- clang knows better what the target triple (and the data
              -- layout) should be than us, so let it override the triple, and
              -- don't warn about it
              -- TODO: change llvm-pretty so that it doesn't require us to give
              -- it a target triple
              ,"-Wno-override-module"] ++
              outputFlags

        let linkOutputFlags | Info.os == "mingw32" = []
                            | otherwise = ["-fPIC"]

        -- Minimise the number of clang invocations (to 1) in the common case
        -- of no verbose debug flags. If we need to print some intermediate
        -- stages, run all stages separately for simplicity, and print only the
        -- intermediate values that were requested.
        -- See llvm-project/clang/include/clang/Driver/Types.def for "-x" argument values:
        --   https://github.com/llvm/llvm-project/blob/da286c8bf69684d1612d1fc440bd9c6f1a4326df/clang/include/clang/Driver/Types.def
        if dVerbose && (dDumpCC || dDumpAsm)
          then do
            optText <- readProcess clangExePath (clangFlags "ir" ["-S", "-emit-llvm"] "-") unoptimisedText
            Debug.traceM Debug.dump_cc ("Optimised LLVM IR:\n" % string) optText
            asmText <- readProcess clangExePath (clangFlags "ir" ["-S"] "-") optText
            Debug.traceM Debug.dump_asm ("Optimised assembly:\n" % string) asmText
            _ <- readProcess clangExePath (clangFlags "assembler" linkOutputFlags staticObjFile) asmText
            return ()
          else do
            _ <- readProcess clangExePath (clangFlags "ir" linkOutputFlags staticObjFile) unoptimisedText
            return ()

        Debug.traceM Debug.dump_cc ("cc: new object code " % shown) uid

    return staticObjFile

  -- Convert the relocatable object file (created above) into a shared
  -- object file using the operating system's native linker.
  --
  -- Once again, the 'sharedObjPath' is only lazily evaluated since the
  -- object code might already have been loaded into memory from
  -- a different function.
  --
  so_file <- liftIO . unsafeInterleaveIO $ do
    force_recomp   <- if Debug.debuggingIsEnabled then Debug.getFlag Debug.force_recomp else return False
    so_file_exists <- doesFileExist sharedObjFile
    if so_file_exists && not force_recomp
      then
        Debug.traceM Debug.dump_cc ("cc: found cached shared object " % shown) uid

      else do
        o_file_exists <- doesFileExist staticObjFile
        objFile       <- if o_file_exists && not force_recomp
                           then do
                             Debug.traceM Debug.dump_cc ("cc: found cached object " % shown) uid
                             return staticObjFile
                           else
                             return o_file

        -- LLVM doesn't seem to provide a way to build a shared object file
        -- directly, so shell out to the system linker to do this.
        --
        case Info.os of
          "darwin" ->
            -- TODO: should we pass -lm on Darwin too? Seems likely. (The -lm on
            -- Linux was added to properly declare dependency on libm, so that it
            -- gets pulled in even if the main executable is statically-linked and
            -- thus does not have a dynamic libm in its address space.)
            callProcess ld ["--shared", "-o", sharedObjFile, objFile, "-undefined", "dynamic_lookup"]
          "mingw32" ->  -- windows
            callProcess ld ["--shared", "-o", sharedObjFile, objFile]  -- no -lm necessary on windows
          _ ->  -- linux etc.
            callProcess ld ["--shared", "-o", sharedObjFile, objFile, "-lm"]
        Debug.traceM Debug.dump_cc ("cc: new shared object " % shown) uid

    return sharedObjFile

  return $! ObjectR uid nms o_file so_file


-- Respect the common @LD@ and @CC@ environment variables, falling back to
-- search the path for @cc@ if neither of those exist.
--
-- XXX: On Unixy systems, we use @cc@ as the default instead of @ld@ because
-- on macOS this will do the right thing, whereas 'ld --shared' will not.
-- On Windows, we just use clang as the driver to "do the right thing".
--
ld :: FilePath
ld = unsafePerformIO $ do
  let defProgram | Info.os == "mingw32" = clangExePath
                 | otherwise = "cc"
  mfromEnv <- liftA2 (<|>) (lookupEnv "LD") (lookupEnv "CC")
  return (fromMaybe defProgram mfromEnv)

-- The file extension for static libraries
--
staticObjExt :: String
staticObjExt | Info.os == "mingw32" = "obj"
             | otherwise = "o"

-- The file extension used for shared libraries
--
sharedObjExt :: String
sharedObjExt = case Info.os of
  "darwin" -> "dylib"
  "mingw32" -> "dll"
  _ -> "so"  -- let's just default to the unixy ".so"
