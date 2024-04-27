{-# LANGUAGE CPP               #-}
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
import Data.Array.Accelerate.LLVM.CodeGen.Environment               ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                    ( Module(..) )

import Data.Array.Accelerate.LLVM.Native.CodeGen                    ( )
import Data.Array.Accelerate.LLVM.Native.Compile.Cache
import Data.Array.Accelerate.LLVM.Native.Compile.Optimise
import Data.Array.Accelerate.LLVM.Native.Foreign                    ( )
import Data.Array.Accelerate.LLVM.Native.Target
import qualified Data.Array.Accelerate.LLVM.Native.Debug            as Debug
import LLVM.AST.ToLLVMPretty

import LLVM.AST                                                     hiding ( Module )
-- import LLVM.Module                                                  as LLVM hiding ( Module )
-- import LLVM.Context
-- import LLVM.Target
import qualified Text.LLVM.PP                                       as P
import qualified Text.PrettyPrint                                   as P ( render )

import Control.Applicative
import Control.Monad.State
import Data.ByteString.Short                                        ( ShortByteString )
import Data.List                                                    ( intercalate )
import Data.Foldable                                                ( toList )
import Data.Maybe
import Data.Text.Encoding
import Formatting
import System.Directory
import System.Environment
import System.FilePath                                              ( (<.>) )
import System.IO                                                    ( hPutStrLn, stderr )
import System.IO.Unsafe
import System.Process
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Short                              as BS
import qualified Data.HashMap.Strict                                as HashMap


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
      triple        = fromMaybe BS.empty (moduleTargetTriple ast)
      datalayout    = moduleDataLayout ast
      nms           = [ f | Name f <- HashMap.keys md ]

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
        print ast

        -- Detect LLVM version
        let prettyHostLLVMVersion = intercalate "." (Prelude.map show (toList hostLLVMVersion))
        llvmver <- case llvmverFromTuple hostLLVMVersion of
                     Just llvmver -> return llvmver
                     Nothing -> internalError ("accelerate-llvm-native: Unsupported LLVM version: " % string)
                                              prettyHostLLVMVersion
        Debug.traceM Debug.dump_cc ("Using LLVM version " % shown) prettyHostLLVMVersion

        -- Convert module to llvm-pretty format so that we can print it
        ast' <- case toLLVMPretty ast of
                  Right m -> return m
                  Left err -> internalError ("accelerate-llvm-native: Unsupported LLVM IR generated: " % string) err
        -- print ast'
        let unoptimisedText = P.render (P.ppLLVM llvmver (P.ppModule ast'))
        -- putStrLn unoptimisedText
        Debug.when Debug.verbose $ do
          Debug.traceM Debug.dump_cc ("Unoptimised LLVM IR:\n" % string) unoptimisedText

        -- Run the module through `opt` to optimise it
        optimisedText <- readProcess "opt" ["-S", "-O3"] unoptimisedText
        Debug.when Debug.verbose $ do
          Debug.traceM Debug.dump_cc ("Optimised LLVM IR:\n" % string) optimisedText

        -- Generate optimise machine code and write to the object file
        doDumpAsm <- Debug.getFlag Debug.dump_asm
        when doDumpAsm $
          hPutStrLn stderr "Warning: -ddump-asm non-functional when compiling via textual LLVM IR"
        _ <- readProcess "llc"
               ["-o", staticObjFile, "-O3", "-mcpu=native", "-filetype=obj", "-relocation-model=pic"]
               optimisedText
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
#if defined(darwin_HOST_OS)
        callProcess ld ["--shared", "-o", sharedObjFile, objFile, "-undefined", "dynamic_lookup"]
#else
        callProcess ld ["--shared", "-o", sharedObjFile, objFile]
#endif
        Debug.traceM Debug.dump_cc ("cc: new shared object " % shown) uid

    return sharedObjFile

  return $! ObjectR uid nms o_file so_file


-- Respect the common @LD@ and @CC@ environment variables, falling back to
-- search the path for @cc@ if neither of those exist.
--
-- XXX: Using @cc@ as the default here instead of @ld@ because on macOS
-- this will do the right thing, whereas 'ld --shared' will not.
--
ld :: FilePath
ld = unsafePerformIO
   $ fromMaybe "cc" <$> liftA2 (<|>) (lookupEnv "LD") (lookupEnv "CC")

-- The file extension for static libraries
--
staticObjExt :: String
#if   defined(mingw32_HOST_OS)
staticObjExt = "obj"
#else
staticObjExt = "o"
#endif

-- The file extension used for shared libraries
--
sharedObjExt :: String
#if   defined(darwin_HOST_OS)
sharedObjExt = "dylib"
#elif defined(linux_HOST_OS)
sharedObjExt = "so"
#elif defined(mingw32_HOST_OS)
sharedObjExt = "dll"
#else
#error "I don't know what platform I am"
#endif

