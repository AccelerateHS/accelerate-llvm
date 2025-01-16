{-# LANGUAGE CPP               #-}
#define MIN_VERSION_llvm_hs(a,b,c) 1
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile (

  module Data.Array.Accelerate.LLVM.Compile,
  ObjectR(..),

) where

import Data.Array.Accelerate.AST                                    ( PreOpenAcc )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo.Delayed

import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.CodeGen.Environment               ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                    ( Module(..) )
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Extra
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Target.ClangInfo                  ( hostLLVMVersion, llvmverFromTuple )

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.CodeGen
import Data.Array.Accelerate.LLVM.PTX.Compile.Cache
-- import Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice
import Data.Array.Accelerate.LLVM.PTX.Foreign                       ( )
import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

import Foreign.CUDA.Path
import qualified Foreign.CUDA.Analysis                              as CUDA
import qualified Foreign.NVVM                                       as NVVM

-- import qualified LLVM.AST                                           as AST
-- import qualified LLVM.AST.Name                                      as LLVM
-- import qualified LLVM.Context                                       as LLVM
-- import qualified LLVM.Module                                        as LLVM
-- import qualified LLVM.Target                                        as LLVM
-- import qualified LLVM.Internal.Module                               as LLVM.Internal
-- import qualified LLVM.Internal.FFI.LLVMCTypes                       as LLVM.Internal.FFI
-- import qualified LLVM.Analysis                                      as LLVM
-- #if MIN_VERSION_llvm_hs(15,0,0)
-- import qualified LLVM.Passes                                        as LLVM
-- #else
-- import qualified LLVM.PassManager                                   as LLVM
-- #endif

import qualified Text.LLVM                                          as LP
import qualified Text.LLVM.PP                                       as LP
import qualified Text.PrettyPrint                                   as LP ( render )

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short                                        ( ShortByteString )
import Data.List                                                    ( intercalate )
import Data.Foldable                                                ( toList )
import Data.Maybe
import Data.Text.Encoding
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Formatting
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process
import System.Process.Extra
import Text.Printf                                                  ( printf )
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Char8                              as BS8
import qualified Data.ByteString.Internal                           as B
import qualified Data.ByteString.Short.Char8                        as SBS8
import qualified Data.HashMap.Strict                                as HashMap
import qualified Data.Map.Strict                                    as Map
import Prelude                                                      as P


instance Compile PTX where
  data ObjectR PTX = ObjectR { objId     :: {-# UNPACK #-} !UID
                             , -- | Config for each exported kernel (symbol)
                               ptxConfig :: ![(ShortByteString, LaunchConfig)]
                             , objPath   :: {- LAZY -} FilePath
                             }
  compileForTarget = compile


-- | Compile an Accelerate expression to object code.
--
-- This generates the target code together with a list of each kernel function
-- defined in the module paired with its occupancy information.
--
compile :: HasCallStack => PreOpenAcc DelayedOpenAcc aenv a -> Gamma aenv -> LLVM PTX (ObjectR PTX)
compile pacc aenv = do

  -- Generate code for this Acc operation
  --
  dev                  <- gets ptxDeviceProperties
  let CUDA.Compute m n = CUDA.computeCapability dev
  let arch             = printf "sm_%d%d" m n
  (uid, cacheFile)     <- cacheOfPreOpenAcc pacc
  Module ast md        <- llvmOfPreOpenAcc uid pacc aenv
  let config           = [ (SBS8.pack f, x) | (LP.Symbol f, KM_PTX x) <- Map.toList md ]

  -- Lower the generated LLVM into a CUBIN object code.
  --
  -- The 'objData' field is lazily evaluated since the object code might have
  -- already been loaded into the current context from a different function, in
  -- which case it will be found by the linker cache.
  --
  cubin <- liftIO . unsafeInterleaveIO $ do
    exists <- doesFileExist cacheFile
    recomp <- if Debug.debuggingIsEnabled then Debug.getFlag Debug.force_recomp else return False
    if exists && not recomp
      then do
        Debug.traceM Debug.dump_cc ("cc: found cached object code " % shown) uid

      else do
        -- Detect LLVM version
        -- Note: this LLVM version is incorporated in the cache path, so we're safe detecting it at runtime.
        let prettyHostLLVMVersion = intercalate "." (P.map show (toList hostLLVMVersion))
        llvmver <- case llvmverFromTuple hostLLVMVersion of
                     Just llvmver -> return llvmver
                     Nothing -> internalError ("accelerate-llvm-ptx: Unsupported LLVM version: " % string)
                                              prettyHostLLVMVersion
        Debug.traceM Debug.dump_cc ("Using LLVM version " % shown) prettyHostLLVMVersion

        -- Convert module to llvm-pretty format so that we can print it
        let unoptimisedText = LP.render (LP.ppLLVM llvmver (LP.ppModule ast))
        -- putStrLn unoptimisedText
        Debug.when Debug.verbose $ do
          Debug.traceM Debug.dump_cc ("Unoptimised LLVM IR:\n" % string) unoptimisedText

        -- '--cuda-gpu-arch=sm_XX' ?
        _ <- readProcess "llc" ["-O3", "-mcpu=" ++ arch
                               ,"-o", cacheFile ++ ".ptx"
                               ,"-"]
                         unoptimisedText

        ptxData <- B.readFile (cacheFile ++ ".ptx")
        compileCUBIN arch cacheFile ptxData

        -- LLVM.withContext $ \ctx -> do
        --   ptx   <- compilePTX dev ctx ast
        --   cubin <- compileCUBIN arch cacheFile ptx
        --   return cubin

    return cacheFile

  return $! ObjectR uid config cubin


{-
-- | Compile the LLVM module to PTX assembly. This is done either by the
-- closed-source libNVVM library, or via the standard NVPTX backend (which is
-- the default).
--
compilePTX :: CUDA.DeviceProperties -> LLVM.Context -> AST.Module -> IO ByteString
compilePTX dev ctx ast = do
#ifdef ACCELERATE_USE_NVVM
  ptx <- withLibdeviceNVVM  dev ctx ast (_compileModuleNVVM dev (AST.moduleName ast))
#else
  ptx <- withLibdeviceNVPTX dev ctx ast (_compileModuleNVPTX dev)
#endif
  Debug.when Debug.dump_asm $ Debug.traceM Debug.verbose stext (decodeUtf8 ptx)
  return ptx
-}

-- | Compile the given PTX assembly to a CUBIN file (SASS object code). The
-- compiled code will be stored at the given FilePath.
--
compileCUBIN :: HasCallStack => String -> FilePath -> ByteString -> IO ()
compileCUBIN arch sass ptx = do
  _verbose  <- if Debug.debuggingIsEnabled then Debug.getFlag Debug.verbose else return False
  _debug    <- if Debug.debuggingIsEnabled then Debug.getFlag Debug.debug   else return False
  --
  let verboseFlag       = if _verbose then [ "-v" ]              else []
      debugFlag         = if _debug   then [ "-g", "-lineinfo" ] else []
      flags             = "-" : "-o" : sass : ("-arch=" ++ arch) : verboseFlag ++ debugFlag
      --
      cp = (proc (cudaBinPath </> "ptxas") flags)
            { std_in  = CreatePipe
            , std_out = NoStream
            , std_err = CreatePipe
            }

  -- Invoke the 'ptxas' executable to compile the generated PTX into SASS (GPU
  -- object code). The output is written directly to the final cache location.
  --
  withCreateProcess cp $ \(Just inh) Nothing (Just errh) ph -> do

    -- fork off a thread to start consuming stderr
    info <- hGetContents errh
    withForkWait (evaluate (rnf info)) $ \waitErr -> do

      -- write the PTX to the input handle
      -- closing the handle performs an implicit flush, thus may trigger SIGPIPE
      ignoreSIGPIPE $ B.hPut inh ptx
      ignoreSIGPIPE $ hClose inh

      -- wait on the output
      waitErr
      hClose errh

    -- wait on the process
    ex <- waitForProcess ph
    case ex of
      ExitFailure r -> internalError ("ptxas " % unworded string % " (exit " % int % ")\n" % reindented 2 string) flags r info
      ExitSuccess   -> return ()

    when _verbose $
      unless (null info) $
        Debug.traceM Debug.dump_cc ("ptx: compiled entry function(s)\n" % reindented 2 string) info


{-
-- Compile and optimise the module to PTX using the (closed source) NVVM
-- library. This _may_ produce faster object code than the LLVM NVPTX compiler.
--
_compileModuleNVVM :: HasCallStack => CUDA.DeviceProperties -> ShortByteString -> [(ShortByteString, ByteString)] -> LLVM.Module -> IO ByteString
_compileModuleNVVM dev name libdevice mdl = do
  _debug <- if Debug.debuggingIsEnabled then Debug.getFlag Debug.debug else return False
  --
  let arch    = CUDA.computeCapability dev
      verbose = if _debug then [ NVVM.GenerateDebugInfo ] else []
      flags   = NVVM.Target arch : verbose

      -- Note: [NVVM and target datalayout]
      --
      -- The NVVM library does not correctly parse the target datalayout field,
      -- instead doing a (very dodgy) string compare against exactly two
      -- expected values. This means that it is sensitive to, e.g. the ordering
      -- of the fields, and changes to the representation in each LLVM release.
      --
      -- We get around this by only specifying the data layout in a separate
      -- (otherwise empty) module that we additionally link against.
      --
      header  = case bitSize (undefined::Int) of
                  32 -> "target triple = \"nvptx-nvidia-cuda\"\ntarget datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64\""
                  64 -> "target triple = \"nvptx64-nvidia-cuda\"\ntarget datalayout = \"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64\""
                  _  -> internalError "I don't know what architecture I am"

  Debug.when Debug.dump_cc   $ do
    Debug.when Debug.verbose $ do
      ll <- LLVM.moduleLLVMAssembly mdl -- TLM: unfortunate to do the lowering twice in debug mode
      Debug.traceM Debug.verbose stext (decodeUtf8 ll)

  -- Lower the generated module to bitcode, then compile and link together with
  -- the shim header and libdevice library (if necessary)
  bc  <- LLVM.moduleBitcode mdl
  ptx <- NVVM.compileModules (("",header) : (name,bc) : libdevice) flags

  unless (B.null (NVVM.compileLog ptx)) $ do
    Debug.traceM Debug.dump_cc ("llvm: " % stext) (decodeUtf8 (NVVM.compileLog ptx))

  -- Return the generated binary code
  return (NVVM.compileResult ptx)


-- Compiling with the NVPTX backend uses LLVM-3.3 and above
--
_compileModuleNVPTX :: CUDA.DeviceProperties -> LLVM.Module -> IO ByteString
_compileModuleNVPTX dev mdl =
  withPTXTargetMachine dev $ \nvptx -> do

    when Debug.internalChecksAreEnabled $ LLVM.verify mdl

    -- Run the standard optimisation pass
    --
#if MIN_VERSION_llvm_hs(15,0,0)
    let pss = LLVM.PassSetSpec { passes = [ LLVM.CuratedPassSet 3 ], targetMachine = Just nvptx }
    do
      LLVM.runPasses pss mdl
#else
    let pss = LLVM.defaultCuratedPassSetSpec { LLVM.optLevel = Just 3 }
    LLVM.withPassManager pss $ \pm -> do

      b1 <- LLVM.runPassManager pm mdl
#endif

      -- debug printout
      Debug.when Debug.dump_cc $ do
#if !MIN_VERSION_llvm_hs(15,0,0)
        Debug.traceM Debug.dump_cc ("llvm: optimisation did work? " % shown) b1
#endif
        Debug.traceM Debug.verbose stext . decodeUtf8 =<< LLVM.moduleLLVMAssembly mdl

      -- Lower the LLVM module into target assembly (PTX)
      moduleTargetAssembly nvptx mdl


-- | Produce target specific assembly as a 'ByteString'.
--
moduleTargetAssembly :: LLVM.TargetMachine -> LLVM.Module -> IO ByteString
moduleTargetAssembly tm m = unsafe0 =<< LLVM.Internal.emitToByteString LLVM.Internal.FFI.codeGenFileTypeAssembly tm m
  where
    -- Ensure that the ByteString is NULL-terminated, so that it can be passed
    -- directly to C. This will unsafely mutate the underlying ForeignPtr if the
    -- string is not NULL-terminated but the last character is a whitespace
    -- character (there are usually a few blank lines at the end).
    --
    unsafe0 :: ByteString -> IO ByteString
    unsafe0 bs@(B.PS fp s l) =
      liftIO . withForeignPtr fp $ \p -> do
        let p' :: Ptr Word8
            p' = p `plusPtr` (s+l-1)
        --
        x <- peek p'
        case x of
          0                    -> return bs
          _ | B.isSpaceWord8 x -> poke p' 0 >> return bs
          _                    -> return (B.snoc bs 0)
-}
