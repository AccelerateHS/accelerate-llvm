{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile (

  module Data.Array.Accelerate.LLVM.Compile,
  ObjectR(..),

) where

-- llvm-hs
import qualified LLVM.AST                                           as AST
import qualified LLVM.AST.Name                                      as LLVM
import qualified LLVM.Context                                       as LLVM
import qualified LLVM.Module                                        as LLVM
import qualified LLVM.PassManager                                   as LLVM
import qualified LLVM.Target                                        as LLVM
import qualified LLVM.Internal.Module                               as LLVM.Internal
import qualified LLVM.Internal.FFI.LLVMCTypes                       as LLVM.Internal.FFI
#ifdef ACCELERATE_INTERNAL_CHECKS
import qualified LLVM.Analysis                                      as LLVM
#endif

-- accelerate
import Data.Array.Accelerate.Error                                  ( internalError )
import Data.Array.Accelerate.Trafo                                  ( DelayedOpenAcc )

import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.CodeGen.Environment               ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                    ( Module(..) )
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Util

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.CodeGen
import Data.Array.Accelerate.LLVM.PTX.Compile.Cache
import Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice
import Data.Array.Accelerate.LLVM.PTX.Foreign                       ( )
import Data.Array.Accelerate.LLVM.PTX.Target

import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

-- cuda
import Foreign.CUDA.Path
import qualified Foreign.CUDA.Analysis                              as CUDA
import qualified Foreign.NVVM                                       as NVVM

-- standard library
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short                                        ( ShortByteString )
import Data.Maybe
import Data.Word
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.IO.Exception                                             ( IOErrorType(..), IOException(..) )
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process
import Text.Printf                                                  ( printf )
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Char8                              as B8
import qualified Data.ByteString.Internal                           as B
import qualified Data.Map                                           as Map
import Prelude                                                      as P


instance Compile PTX where
  data ObjectR PTX = ObjectR { ptxConfig :: ![(ShortByteString, LaunchConfig)]
                             , objId     :: {-# UNPACK #-} !Int
                             , objData   :: {- LAZY -} ByteString
                             }
  compileForTarget = compile


-- | Compile an Accelerate expression to object code.
--
-- This generates the target code together with a list of each kernel function
-- defined in the module paired with its occupancy information.
--
compile :: DelayedOpenAcc aenv a -> Gamma aenv -> LLVM PTX (ObjectR PTX)
compile acc aenv = do
  target            <- gets llvmTarget
  (uid, cacheFile)  <- cacheOfOpenAcc acc

  -- Generate code for this Acc operation
  --
  let Module ast md = llvmOfOpenAcc target acc aenv
      dev           = ptxDeviceProperties target
      config        = [ (f,x) | (LLVM.Name f, KM_PTX x) <- Map.toList md ]

  -- Lower the generated LLVM into a CUBIN object code.
  --
  -- The 'objData' field is lazily evaluated since the object code might have
  -- already been loaded into the current context from a different function, in
  -- which case it will be found by the linker cache.
  --
  cubin <- liftIO . unsafeInterleaveIO $ do
    exists <- doesFileExist cacheFile
    recomp <- Debug.queryFlag Debug.force_recomp
    if exists && not (fromMaybe False recomp)
      then B.readFile cacheFile
      else
        LLVM.withContext $ \ctx -> do
          ptx   <- compilePTX dev ctx ast
          cubin <- compileCUBIN dev cacheFile ptx
          return cubin

  return $! ObjectR config uid cubin


-- | Compile the LLVM module to PTX assembly. This is done either by the
-- closed-source libNVVM library, or via the standard NVPTX backend (which is
-- the default).
--
compilePTX :: CUDA.DeviceProperties -> LLVM.Context -> AST.Module -> IO ByteString
compilePTX dev ctx ast = do
#ifdef ACCELERATE_USE_NVVM
  ptx <- withLibdeviceNVVM  dev ctx ast (compileModuleNVVM dev (AST.moduleName ast))
#else
  ptx <- withLibdeviceNVPTX dev ctx ast (compileModuleNVPTX dev)
#endif
  Debug.when Debug.dump_asm $ Debug.traceIO Debug.verbose (B8.unpack ptx)
  return ptx


-- | Compile the given PTX assembly to a CUBIN file (SASS object code). The
-- compiled code will be stored at the given FilePath.
--
compileCUBIN :: CUDA.DeviceProperties -> FilePath -> ByteString -> IO ByteString
compileCUBIN dev sass ptx = do
  _verbose  <- Debug.queryFlag Debug.verbose
  _debug    <- Debug.queryFlag Debug.debug_cc
  --
  let verboseFlag       = if _verbose then [ "-v" ]              else []
      debugFlag         = if _debug   then [ "-g", "-lineinfo" ] else []
      arch              = printf "-arch=sm_%d%d" m n
      CUDA.Compute m n  = CUDA.computeCapability dev
      flags             = "-" : "-o" : sass : arch : verboseFlag ++ debugFlag
      --
      cp = (proc (cudaBinPath </> "ptxas") flags)
            { std_in  = CreatePipe
            , std_out = NoStream
            , std_err = CreatePipe
            }

  -- Invoke the 'ptxas' executable (which must be on the PATH) to compile the
  -- PTX into SASS. The output is written directly to the final cache location.
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
      ExitFailure r -> $internalError "compile" (printf "ptxas %s (exit %d)\n%s" (unwords flags) r info)
      ExitSuccess   -> return ()

    when _verbose $
      unless (null info) $
        Debug.traceIO Debug.dump_cc (printf "ptx: compiled entry function(s)\n%s" info)

  -- Read back the results
  B.readFile sass


-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important because we want to kill the thread that is holding the
-- Handle lock, because when we clean up the process we try to close that
-- handle, which could otherwise deadlock.
--
-- Stolen from the 'process' package.
--
withForkWait :: IO () -> (IO () -> IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid

ignoreSIGPIPE :: IO () -> IO ()
ignoreSIGPIPE =
  handle $ \e ->
    case e of
      IOError{..} | ResourceVanished <- ioe_type
                  , Just ioe         <- ioe_errno
                  , Errno ioe == ePIPE
                  -> return ()
      _ -> throwIO e


-- Compile and optimise the module to PTX using the (closed source) NVVM
-- library. This _may_ produce faster object code than the LLVM NVPTX compiler.
--
compileModuleNVVM :: CUDA.DeviceProperties -> String -> [(String, ByteString)] -> LLVM.Module -> IO ByteString
compileModuleNVVM dev name libdevice mdl = do
  _debug <- Debug.queryFlag Debug.debug_cc
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
                  _  -> $internalError "compileModuleNVVM" "I don't know what architecture I am"

  Debug.when Debug.dump_cc   $ do
    Debug.when Debug.verbose $ do
      ll <- LLVM.moduleLLVMAssembly mdl -- TLM: unfortunate to do the lowering twice in debug mode
      Debug.traceIO Debug.verbose (B8.unpack ll)

  -- Lower the generated module to bitcode, then compile and link together with
  -- the shim header and libdevice library (if necessary)
  bc  <- LLVM.moduleBitcode mdl
  ptx <- NVVM.compileModules (("",header) : (name,bc) : libdevice) flags

  unless (B.null (NVVM.compileLog ptx)) $ do
    Debug.traceIO Debug.dump_cc $ "llvm: " ++ B8.unpack (NVVM.compileLog ptx)

  -- Return the generated binary code
  return (NVVM.compileResult ptx)


-- Compiling with the NVPTX backend uses LLVM-3.3 and above
--
compileModuleNVPTX :: CUDA.DeviceProperties -> LLVM.Module -> IO ByteString
compileModuleNVPTX dev mdl =
  withPTXTargetMachine dev $ \nvptx -> do

    -- Run the standard optimisation pass
    --
    let pss = LLVM.defaultCuratedPassSetSpec { LLVM.optLevel = Just 3 }
    LLVM.withPassManager pss $ \pm -> do
#ifdef ACCELERATE_INTERNAL_CHECKS
      LLVM.verify mdl
#endif
      b1      <- LLVM.runPassManager pm mdl

      -- debug printout
      Debug.when Debug.dump_cc $ do
        Debug.traceIO Debug.dump_cc $ printf "llvm: optimisation did work? %s" (show b1)
        Debug.traceIO Debug.verbose . B8.unpack =<< LLVM.moduleLLVMAssembly mdl

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

