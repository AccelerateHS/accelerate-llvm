{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
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

import Data.Array.Accelerate.LLVM.CodeGen                           ( llvmOfPreOpenAcc )
import Data.Array.Accelerate.LLVM.CodeGen.Environment               ( Gamma )
import Data.Array.Accelerate.LLVM.CodeGen.Module                    ( Module(..) )
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Target.ClangInfo                  ( hostLLVMVersion, llvmverFromTuple, clangExePath, clangExePathEnvironment )

import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch
import Data.Array.Accelerate.LLVM.PTX.CodeGen
import Data.Array.Accelerate.LLVM.PTX.Compile.Cache
import Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.Load
import Data.Array.Accelerate.LLVM.PTX.Foreign                       ( )
import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

import Foreign.CUDA.Path                                            ( cudaInstallPath )
import qualified Foreign.CUDA.Analysis                              as CUDA

import qualified LLVM.AST.Type.Name                                 as LLVM

import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty     as LP
import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty.PP  as LP
import qualified Text.PrettyPrint                                   as Pretty

import Control.Monad                                                ( when )
import Control.Monad.Reader
import Data.ByteString.Short                                        ( ShortByteString )
import Data.List                                                    ( intercalate )
import qualified Data.List.NonEmpty                                 as NE
import Data.Foldable                                                ( toList )
import GHC.IO.Exception                                             ( IOErrorType(OtherError) )
import Formatting
import System.Directory
import System.Exit                                                  ( ExitCode(..) )
import System.IO                                                    ( hPutStrLn, stderr )
import System.IO.Error                                              ( mkIOError )
import System.IO.Unsafe
import System.Process
import Text.Printf                                                  ( printf )
import qualified Data.ByteString.Short.Char8                        as SBS8
import qualified Data.Map.Strict                                    as Map


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
  dev                  <- asks ptxDeviceProperties
  let CUDA.Compute m n = CUDA.computeCapability dev
  let arch             = printf "sm_%d%d" m n
  (uid, cacheFile)     <- cacheOfPreOpenAcc pacc
  Module ast md        <- llvmOfPreOpenAcc uid pacc aenv
  let config           = [ (SBS8.pack f, x) | (LP.Symbol f, KM_PTX x) <- Map.toList md ]

  libdevice_bc <- liftIO libdeviceBitcodePath

  case isDeviceSupported (CUDA.computeCapability dev) of
    Nothing -> return ()  -- all fine
    Just err -> internalError string err

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
        let prettyHostLLVMVersion = intercalate "." (map show (toList hostLLVMVersion))
        llvmver <- case llvmverFromTuple hostLLVMVersion of
                     Just llvmver -> return llvmver
                     Nothing -> internalError ("accelerate-llvm-ptx: Unsupported LLVM version: " % string)
                                              prettyHostLLVMVersion
        Debug.traceM Debug.dump_cc ("Using Clang at " % string % " version " % shown) clangExePath prettyHostLLVMVersion

        when (NE.head hostLLVMVersion < 16) $
          case clangExePathEnvironment of
            Nothing -> do
              hPutStrLn stderr $
                "[accelerate-llvm-ptx] Clang version 16 or newer is required for the Nvidia PTX " ++
                "backend, but version " ++ prettyHostLLVMVersion ++ " was found at '" ++
                clangExePath ++ "'. To override this choice, set the ACCELERATE_LLVM_CLANG_PATH " ++
                "environment variable to point to the desired clang executable."
              -- not an IOError because we're in unsafePerformIO, somewhere up the call chain
              errorWithoutStackTrace $
                "accelerate-llvm-ptx: Clang version " ++ prettyHostLLVMVersion ++
                " found but >=16 required (set ACCELERATE_LLVM_CLANG_PATH to override)"
            Just{} ->  -- If an explicit path was given, let's just continue and see what happens.
              return ()

        -- Convert module to llvm-pretty format so that we can print it
        let unoptimisedText = Pretty.renderStyle
                                Pretty.style { Pretty.lineLength = maxBound `div` 2 }
                                (LP.ppLLVM llvmver (LP.ppModule ast))
                              ++ "\n\n" ++ accPreludePTX
        Debug.when Debug.verbose $ do
          Debug.traceM Debug.dump_cc ("Unoptimised LLVM IR:\n" % string) unoptimisedText

        isVerboseFlagSet <- Debug.getFlag Debug.verbose
        let clangArgs = ["-O3", "--target=nvptx64-nvidia-cuda", "-march=" ++ arch
                        ,"-o", cacheFile
                        ,"-Wno-override-module"
                        ,"--cuda-path=" ++ cudaInstallPath
                        ,"-x", "ir", "-"
                        -- See Note [Internalizing Libdevice]
                        -- TODO: only link in libdevice if we're actually using __nv_ functions!
                        ,"-Xclang", "-mlink-builtin-bitcode", "-Xclang", libdevice_bc]
                        ++ (if isVerboseFlagSet then ["-v"] else [])

        Debug.traceM Debug.dump_cc ("Arguments to clang: " % shown) clangArgs

        -- Remove some diagnostics from clang (and subprocesses) output that we
        -- know are fine. See filterClangStderr. Unfortunately, System.Process
        -- does not have a combinator for "give me stdout and stderr but throw
        -- exception on ExitFailure", so we do it manually.
        (clangEC, clangOut, clangErr) <- readProcessWithExitCode clangExePath clangArgs unoptimisedText
        putStr clangOut
        putStr (filterClangStderr clangErr)
        case clangEC of
          ExitSuccess -> return ()
          ExitFailure code -> do
            let msg = "clang returned non-zero exit code: " ++ show code ++
                      " (invocation: " ++ show (clangExePath : clangArgs) ++ ")"
            ioError $ mkIOError OtherError msg Nothing Nothing

        Debug.traceM Debug.dump_cc ("Written PTX to: " % string) cacheFile

    return cacheFile

  return $! ObjectR uid config cubin


{- Note [Internalizing Libdevice]

"Libdevice" refers to $CUDAPATH/nvvm/libdevice/libdevice.XX.bc, an LLVM bitcode
file that (reportedly) contains definitions of various math functions for use
in NVIDIA PTX. Most interesting primitive arithmetic operations on
floating-point numbers get compiled to calls to functions from libdevice, so it
is essential that we link it into any kernel that we create (or at least, any
kernel that references functions from libdevice).

However, libdevice is quite large; it is 473 KB of LLVM bitcode for cuda 12.6
on my machine, and clang takes >1 second to compile it on my (5 GHz Intel)
machine. Indeed, the LLVM NVPTX usage guide [1] recommends _internalizing_ the
symbols from libdevice after linking it with the kernel module; more precisely,
it recommends to first link the kernel module with libdevice, and subsequently
internalize all functions that we don't explicitly want exported (the public
kernel functions).

Clang doesn't have a command-line option to internalize symbols. Indeed, it
would be somewhat ambiguous when in the compilation process to do said
internalization. The LLVM command-line tool that _can_ do internalization is
`llvm-link`, the tool for linking LLVM modules together (and doing little
else). So translating the recommended [1] strategy to command-line tools
(because linking with LLVM through bindings is a version nightmare -- been
there, done that, not again), we get the following sensible procedure:

$ llvm-link --internalize kernel.ll libdevice.bc -o kernel-linked.bc
$ clang --target=... kernel-linked.bc -o kernel.sass

However, llvm-link is not clang, and we'd very much like to depend _only_ on
clang, not on the full LLVM suite of tools. Especially not for this vexingly
small bit of functionality! But clang is huge, and surely it can do
internalization somehow?

It turns out it can, but they did their absolute best to hide it. (All
references in this paragraph are to LLVM HEAD on 2024-12-04: 7954a0514ba7de.)
The workhorse function, called from `llvm-link`, is internalizeModule(). This
function is also called from clang in BackendConsumer::LinkInModules() in
clang/lib/CodeGen/CodeGenAction.cpp, but only if .Internalize is set on the
CodeGenAction::LinkModule in question. In CompilerInvocation::ParseCodeGenArgs
(clang/lib/Frontend/CompilerInvocation.cpp), we see that _some_ field called
"Internalize" is set on _something_ (not a LinkModule, but whatever?) if the
OPT_mlink_builtin_bitcode flag is set. Of course, no documentation anywhere
explains what this option does; the only mention I could find anywhere is here
[2], as well as some mailing list posts / issue tracker comments mentioning it.
How do we use the option? Well, it's not a clang option, it's actually a (I
think!) cc1 option, so you have to do:

$ clang -Xclang -mlink-builtin-bitcode -Xclang libdevice.bc

This makes clang internalize everything in that module that is not globally
exported (I think), which is what we want.

[1]: https://releases.llvm.org/19.1.0/docs/NVPTXUsage.html#linking-with-libdevice
[2]: https://clang.llvm.org/docs/OffloadingDesign.html#offload-device-compilation
-}

filterClangStderr :: String -> String
filterClangStderr = unlines . filter (not . isShflSyncWarn) . lines
  where
    -- ptxas warns about use of shfl instructions without the .sync suffix on
    -- CC 6.0, because such non-sync shuffles are deprecated (and indeed
    -- removed in CC 7.0). We still use them in CC 6.0 (and not any more in CC
    -- 7.0) because the shfl.sync in CC 6.0 has restrictions:
    --
    -- > For .target `sm_6x` or below, all threads in `membermask` must execute
    -- > the same `shfl.sync` instruction in convergence, and only threads
    -- > belonging to some `membermask` can be active when the `shfl.sync`
    -- > instruction is executed. Otherwise, the behavior is undefined.
    -- (https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-shfl-sync)
    --
    -- Perhaps we do use the shuffles in convergence, but we don't want to risk
    -- it. Hence in CC 6.0, we still use non-sync shuffles.
    --
    -- The ptxas warning cannot be turned off, however, and is **incredibly**
    -- noisy (there's a warning for every single shfl instruction). Hence we
    -- filter them out here.
    --
    -- > ptxas /tmp/--f8a421.s, line 119; warning : Instruction 'shfl' without '.sync' is deprecated since PTX ISA version 6.0 and will be discontinued in a future PTX ISA version
    isShflSyncWarn line =
      let (presemi, postsemi) = break (== ';') line
      in takeWhile (/= ' ') presemi == "ptxas" &&
           postsemi == "; warning : Instruction 'shfl' without '.sync' is deprecated since " ++
                       "PTX ISA version 6.0 and will be discontinued in a future PTX ISA version"

-- | Returns a human-readable error message in case the device is unsupported,
-- and Nothing if everything is alright.
isDeviceSupported :: CUDA.Compute -> Maybe String
isDeviceSupported cc@(CUDA.Compute m _)
  -- We require shfl instructions which are available only from CC 3.0.
  | m >= 3 = Nothing
  | otherwise = Just $
      "Your GPU has compute capability " ++ show cc ++ ", but only >= 3.0 is supported."

accPreludePTX :: String
accPreludePTX = unlines
  -- see Data.Array.Accelerate.LLVM.PTX.CodeGen.Base.nanosleep for why this is a hand-written function
  ["define private void @" ++ name_nanosleep ++ "(i32 noundef %0) alwaysinline convergent nounwind {"
  ,"  tail call void asm sideeffect \"nanosleep.u32 $0;\", \"r\"(i32 %0)"
  ,"  ret void"
  ,"}"]
  where
    name_nanosleep = let LLVM.Label name = LLVM.makeAccPreludeLabel "nanosleep" in SBS8.unpack name
