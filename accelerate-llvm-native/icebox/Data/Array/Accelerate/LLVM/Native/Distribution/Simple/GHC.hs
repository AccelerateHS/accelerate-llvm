{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Distribution.Simple.GHC
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Copied from: https://github.com/haskell/cabal/blob/2.0/Cabal/Distribution/Simple/GHC.hs
--

module Data.Array.Accelerate.LLVM.Native.Distribution.Simple.GHC (

  buildLib,
  replLib,

) where

import qualified Data.Array.Accelerate.LLVM.Native.Distribution.Simple.GHC.Internal as Internal
import qualified Data.Array.Accelerate.LLVM.Native.Plugin.BuildInfo                 as Internal

import qualified Distribution.Simple.GHC as Cabal
import Distribution.PackageDescription as PD
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.ComponentLocalBuildInfo
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.Program
import qualified Distribution.Simple.Program.Ar    as Ar
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Version
import Distribution.System
import Distribution.Verbosity
import Distribution.Text
import Distribution.Utils.NubList
import Language.Haskell.Extension

import Control.Monad (when, unless)
import Data.List (nub)
import Data.Maybe (catMaybes)
import System.FilePath ( (</>), replaceExtension, isRelative )
import qualified Data.Map as Map


-- <https://github.com/haskell/cabal/blob/2.0/Cabal/Distribution/Simple/GHC.hs#L505>
--
buildLib, replLib :: Verbosity          -> Cabal.Flag (Maybe Int)
                  -> PackageDescription -> LocalBuildInfo
                  -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib = buildOrReplLib False
replLib  = buildOrReplLib True

buildOrReplLib :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Library            -> ComponentLocalBuildInfo -> IO ()
buildOrReplLib forRepl verbosity numJobs pkg_descr lbi lib clbi = do
  let uid = componentUnitId clbi
      libTargetDir = componentBuildDir lbi clbi
      whenVanillaLib forceVanilla =
        when (forceVanilla || withVanillaLib lbi)
      whenProfLib = when (withProfLib lbi)
      whenSharedLib forceShared =
        when (forceShared || withSharedLib lbi)
      whenGHCiLib = when (withGHCiLib lbi && withVanillaLib lbi)
      ifReplLib = when forRepl
      comp = compiler lbi
      ghcVersion = compilerVersion comp
      implInfo  = Cabal.getImplInfo comp
      platform@(Platform _hostArch hostOS) = hostPlatform lbi
      has_code = not (componentIsIndefinite clbi)

  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  let runGhcProg = runGHC verbosity ghcProg comp platform

  libBi <- hackThreadedFlag verbosity
             comp (withProfLib lbi) (libBuildInfo lib)

  let isGhcDynamic        = Cabal.isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      doingTH = EnableExtension TemplateHaskell `elem` allExtensions libBi
      forceVanillaLib = doingTH && not isGhcDynamic
      forceSharedLib  = doingTH &&     isGhcDynamic
      -- TH always needs default libs, even when building for profiling

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = libCoverage lbi
      -- TODO: Historically HPC files have been put into a directory which
      -- has the package name.  I'm going to avoid changing this for
      -- now, but it would probably be better for this to be the
      -- component ID instead...
      pkg_name = display (PD.package pkg_descr)
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | forRepl = mempty  -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way pkg_name
        | otherwise = mempty

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?
  let cObjs       = map (`replaceExtension` objExtension) (cSources libBi)
      baseOpts    = Cabal.componentGhcOptions verbosity lbi libBi clbi libTargetDir
      vanillaOpts = baseOpts `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptNumJobs      = numJobs,
                      ghcOptInputModules = toNubListR $ allLibModules lib clbi,
                      ghcOptHPCDir       = hpcdir Hpc.Vanilla
                    }

      profOpts    = vanillaOpts `mappend` mempty {
                      ghcOptProfilingMode = toFlag True,
                      ghcOptProfilingAuto = Internal.profDetailLevelFlag True
                                              (withProfLibDetail lbi),
                      ghcOptHiSuffix      = toFlag "p_hi",
                      ghcOptObjSuffix     = toFlag "p_o",
#if MIN_VERSION_Cabal(2,3,0)
                      ghcOptExtra         =              hcProfOptions GHC libBi,
#else
                      ghcOptExtra         = toNubListR $ hcProfOptions GHC libBi,
#endif
                      ghcOptHPCDir        = hpcdir Hpc.Prof
                    }

      sharedOpts  = vanillaOpts `mappend` mempty {
                      ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                      ghcOptFPic        = toFlag True,
                      ghcOptHiSuffix    = toFlag "dyn_hi",
                      ghcOptObjSuffix   = toFlag "dyn_o",
#if MIN_VERSION_Cabal(2,3,0)
                      ghcOptExtra       =              hcSharedOptions GHC libBi,
#else
                      ghcOptExtra       = toNubListR $ hcSharedOptions GHC libBi,
#endif
                      ghcOptHPCDir      = hpcdir Hpc.Dyn
                    }
      linkerOpts = mempty {
#if MIN_VERSION_Cabal(2,3,0)
                      ghcOptLinkOptions       =              PD.ldOptions libBi,
                      ghcOptLinkLibs          =              extraLibs libBi,
#else
                      ghcOptLinkOptions       = toNubListR $ PD.ldOptions libBi,
                      ghcOptLinkLibs          = toNubListR $ extraLibs libBi,
#endif
                      ghcOptLinkLibPath       = toNubListR $ extraLibDirs libBi,
                      ghcOptLinkFrameworks    = toNubListR $
                                                PD.frameworks libBi,
                      ghcOptLinkFrameworkDirs = toNubListR $
                                                PD.extraFrameworkDirs libBi,
                      ghcOptInputFiles     = toNubListR
                                             [libTargetDir </> x | x <- cObjs]
                   }
      replOpts    = vanillaOpts {
#if MIN_VERSION_Cabal(2,3,0)
                      ghcOptExtra        =              Internal.filterGhciFlags $ ghcOptExtra vanillaOpts,
#else
                      ghcOptExtra        = overNubListR Internal.filterGhciFlags $ ghcOptExtra vanillaOpts,
#endif
                      ghcOptNumJobs      = mempty
                    }
                    `mappend` linkerOpts
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeInteractive,
                      ghcOptOptimisation = toFlag GhcNoOptimisation
                    }

      vanillaSharedOpts = vanillaOpts `mappend` mempty {
                      ghcOptDynLinkMode  = toFlag GhcStaticAndDynamic,
                      ghcOptDynHiSuffix  = toFlag "dyn_hi",
                      ghcOptDynObjSuffix = toFlag "dyn_o",
                      ghcOptHPCDir       = hpcdir Hpc.Dyn
                    }

  unless (forRepl || null (allLibModules lib clbi)) $
    do let vanilla = whenVanillaLib forceVanillaLib (runGhcProg vanillaOpts)
           shared  = whenSharedLib  forceSharedLib  (runGhcProg sharedOpts)
           useDynToo = dynamicTooSupported &&
                       (forceVanillaLib || withVanillaLib lbi) &&
                       (forceSharedLib  || withSharedLib  lbi) &&
                       null (hcSharedOptions GHC libBi)
       if not has_code
        then vanilla
        else
         if useDynToo
          then do
              runGhcProg vanillaSharedOpts
              case (hpcdir Hpc.Dyn, hpcdir Hpc.Vanilla) of
                (Cabal.Flag dynDir, Cabal.Flag vanillaDir) ->
                    -- When the vanilla and shared library builds are done
                    -- in one pass, only one set of HPC module interfaces
                    -- are generated. This set should suffice for both
                    -- static and dynamically linked executables. We copy
                    -- the modules interfaces so they are available under
                    -- both ways.
                    copyDirectoryRecursive verbosity dynDir vanillaDir
                _ -> return ()
          else if isGhcDynamic
            then do shared;  vanilla
            else do vanilla; shared
       when has_code $ whenProfLib (runGhcProg profOpts)

  -- build any C sources
  unless (not has_code || null (cSources libBi)) $ do
    info verbosity "Building C Sources..."
    sequence_
      [ do let baseCcOpts    = Cabal.componentCcGhcOptions verbosity
                               lbi libBi clbi libTargetDir filename
               vanillaCcOpts = if isGhcDynamic
                               -- Dynamic GHC requires C sources to be built
                               -- with -fPIC for REPL to work. See #2207.
                               then baseCcOpts { ghcOptFPic = toFlag True }
                               else baseCcOpts
               profCcOpts    = vanillaCcOpts `mappend` mempty {
                                 ghcOptProfilingMode = toFlag True,
                                 ghcOptObjSuffix     = toFlag "p_o"
                               }
               sharedCcOpts  = vanillaCcOpts `mappend` mempty {
                                 ghcOptFPic        = toFlag True,
                                 ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                                 ghcOptObjSuffix   = toFlag "dyn_o"
                               }
               odir          = fromFlag (ghcOptObjDir vanillaCcOpts)
           createDirectoryIfMissingVerbose verbosity True odir
           let runGhcProgIfNeeded ccOpts = do
                 needsRecomp <- checkNeedsRecompilation filename ccOpts
                 when needsRecomp $ runGhcProg ccOpts
           runGhcProgIfNeeded vanillaCcOpts
           unless forRepl $
             whenSharedLib forceSharedLib (runGhcProgIfNeeded sharedCcOpts)
           unless forRepl $ whenProfLib (runGhcProgIfNeeded profCcOpts)
      | filename <- cSources libBi]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.

  when has_code . ifReplLib $ do
    when (null (allLibModules lib clbi)) $ warn verbosity "No exposed modules"
    ifReplLib (runGhcProg replOpts)

  -- link:
  when has_code . unless forRepl $ do
    info verbosity "Linking..."
    let cProfObjs   = map (`replaceExtension` ("p_" ++ objExtension))
                      (cSources libBi)
        cSharedObjs = map (`replaceExtension` ("dyn_" ++ objExtension))
                      (cSources libBi)
        compiler_id = compilerId (compiler lbi)
        vanillaLibFilePath = libTargetDir </> mkLibName uid
        profileLibFilePath = libTargetDir </> mkProfLibName uid
#if MIN_VERSION_Cabal(2,3,0)
        sharedLibFilePath    = libTargetDir   </> mkSharedLibName platform compiler_id uid
        sharedLibInstallPath = libInstallPath </> mkSharedLibName platform compiler_id uid
#else
        sharedLibFilePath    = libTargetDir   </> mkSharedLibName          compiler_id uid
        sharedLibInstallPath = libInstallPath </> mkSharedLibName          compiler_id uid
#endif
        ghciLibFilePath    = libTargetDir </> Internal.mkGHCiLibName uid
        libInstallPath     = libdir $ absoluteComponentInstallDirs pkg_descr lbi uid NoCopyDest

    stubObjs <- catMaybes <$> sequenceA
      [ findFileWithExtension [objExtension] [libTargetDir]
          (ModuleName.toFilePath x ++"_stub")
      | ghcVersion < mkVersion [7,2] -- ghc-7.2+ does not make _stub.o files
      , x <- allLibModules lib clbi ]
    stubProfObjs <- catMaybes <$> sequenceA
      [ findFileWithExtension ["p_" ++ objExtension] [libTargetDir]
          (ModuleName.toFilePath x ++"_stub")
      | ghcVersion < mkVersion [7,2] -- ghc-7.2+ does not make _stub.o files
      , x <- allLibModules lib clbi ]
    stubSharedObjs <- catMaybes <$> sequenceA
      [ findFileWithExtension ["dyn_" ++ objExtension] [libTargetDir]
          (ModuleName.toFilePath x ++"_stub")
      | ghcVersion < mkVersion [7,2] -- ghc-7.2+ does not make _stub.o files
      , x <- allLibModules lib clbi ]

    hObjs     <- Internal.getHaskellObjects implInfo lib lbi clbi
                      libTargetDir objExtension True
    hProfObjs <-
      if withProfLib lbi
              then Internal.getHaskellObjects implInfo lib lbi clbi
                      libTargetDir ("p_" ++ objExtension) True
              else return []
    hSharedObjs <-
      if withSharedLib lbi
              then Internal.getHaskellObjects implInfo lib lbi clbi
                      libTargetDir ("dyn_" ++ objExtension) False
              else return []

    -- XXX: This is the only change; determine if there are any
    -- accelerate-generated object files which need to linked into the final
    -- libraries.
    accObjs   <- fmap (nub . concat . Map.elems)
               $ Internal.readBuildInfo
               $ Internal.mkBuildInfoFileName libTargetDir

    unless (null accObjs && null hObjs && null cObjs && null stubObjs) $ do
      rpaths <- getRPaths lbi clbi

      let staticObjectFiles =
                 hObjs
              ++ accObjs
              ++ map (libTargetDir </>) cObjs
              ++ stubObjs
          profObjectFiles =
                 hProfObjs
              ++ accObjs
              ++ map (libTargetDir </>) cProfObjs
              ++ stubProfObjs
          ghciObjFiles =
                 hObjs
              ++ accObjs
              ++ map (libTargetDir </>) cObjs
              ++ stubObjs
          dynamicObjectFiles =
                 hSharedObjs
              ++ accObjs
              ++ map (libTargetDir </>) cSharedObjs
              ++ stubSharedObjs
          -- After the relocation lib is created we invoke ghc -shared
          -- with the dependencies spelled out as -package arguments
          -- and ghc invokes the linker with the proper library paths
          ghcSharedLinkArgs =
              mempty {
                ghcOptShared             = toFlag True,
                ghcOptDynLinkMode        = toFlag GhcDynamicOnly,
                ghcOptInputFiles         = toNubListR dynamicObjectFiles,
                ghcOptOutputFile         = toFlag sharedLibFilePath,
#if MIN_VERSION_Cabal(2,3,0)
                ghcOptExtra              =              hcSharedOptions GHC libBi,
#else
                ghcOptExtra              = toNubListR $ hcSharedOptions GHC libBi,
#endif
                -- For dynamic libs, Mac OS/X needs to know the install location
                -- at build time. This only applies to GHC < 7.8 - see the
                -- discussion in #1660.
                ghcOptDylibName          = if hostOS == OSX
                                              && ghcVersion < mkVersion [7,8]
                                            then toFlag sharedLibInstallPath
                                            else mempty,
                ghcOptHideAllPackages    = toFlag True,
                ghcOptNoAutoLinkPackages = toFlag True,
                ghcOptPackageDBs         = withPackageDB lbi,
                ghcOptThisUnitId = case clbi of
                    LibComponentLocalBuildInfo { componentCompatPackageKey = pk }
                      -> toFlag pk
                    _ -> mempty,
                ghcOptThisComponentId = case clbi of
                    LibComponentLocalBuildInfo { componentInstantiatedWith = insts } ->
                        if null insts
                            then mempty
                            else toFlag (componentComponentId clbi)
                    _ -> mempty,
                ghcOptInstantiatedWith = case clbi of
                    LibComponentLocalBuildInfo { componentInstantiatedWith = insts }
                      -> insts
                    _ -> [],
                ghcOptPackages           = toNubListR $
                                           Internal.mkGhcOptPackages clbi ,
#if MIN_VERSION_Cabal(2,3,0)
                ghcOptLinkLibs           =              extraLibs libBi,
#else
                ghcOptLinkLibPath        = toNubListR $ extraLibDirs libBi,
#endif
                ghcOptLinkFrameworks     = toNubListR $ PD.frameworks libBi,
                ghcOptLinkFrameworkDirs  =
                  toNubListR $ PD.extraFrameworkDirs libBi,
                ghcOptRPaths             = rpaths
              }

      info verbosity (show (ghcOptPackages ghcSharedLinkArgs))

      whenVanillaLib False $
        Ar.createArLibArchive verbosity lbi vanillaLibFilePath staticObjectFiles

      whenProfLib $
        Ar.createArLibArchive verbosity lbi profileLibFilePath profObjectFiles

      whenGHCiLib $ do
        (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
        Internal.combineObjectFiles verbosity lbi ldProg
          ghciLibFilePath ghciObjFiles

      whenSharedLib False $
        runGhcProg ghcSharedLinkArgs


-- | Returns True if the modification date of the given source file is newer than
-- the object file we last compiled for it, or if no object file exists yet.
checkNeedsRecompilation :: FilePath -> GhcOptions -> IO Bool
checkNeedsRecompilation filename opts = filename `moreRecentFile` oname
    where oname = getObjectFileName filename opts

-- | Finds the object file name of the given source file
getObjectFileName :: FilePath -> GhcOptions -> FilePath
getObjectFileName filename opts = oname
    where odir  = fromFlag (ghcOptObjDir opts)
          oext  = fromFlagOrDefault "o" (ghcOptObjSuffix opts)
          oname = odir </> replaceExtension filename oext

-- | Calculate the RPATHs for the component we are building.
--
-- Calculates relative RPATHs when 'relocatable' is set.
getRPaths :: LocalBuildInfo
          -> ComponentLocalBuildInfo -- ^ Component we are building
          -> IO (NubListR FilePath)
getRPaths lbi clbi | supportRPaths hostOS = do
    libraryPaths <- depLibraryPaths False (relocatable lbi) lbi clbi
    let hostPref = case hostOS of
                     OSX -> "@loader_path"
                     _   -> "$ORIGIN"
        relPath p = if isRelative p then hostPref </> p else p
        rpaths    = toNubListR (map relPath libraryPaths)
    return rpaths
  where
    (Platform _ hostOS) = hostPlatform lbi

    -- The list of RPath-supported operating systems below reflects the
    -- platforms on which Cabal's RPATH handling is tested. It does _NOT_
    -- reflect whether the OS supports RPATH.

    -- E.g. when this comment was written, the *BSD operating systems were
    -- untested with regards to Cabal RPATH handling, and were hence set to
    -- 'False', while those operating systems themselves do support RPATH.
    supportRPaths Linux       = True
    supportRPaths Windows     = False
    supportRPaths OSX         = True
    supportRPaths FreeBSD     = False
    supportRPaths OpenBSD     = False
    supportRPaths NetBSD      = False
    supportRPaths DragonFly   = False
    supportRPaths Solaris     = False
    supportRPaths AIX         = False
    supportRPaths HPUX        = False
    supportRPaths IRIX        = False
    supportRPaths HaLVM       = False
    supportRPaths IOS         = False
    supportRPaths Android     = False
    supportRPaths Ghcjs       = False
    supportRPaths Hurd        = False
    supportRPaths (OtherOS _) = False
    -- Do _not_ add a default case so that we get a warning here when a new OS
    -- is added.

getRPaths _ _ = return mempty

-- | Filter the "-threaded" flag when profiling as it does not
--   work with ghc-6.8 and older.
hackThreadedFlag :: Verbosity -> Compiler -> Bool -> BuildInfo -> IO BuildInfo
hackThreadedFlag _ _ _ = return

-- -----------------------------------------------------------------------------
-- Utils

supportsDynamicToo :: Compiler -> Bool
supportsDynamicToo = Internal.ghcLookupProperty "Support dynamic-too"

