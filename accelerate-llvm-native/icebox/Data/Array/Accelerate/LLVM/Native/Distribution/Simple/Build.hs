{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Distribution.Simple.Build
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Copied from: https://github.com/haskell/cabal/blob/2.0/Cabal/Distribution/Simple/Build.hs
--

module Data.Array.Accelerate.LLVM.Native.Distribution.Simple.Build (

  build,

) where

import qualified Data.Array.Accelerate.LLVM.Native.Distribution.Simple.GHC as Acc

import qualified Distribution.Simple.Build as Cabal

import Distribution.Types.Dependency
import Distribution.Types.LocalBuildInfo
import Distribution.Types.TargetInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.ForeignLib
import Distribution.Types.MungedPackageId
import Distribution.Types.MungedPackageName
import Distribution.Types.UnqualComponentName
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ExecutableScope

import Distribution.Package
import Distribution.Backpack
import Distribution.Backpack.DescribeUnitId
import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
#if !MIN_VERSION_Cabal(2,3,0)
import qualified Distribution.Simple.JHC   as JHC
import qualified Distribution.Simple.LHC   as LHC
#endif
import qualified Distribution.Simple.UHC   as UHC
import qualified Distribution.Simple.HaskellSuite as HaskellSuite
import qualified Distribution.Simple.PackageIndex as Index

import qualified Distribution.Simple.Program.HcPkg as HcPkg

import Distribution.Simple.Compiler hiding (Flag)
import Distribution.PackageDescription hiding (Flag)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.InstalledPackageInfo (InstalledPackageInfo)

import Distribution.Simple.Setup
import Distribution.Simple.BuildTarget
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Db
import Distribution.Simple.BuildPaths
import Distribution.Simple.Configure
import Distribution.Simple.Register
import Distribution.Simple.Test.LibV09
import Distribution.Simple.Utils

import Distribution.Text
import Distribution.Verbosity

import Distribution.Compat.Graph (IsNode(..))

import Control.Monad
import qualified Data.Set as Set
import System.FilePath ( (</>), (<.>) )
import System.Directory ( getCurrentDirectory )


build    :: PackageDescription  -- ^ Mostly information from the .cabal file
         -> LocalBuildInfo      -- ^ Configuration information
         -> BuildFlags          -- ^ Flags that the user passed to build
         -> [ PPSuffixHandler ] -- ^ preprocessors to run before compiling
         -> IO ()
build pkg_descr lbi flags suffixes = do
  targets <- readTargetInfos verbosity pkg_descr lbi (buildArgs flags)
  let componentsToBuild = neededTargetsInBuildOrder' pkg_descr lbi (map nodeKey targets)
  info verbosity $ "Component build order: "
                ++ intercalate ", "
                    (map (showComponentName . componentLocalName . targetCLBI)
                        componentsToBuild)

  when (null targets) $
    -- Only bother with this message if we're building the whole package
    setupMessage verbosity "Building" (packageId pkg_descr)

  internalPackageDB <- createInternalPackageDB verbosity lbi distPref

  (\f -> foldM_ f (installedPkgs lbi) componentsToBuild) $ \index target -> do
    let comp = targetComponent target
        clbi = targetCLBI target
    Cabal.componentInitialBuildSteps distPref pkg_descr lbi clbi verbosity
    let bi     = componentBuildInfo comp
        progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
        lbi'   = lbi {
                   withPrograms  = progs',
                   withPackageDB = withPackageDB lbi ++ [internalPackageDB],
                   installedPkgs = index
                 }
    mb_ipi <- buildComponent verbosity (buildNumJobs flags) pkg_descr
                   lbi' suffixes comp clbi distPref
    return (maybe index (Index.insert `flip` index) mb_ipi)
  return ()
 where
  distPref  = fromFlag (buildDistPref flags)
  verbosity = fromFlag (buildVerbosity flags)


buildComponent :: Verbosity
               -> Flag (Maybe Int)
               -> PackageDescription
               -> LocalBuildInfo
               -> [PPSuffixHandler]
               -> Component
               -> ComponentLocalBuildInfo
               -> FilePath
               -> IO (Maybe InstalledPackageInfo)
buildComponent verbosity numJobs pkg_descr lbi suffixes
               comp@(CLib lib) clbi distPref = do
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras verbosity comp lbi
    setupMessage' verbosity "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    let libbi = libBuildInfo lib
        lib' = lib { libBuildInfo = addExtraCSources libbi extras }
    buildLib verbosity numJobs pkg_descr lbi lib' clbi

    let oneComponentRequested (OneComponentRequestedSpec _) = True
        oneComponentRequested _ = False
    -- Don't register inplace if we're only building a single component;
    -- it's not necessary because there won't be any subsequent builds
    -- that need to tag us
    if (not (oneComponentRequested (componentEnabledSpec lbi)))
      then do
        -- Register the library in-place, so exes can depend
        -- on internally defined libraries.
        pwd <- getCurrentDirectory
        let -- The in place registration uses the "-inplace" suffix, not an ABI hash
            installedPkgInfo = inplaceInstalledPackageInfo pwd distPref pkg_descr
                                    -- NB: Use a fake ABI hash to avoid
                                    -- needing to recompute it every build.
                                    (mkAbiHash "inplace") lib' lbi clbi

        debug verbosity $ "Registering inplace:\n" ++ (IPI.showInstalledPackageInfo installedPkgInfo)
        registerPackage verbosity (compiler lbi) (withPrograms lbi)
                        (withPackageDB lbi) installedPkgInfo
                        HcPkg.defaultRegisterOptions {
                          HcPkg.registerMultiInstance = True
                        }
        return (Just installedPkgInfo)
      else return Nothing

buildComponent verbosity numJobs pkg_descr lbi suffixes
               comp@(CFLib flib) clbi _distPref = do
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    setupMessage' verbosity "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    buildFLib verbosity numJobs pkg_descr lbi flib clbi
    return Nothing

buildComponent verbosity numJobs pkg_descr lbi suffixes
               comp@(CExe exe) clbi _ = do
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras verbosity comp lbi
    setupMessage' verbosity "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe verbosity numJobs pkg_descr lbi exe' clbi
    return Nothing


buildComponent verbosity numJobs pkg_descr lbi suffixes
               comp@(CTest test@TestSuite { testInterface = TestSuiteExeV10{} })
               clbi _distPref = do
    let exe = testSuiteExeV10AsExe test
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras verbosity comp lbi
    setupMessage' verbosity "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe verbosity numJobs pkg_descr lbi exe' clbi
    return Nothing


buildComponent verbosity numJobs pkg_descr lbi0 suffixes
               comp@(CTest
                 test@TestSuite { testInterface = TestSuiteLibV09{} })
               clbi -- This ComponentLocalBuildInfo corresponds to a detailed
                    -- test suite and not a real component. It should not
                    -- be used, except to construct the CLBIs for the
                    -- library and stub executable that will actually be
                    -- built.
               distPref = do
    pwd <- getCurrentDirectory
    let (pkg, lib, libClbi, lbi, ipi, exe, exeClbi) =
          testSuiteLibV09AsLibAndExe pkg_descr test clbi lbi0 distPref pwd
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras verbosity comp lbi
    setupMessage' verbosity "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    buildLib verbosity numJobs pkg lbi lib libClbi
    -- NB: need to enable multiple instances here, because on 7.10+
    -- the package name is the same as the library, and we still
    -- want the registration to go through.
    registerPackage verbosity (compiler lbi) (withPrograms lbi)
                    (withPackageDB lbi) ipi
                    HcPkg.defaultRegisterOptions {
                      HcPkg.registerMultiInstance = True
                    }
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe verbosity numJobs pkg_descr lbi exe' exeClbi
    return Nothing -- Can't depend on test suite


buildComponent verbosity _ _ _ _
               (CTest TestSuite { testInterface = TestSuiteUnsupported tt })
               _ _ =
    die' verbosity $ "No support for building test suite type " ++ display tt


buildComponent verbosity numJobs pkg_descr lbi suffixes
               comp@(CBench bm@Benchmark { benchmarkInterface = BenchmarkExeV10 {} })
               clbi _ = do
    let (exe, exeClbi) = benchmarkExeV10asExe bm clbi
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras verbosity comp lbi
    setupMessage' verbosity "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe verbosity numJobs pkg_descr lbi exe' exeClbi
    return Nothing


buildComponent verbosity _ _ _ _
               (CBench Benchmark { benchmarkInterface = BenchmarkUnsupported tt })
               _ _ =
    die' verbosity $ "No support for building benchmark type " ++ display tt



-- | Add extra C sources generated by preprocessing to build
-- information.
addExtraCSources :: BuildInfo -> [FilePath] -> BuildInfo
addExtraCSources bi extras = bi { cSources = new }
  where new = Set.toList $ old `Set.union` exs
        old = Set.fromList $ cSources bi
        exs = Set.fromList extras


-- | Translate a exe-style 'TestSuite' component into an exe for building
testSuiteExeV10AsExe :: TestSuite -> Executable
testSuiteExeV10AsExe test@TestSuite { testInterface = TestSuiteExeV10 _ mainFile } =
    Executable {
      exeName    = testName test,
      modulePath = mainFile,
      exeScope   = ExecutablePublic,
      buildInfo  = testBuildInfo test
    }
testSuiteExeV10AsExe TestSuite{} = error "testSuiteExeV10AsExe: wrong kind"


-- | Translate a lib-style 'TestSuite' component into a lib + exe for building
testSuiteLibV09AsLibAndExe :: PackageDescription
                           -> TestSuite
                           -> ComponentLocalBuildInfo
                           -> LocalBuildInfo
                           -> FilePath
                           -> FilePath
                           -> (PackageDescription,
                               Library, ComponentLocalBuildInfo,
                               LocalBuildInfo,
                               IPI.InstalledPackageInfo,
                               Executable, ComponentLocalBuildInfo)
testSuiteLibV09AsLibAndExe pkg_descr
                     test@TestSuite { testInterface = TestSuiteLibV09 _ m }
                     clbi lbi distPref pwd =
    (pkg, lib, libClbi, lbi, ipi, exe, exeClbi)
  where
    bi  = testBuildInfo test
    lib = Library {
            libName = Nothing,
            exposedModules = [ m ],
            reexportedModules = [],
            signatures = [],
            libExposed     = True,
            libBuildInfo   = bi
          }
    -- This is, like, the one place where we use a CTestName for a library.
    -- Should NOT use library name, since that could conflict!
    PackageIdentifier pkg_name pkg_ver = package pkg_descr
    compat_name = computeCompatPackageName pkg_name (Just (testName test))
    compat_key = computeCompatPackageKey (compiler lbi) compat_name pkg_ver (componentUnitId clbi)
    libClbi = LibComponentLocalBuildInfo
                { componentPackageDeps = componentPackageDeps clbi
                , componentInternalDeps = componentInternalDeps clbi
                , componentIsIndefinite_ = False
                , componentExeDeps = componentExeDeps clbi
                , componentLocalName = CSubLibName (testName test)
                , componentIsPublic = False
                , componentIncludes = componentIncludes clbi
                , componentUnitId = componentUnitId clbi
                , componentComponentId = componentComponentId clbi
                , componentInstantiatedWith = []
                , componentCompatPackageName = compat_name
                , componentCompatPackageKey = compat_key
                , componentExposedModules = [IPI.ExposedModule m Nothing]
                }
    pkg = pkg_descr {
            package      = (package pkg_descr) { pkgName = mkPackageName $ unMungedPackageName compat_name }
#if !MIN_VERSION_Cabal(2,3,0)
          , buildDepends = targetBuildDepends $ testBuildInfo test
#endif
          , executables  = []
          , testSuites   = []
          , subLibraries = [lib]
          }
    ipi    = inplaceInstalledPackageInfo pwd distPref pkg (mkAbiHash "") lib lbi libClbi
    testDir = buildDir lbi </> stubName test
          </> stubName test ++ "-tmp"
    testLibDep = thisPackageVersion $ package pkg
    exe = Executable {
            exeName    = mkUnqualComponentName $ stubName test,
            modulePath = stubFilePath test,
            exeScope   = ExecutablePublic,
            buildInfo  = (testBuildInfo test) {
                           hsSourceDirs       = [ testDir ],
                           targetBuildDepends = testLibDep
                             : (targetBuildDepends $ testBuildInfo test)
                         }
          }
    -- | The stub executable needs a new 'ComponentLocalBuildInfo'
    -- that exposes the relevant test suite library.
    deps = (IPI.installedUnitId ipi, mungedId ipi)
         : (filter (\(_, x) -> let name = unMungedPackageName $ mungedName x
                               in name == "Cabal" || name == "base")
                   (componentPackageDeps clbi))
    exeClbi = ExeComponentLocalBuildInfo {
                -- TODO: this is a hack, but as long as this is unique
                -- (doesn't clobber something) we won't run into trouble
                componentUnitId = mkUnitId (stubName test),
                componentComponentId = mkComponentId (stubName test),
                componentInternalDeps = [componentUnitId clbi],
                componentExeDeps = [],
                componentLocalName = CExeName $ mkUnqualComponentName $ stubName test,
                componentPackageDeps = deps,
                -- Assert DefUnitId invariant!
                -- Executable can't be indefinite, so dependencies must
                -- be definite packages.
                componentIncludes = zip (map (DefiniteUnitId . unsafeMkDefUnitId . fst) deps)
                                        (repeat defaultRenaming)
              }
testSuiteLibV09AsLibAndExe _ TestSuite{} _ _ _ _ = error "testSuiteLibV09AsLibAndExe: wrong kind"


-- | Translate a exe-style 'Benchmark' component into an exe for building
benchmarkExeV10asExe :: Benchmark -> ComponentLocalBuildInfo
                     -> (Executable, ComponentLocalBuildInfo)
benchmarkExeV10asExe bm@Benchmark { benchmarkInterface = BenchmarkExeV10 _ f }
                     clbi =
    (exe, exeClbi)
  where
    exe = Executable {
            exeName    = benchmarkName bm,
            modulePath = f,
            exeScope   = ExecutablePublic,
            buildInfo  = benchmarkBuildInfo bm
          }
    exeClbi = ExeComponentLocalBuildInfo {
                componentUnitId = componentUnitId clbi,
                componentComponentId = componentComponentId clbi,
                componentLocalName = CExeName (benchmarkName bm),
                componentInternalDeps = componentInternalDeps clbi,
                componentExeDeps = componentExeDeps clbi,
                componentPackageDeps = componentPackageDeps clbi,
                componentIncludes = componentIncludes clbi
              }
benchmarkExeV10asExe Benchmark{} _ = error "benchmarkExeV10asExe: wrong kind"


-- | Initialize a new package db file for libraries defined
-- internally to the package.
createInternalPackageDB :: Verbosity -> LocalBuildInfo -> FilePath
                        -> IO PackageDB
createInternalPackageDB verbosity lbi distPref = do
    existsAlready <- doesPackageDBExist dbPath
    when existsAlready $ deletePackageDB dbPath
    createPackageDB verbosity (compiler lbi) (withPrograms lbi) False dbPath
    return (SpecificPackageDB dbPath)
  where
    dbPath = internalPackageDBPath lbi distPref

addInternalBuildTools :: PackageDescription -> LocalBuildInfo -> BuildInfo
                      -> ProgramDb -> ProgramDb
addInternalBuildTools pkg lbi bi progs =
    foldr updateProgram progs internalBuildTools
  where
    internalBuildTools =
      [ simpleConfiguredProgram toolName' (FoundOnSystem toolLocation)
      | toolName <- getAllInternalToolDependencies pkg bi
      , let toolName' = unUnqualComponentName toolName
#if MIN_VERSION_Cabal(2,3,0)
      , let toolLocation = buildDir lbi </> toolName' </> toolName' <.> exeExtension (hostPlatform lbi) ]
#else
      , let toolLocation = buildDir lbi </> toolName' </> toolName' <.> exeExtension ]
#endif


-- TODO: build separate libs in separate dirs so that we can build
-- multiple libs, e.g. for 'LibTest' library-style test suites
buildLib :: Verbosity -> Flag (Maybe Int)
                      -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity numJobs pkg_descr lbi lib clbi =
  case compilerFlavor (compiler lbi) of
    GHC   -> Acc.buildLib   verbosity numJobs pkg_descr lbi lib clbi    -- XXX only change here
    GHCJS -> GHCJS.buildLib verbosity numJobs pkg_descr lbi lib clbi
#if !MIN_VERSION_Cabal(2,3,0)
    JHC   -> JHC.buildLib   verbosity         pkg_descr lbi lib clbi
    LHC   -> LHC.buildLib   verbosity         pkg_descr lbi lib clbi
#endif
    UHC   -> UHC.buildLib   verbosity         pkg_descr lbi lib clbi
    HaskellSuite {} -> HaskellSuite.buildLib verbosity pkg_descr lbi lib clbi
    _    -> die' verbosity "Building is not supported with this compiler."

-- | Build a foreign library
--
-- NOTE: We assume that we already checked that we can actually build the
-- foreign library in configure.
buildFLib :: Verbosity -> Flag (Maybe Int)
                       -> PackageDescription -> LocalBuildInfo
                       -> ForeignLib         -> ComponentLocalBuildInfo -> IO ()
buildFLib verbosity numJobs pkg_descr lbi flib clbi =
    case compilerFlavor (compiler lbi) of
      GHC -> GHC.buildFLib verbosity numJobs pkg_descr lbi flib clbi
      _   -> die' verbosity "Building is not supported with this compiler."

buildExe :: Verbosity -> Flag (Maybe Int)
                      -> PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe verbosity numJobs pkg_descr lbi exe clbi =
  case compilerFlavor (compiler lbi) of
    GHC   -> GHC.buildExe   verbosity numJobs pkg_descr lbi exe clbi
    GHCJS -> GHCJS.buildExe verbosity numJobs pkg_descr lbi exe clbi
#if !MIN_VERSION_Cabal(2,3,0)
    JHC   -> JHC.buildExe   verbosity         pkg_descr lbi exe clbi
    LHC   -> LHC.buildExe   verbosity         pkg_descr lbi exe clbi
#endif
    UHC   -> UHC.buildExe   verbosity         pkg_descr lbi exe clbi
    _     -> die' verbosity "Building is not supported with this compiler."


