{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Distribution.PackageDescription
import Distribution.PackageDescription.PrettyPrint
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Setup                                    as Setup
import Distribution.Verbosity
import qualified Distribution.InstalledPackageInfo                  as Installed

#if MIN_VERSION_Cabal(3,8,0)
import Distribution.Simple.PackageDescription
#elif MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif
#if MIN_VERSION_Cabal(3,14,0)
-- Note [Cabal 3.14]
--
-- If you change any path stuff, either test that the package still works with
-- Cabal 3.12 or stop declaring support for it in cuda.cabal. (If you do the
-- latter, also remove all of the other conditionals in this file.)
-- Note that supporting old versions of Cabal is useful for being able to run
-- e.g. Accelerate on old GPU clusters, which is nice.
import Distribution.Utils.Path (SymbolicPath, FileOrDir(File, Dir), Lib, Include, Pkg, CWD, makeSymbolicPath)
import qualified Distribution.Types.LocalBuildConfig as LBC
#endif

import System.FilePath


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { postConf    = postConfHook
  , preBuild    = readHook buildVerbosity workingDirFlag
  , preRepl     = readHook replVerbosity workingDirFlag
  , preCopy     = readHook copyVerbosity workingDirFlag
  , preInst     = readHook installVerbosity workingDirFlag
  , preHscolour = readHook hscolourVerbosity workingDirFlag
  , preHaddock  = readHook haddockVerbosity workingDirFlag
  , preReg      = readHook regVerbosity workingDirFlag
  , preUnreg    = readHook regVerbosity workingDirFlag
  }
  where
    readHook :: (a -> Setup.Flag Verbosity) -> (a -> Setup.Flag CWDPath) -> Args -> a -> IO HookedBuildInfo
    readHook verbosity cwd _ flags = readHookedBuildInfoWithCWD (fromFlag (verbosity flags)) (flagToMaybe (cwd flags)) (makeSymbolicPath buildinfo_file)

    postConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postConfHook args flags pkg_desc lbi = do
      let accelerate_pkg     = case searchByName (installedPkgs lbi) "accelerate" of
                                 Unambiguous [x] -> x
                                 _               -> error "accelerate package was not found or is ambiguous"

          dyld_library_name  = mkSharedLibName (hostPlatform lbi) (compilerId (compiler lbi)) (installedUnitId accelerate_pkg)
          dyld_install_dir:_ = case Installed.libraryDynDirs accelerate_pkg of
                                 [] -> Installed.libraryDirs accelerate_pkg
                                 ds -> ds

          buildinfo        = emptyBuildInfo { cppOptions = [ "-DACCELERATE_DYLD_LIBRARY_PATH=" ++ quote (dyld_install_dir </> dyld_library_name) ] }
          hooked_buildinfo = (Just buildinfo, [])
          pkg_desc'        = updatePackageDescription hooked_buildinfo pkg_desc


      writeHookedBuildInfo buildinfo_file hooked_buildinfo
      postConf simpleUserHooks args flags pkg_desc' lbi

buildinfo_file :: FilePath
buildinfo_file = "accelerate-llvm-native.buildinfo"

quote :: String -> String
#ifdef mingw32_HOST_OS
quote s = "\"" ++ (s >>= escape) ++ "\""
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape c   = [c]
#else
quote = show
#endif


-- Compatibility across Cabal 3.14 symbolic paths.
-- If we want to drop pre-Cabal-3.14 compatibility at some point, this should all be merged in above.

#if MIN_VERSION_Cabal(3,14,0)
type CWDPath = SymbolicPath CWD ('Dir Pkg)

regVerbosity :: RegisterFlags -> Flag Verbosity
regVerbosity = setupVerbosity . registerCommonFlags

workingDirFlag :: HasCommonFlags flags => flags -> Flag CWDPath
workingDirFlag = setupWorkingDir . getCommonFlags

-- makeSymbolicPath is an actual useful function in Cabal 3.14

class HasCommonFlags flags where getCommonFlags :: flags -> CommonSetupFlags
instance HasCommonFlags BuildFlags where getCommonFlags = buildCommonFlags
instance HasCommonFlags CleanFlags where getCommonFlags = cleanCommonFlags
instance HasCommonFlags ConfigFlags where getCommonFlags = configCommonFlags
instance HasCommonFlags CopyFlags where getCommonFlags = copyCommonFlags
instance HasCommonFlags InstallFlags where getCommonFlags = installCommonFlags
instance HasCommonFlags HscolourFlags where getCommonFlags = hscolourCommonFlags
instance HasCommonFlags HaddockFlags where getCommonFlags = haddockCommonFlags
instance HasCommonFlags RegisterFlags where getCommonFlags = registerCommonFlags
instance HasCommonFlags ReplFlags where getCommonFlags = replCommonFlags

readHookedBuildInfoWithCWD :: Verbosity -> Maybe CWDPath -> SymbolicPath Pkg 'File -> IO HookedBuildInfo
readHookedBuildInfoWithCWD = readHookedBuildInfo
#else
type CWDPath = ()

-- regVerbosity is still present as an actual field in Cabal 3.12

workingDirFlag :: flags -> Flag CWDPath
workingDirFlag _ = NoFlag

makeSymbolicPath :: FilePath -> FilePath
makeSymbolicPath = id

readHookedBuildInfoWithCWD :: Verbosity -> Maybe CWDPath -> FilePath -> IO HookedBuildInfo
readHookedBuildInfoWithCWD verb _ path = readHookedBuildInfo verb path
#endif
