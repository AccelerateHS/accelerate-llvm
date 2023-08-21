{-# LANGUAGE CPP #-}

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

import System.FilePath


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { postConf    = postConfHook
  , preBuild    = readHook buildVerbosity
  , preCopy     = readHook copyVerbosity
  , preInst     = readHook installVerbosity
  , preHscolour = readHook hscolourVerbosity
  , preHaddock  = readHook haddockVerbosity
  , preReg      = readHook regVerbosity
  , preUnreg    = readHook regVerbosity
  }
  where
    readHook :: (a -> Setup.Flag Verbosity) -> Args -> a -> IO HookedBuildInfo
    readHook verbosity _ flags = readHookedBuildInfo (fromFlag (verbosity flags)) buildinfo_file

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
    escape '"' = "\\\""
    escape c   = [c]
#else
quote = show
#endif

