{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Distribution.Simple.GHC.Internal
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Copied from: https://github.com/haskell/cabal/blob/2.0/Cabal/Distribution/Simple/GHC/Internal.hs
--

module Data.Array.Accelerate.LLVM.Native.Distribution.Simple.GHC.Internal (

  mkGHCiLibName,
  ghcLookupProperty,
  filterGhciFlags,
  getHaskellObjects,
  mkGhcOptPackages,
  profDetailLevelFlag,
  combineObjectFiles

) where

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Backpack
#endif
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program (ConfiguredProgram)
import Distribution.Simple.Program.GHC
import qualified Distribution.Simple.Program.Ld as Ld
import Distribution.Simple.Setup
import Distribution.Simple
import Distribution.Verbosity (Verbosity)
import qualified Distribution.ModuleName as ModuleName

import qualified Data.Map as Map
import System.Directory ( getDirectoryContents )
import System.FilePath ( (</>), (<.>), takeExtension )


-- | Strip out flags that are not supported in ghci
filterGhciFlags :: [String] -> [String]
filterGhciFlags = filter supported
  where
    supported ('-':'O':_) = False
    supported "-debug"    = False
    supported "-threaded" = False
    supported "-ticky"    = False
    supported "-eventlog" = False
    supported "-prof"     = False
    supported "-unreg"    = False
    supported _           = True

#if MIN_VERSION_Cabal(1,24,0)
mkGHCiLibName :: UnitId -> String
mkGHCiLibName lib = getHSLibraryName lib <.> "o"
#else
mkGHCiLibName :: LibraryName -> String
mkGHCiLibName (LibraryName lib) = lib <.> "o"
#endif

ghcLookupProperty :: String -> Compiler -> Bool
ghcLookupProperty prop comp =
  case Map.lookup prop (compilerProperties comp) of
    Just "YES" -> True
    _          -> False

-- when using -split-objs, we need to search for object files in the
-- Module_split directory for each module.
getHaskellObjects :: _GhcImplInfo -> Library -> LocalBuildInfo
                  -> ComponentLocalBuildInfo
                  -> FilePath -> String -> Bool -> IO [FilePath]
getHaskellObjects _implInfo lib lbi clbi pref wanted_obj_ext allow_split_objs
  | splitObjs lbi && allow_split_objs = do
        let splitSuffix = "_" ++ wanted_obj_ext ++ "_split"
            dirs = [ pref </> (ModuleName.toFilePath x ++ splitSuffix)
                   | x <- allLibModules lib clbi ]
        objss <- traverse getDirectoryContents dirs
        let objs = [ dir </> obj
                   | (objs',dir) <- zip objss dirs, obj <- objs',
                     let obj_ext = takeExtension obj,
                     '.':wanted_obj_ext == obj_ext ]
        return objs
  | otherwise  =
        return [ pref </> ModuleName.toFilePath x <.> wanted_obj_ext
               | x <- allLibModules lib clbi ]

#if MIN_VERSION_Cabal(2,0,0)
mkGhcOptPackages :: ComponentLocalBuildInfo
                 -> [(OpenUnitId, ModuleRenaming)]
mkGhcOptPackages = componentIncludes
#else
mkGhcOptPackages :: ComponentLocalBuildInfo
                 -> [(InstalledPackageId, PackageId, ModuleRenaming)]
mkGhcOptPackages clbi =
  map (\(i,p) -> (i,p,lookupRenaming p (componentPackageRenaming clbi)))
      (componentPackageDeps clbi)
#endif

profDetailLevelFlag :: Bool -> ProfDetailLevel -> Flag GhcProfAuto
profDetailLevelFlag forLib mpl =
    case mpl of
      ProfDetailNone                -> mempty
      ProfDetailDefault | forLib    -> toFlag GhcProfAutoExported
                        | otherwise -> toFlag GhcProfAutoToplevel
      ProfDetailExportedFunctions   -> toFlag GhcProfAutoExported
      ProfDetailToplevelFunctions   -> toFlag GhcProfAutoToplevel
      ProfDetailAllFunctions        -> toFlag GhcProfAutoAll
      ProfDetailOther _             -> mempty

#if !MIN_VERSION_Cabal(2,0,0)
allLibModules :: Library -> ComponentLocalBuildInfo -> [ModuleName.ModuleName]
allLibModules lib _ = libModules lib
#endif

combineObjectFiles :: Verbosity -> LocalBuildInfo -> ConfiguredProgram
                   -> FilePath -> [FilePath] -> IO ()
#if MIN_VERSION_Cabal(2,1,0)
combineObjectFiles = Ld.combineObjectFiles
#else
combineObjectFiles v _ = Ld.combineObjectFiles v
#endif
