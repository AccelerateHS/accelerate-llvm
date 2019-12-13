{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Plugin.BuildInfo
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Plugin.BuildInfo
  where

import Module

import Data.Map                                                     ( Map )
import Data.Serialize
import System.Directory
import System.FilePath
import qualified Data.ByteString                                    as B
import qualified Data.Map                                           as Map

import Data.Array.Accelerate.Error


mkBuildInfoFileName :: FilePath -> FilePath
mkBuildInfoFileName path = path </> "accelerate-llvm-native.buildinfo"

readBuildInfo :: FilePath -> IO (Map Module [FilePath])
readBuildInfo path = do
  exists <- doesFileExist path
  if not exists
    then return Map.empty
    else do
      f <- B.readFile path
      case decode f of
        Left err -> $internalError "readBuildInfo" err
        Right m  -> return m

writeBuildInfo :: FilePath -> Map Module [FilePath] -> IO ()
writeBuildInfo path objs = B.writeFile path (encode objs)


instance Serialize Module where
  put (Module p n) = put p >> put n
  get = do
    p <- get
    n <- get
    return (Module p n)

#if __GLASGOW_HASKELL__ < 800
instance Serialize PackageKey where
  put p = put (packageKeyString p)
  get = stringToPackageKey <$> get
#else
instance Serialize UnitId where
  put u = put (unitIdString u)
  get   = stringToUnitId <$> get
#endif

instance Serialize ModuleName where
  put m = put (moduleNameString m)
  get   = mkModuleName <$> get

