{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Plugin.BuildInfo
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Plugin.BuildInfo
  where

#if __GLASGOW_HASKELL__ >= 900
import GHC.Unit
import GHC.Utils.Binary
#else
import Binary
import Module
#endif

import Data.Map                                                     ( Map )
import System.Directory
import System.FilePath
import qualified Data.Map                                           as Map
import qualified Data.Map.Internal                                  as Map

import Data.Array.Accelerate.Error


mkBuildInfoFileName :: FilePath -> FilePath
mkBuildInfoFileName path = path </> "accelerate-llvm-native.buildinfo"

readBuildInfo :: HasCallStack => FilePath -> IO (Map Module [FilePath])
readBuildInfo path = do
  exists <- doesFileExist path
  if not exists
    then return Map.empty
    else get =<< readBinMem path

writeBuildInfo :: FilePath -> Map Module [FilePath] -> IO ()
writeBuildInfo path objs = do
  h <- openBinMem 4096
  put_ h objs
  writeBinMem h path


instance (Binary k, Binary v) => Binary (Map k v) where
  get h = do
    t <- getByte h
    case t of
      0 -> return Map.Tip
      _ -> do
        s <- get h
        k <- get h
        a <- get h
        l <- get h
        r <- get h
        return $ Map.Bin s k a l r

  put_ h Map.Tip             = putByte h 0
  put_ h (Map.Bin s k a l r) = do
    putByte h 1
    put_ h s
    put_ h k
    put_ h a
    put_ h l
    put_ h r

