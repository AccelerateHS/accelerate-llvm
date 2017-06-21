{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Compile.Cache
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Compile.Cache (

  Persistent(..),
  cacheOfOpenAcc, removeCacheDirectory,

) where

import Data.Array.Accelerate.Debug
import Data.Array.Accelerate.Trafo

import Data.Array.Accelerate.LLVM.State

import Control.Monad.Trans
import Data.Version
import System.Directory
import System.FilePath
import Text.Printf

import Paths_accelerate_llvm


-- TODO:
--  * Remove old files which have not been accessed in some time
--  * Reuse old cache files when upgrading to new versions (ala stack)

class Persistent arch where
  -- | Specify a filename template which can be used to cache files for a given
  -- backend. This should also include something to distinguish this
  -- particular backend/target from another; examples:
  --
  targetCacheTemplate :: LLVM arch FilePath


-- | Return the unique cache file path corresponding to a given accelerate
-- computation.
--
{-# INLINEABLE cacheOfOpenAcc #-}
cacheOfOpenAcc
    :: Persistent arch
    => DelayedOpenAcc aenv a
    -> LLVM arch (Int, FilePath)
cacheOfOpenAcc acc = do
  dbg       <- liftIO $ queryFlag debug_cc
  appdir    <- liftIO $ getAppUserDataDirectory "accelerate"
  template  <- targetCacheTemplate
  let
      uid           = hashDelayedOpenAcc acc
      (base, file)  = splitFileName template
      (name, ext)   = splitExtensions file
      --
      cachepath     = appdir </> "accelerate-llvm-" ++ showVersion version </> base </> if dbg then "dbg" else "rel"
      cachefile     = cachepath </> printf "%s%016X" name uid <.> ext
  --
  liftIO $ createDirectoryIfMissing True cachepath
  return (uid, cachefile)


-- | Remove the cache directory
--
{-# INLINEABLE removeCacheDirectory #-}
removeCacheDirectory :: Persistent arch => LLVM arch ()
removeCacheDirectory = do
  appdir    <- liftIO $ getAppUserDataDirectory "accelerate"
  template  <- targetCacheTemplate
  let
      (base, _)     = splitFileName template
      cachepath     = appdir </> "accelerate-llvm-" ++ showVersion version </> base
  --
  liftIO $ removeDirectoryRecursive cachepath

