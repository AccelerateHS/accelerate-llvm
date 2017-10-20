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

  Persistent(..), UID,
  cacheOfUID,
  cacheOfOpenAcc,
  removeCacheDirectory,

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
  -- particular backend/target from another.
  --
  targetCacheTemplate :: LLVM arch FilePath


-- | Unique identifier for an accelerate computation
--
type UID = Int


-- | Return the unique cache file path corresponding to a given accelerate
-- computation.
--
{-# INLINEABLE cacheOfOpenAcc #-}
cacheOfOpenAcc
    :: Persistent arch
    => DelayedOpenAcc aenv a
    -> LLVM arch (UID, FilePath)
cacheOfOpenAcc acc = do
  let uid = hashDelayedOpenAcc acc
  cacheFile <- cacheOfUID uid
  return (uid, cacheFile)


-- | Return the unique cache file path corresponding to the unique identifier of
-- an accelerate computation.
--
{-# INLINEABLE cacheOfUID #-}
cacheOfUID
    :: Persistent arch
    => UID
    -> LLVM arch FilePath
cacheOfUID uid = do
  dbg       <- liftIO $ if debuggingIsEnabled then getFlag debug else return False
  appdir    <- liftIO $ getAppUserDataDirectory "accelerate"
  template  <- targetCacheTemplate
  let
      (base, file)  = splitFileName template
      (name, ext)   = splitExtensions file
      --
      cachepath     = appdir </> "accelerate-llvm-" ++ showVersion version </> base </> if dbg then "dbg" else "rel"
      cachefile     = cachepath </> printf "%s%016X" name uid <.> ext
  --
  liftIO $ createDirectoryIfMissing True cachepath
  return cachefile


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

