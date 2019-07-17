-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Distribution.Simple
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Distribution.Simple {-# DEPRECATED "This module is obviated by GHC-8.6 and will be removed in future releases" #-} (

  defaultMain,
  simpleUserHooks,
  module Distribution.Simple,

) where

import Data.Array.Accelerate.LLVM.Native.Distribution.Simple.Build

import Distribution.PackageDescription                              ( PackageDescription )
import Distribution.Simple.Setup                                    ( BuildFlags )
import Distribution.Simple.LocalBuildInfo                           ( LocalBuildInfo )
import Distribution.Simple.PreProcess                               ( PPSuffixHandler, knownSuffixHandlers )
import Distribution.Simple                                          hiding ( defaultMain, simpleUserHooks )
import qualified Distribution.Simple                                as Cabal

import Data.List                                                    ( unionBy )


-- | A simple implementation of @main@ for a Cabal setup script. This is the
-- same as 'Distribution.Simple.defaultMain', with added support for building
-- libraries utilising 'Data.Array.Accelerate.LLVM.Native.runQ'*.
--
defaultMain :: IO ()
defaultMain = Cabal.defaultMainWithHooks simpleUserHooks


-- | Hooks that correspond to a plain instantiation of the \"simple\" build
-- system.
--
simpleUserHooks :: UserHooks
simpleUserHooks =
  Cabal.simpleUserHooks
    { buildHook = accelerateBuildHook
    }

accelerateBuildHook
    :: PackageDescription
    -> LocalBuildInfo
    -> UserHooks
    -> BuildFlags
    -> IO ()
accelerateBuildHook pkg_descr localbuildinfo hooks flags =
  build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)

-- | Combine the preprocessors in the given hooks with the
-- preprocessors built into cabal.
allSuffixHandlers :: UserHooks -> [PPSuffixHandler]
allSuffixHandlers hooks
    = overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
    where
      overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
      overridesPP = unionBy (\x y -> fst x == fst y)

