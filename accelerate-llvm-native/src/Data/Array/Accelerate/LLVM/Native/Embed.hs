{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Embed
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Embed (

  module Data.Array.Accelerate.LLVM.Embed,

) where

import Data.ByteString.Short.Char8                                  as S8
import Data.ByteString.Short.Extra                                  as BS

import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Embed

import Data.Array.Accelerate.LLVM.Native.Compile
import Data.Array.Accelerate.LLVM.Native.Link
import Data.Array.Accelerate.LLVM.Native.Link.Util
import Data.Array.Accelerate.LLVM.Native.State
import Data.Array.Accelerate.LLVM.Native.Target

import Data.ByteString                                              ( ByteString )
import qualified Data.ByteString                                    as B
import Language.Haskell.TH.Extra                                    ( Q, CodeQ )
import System.FilePath                                              ( takeFileName )
import System.IO.Unsafe
import qualified Language.Haskell.TH.Extra                          as TH

-- TODO: bytestring 0.11.2.0 adds their own, better lifting instances. Once
--       that's stable we should remove this dependency and bump bytestring's
--       version bound.
#if !MIN_VERSION_bytestring(0,11,2)
import Instances.TH.Lift                                            ()
#endif


instance Embed Native where
  embedForTarget = embed

-- | Store the compiled shared object in this binary, and dynamically link to it
-- when the code needs to be run. The cleaner solution would be to create a
-- static archive, link to that, and to then generate FFI imports, but this
-- works and requires the least amount of modifications to the rest of the
-- compiler. If you feel like giving that a shot, then you can use the git
-- pickaxe to find the commit that removed the old approach that did more or
-- less that with `git log -p -S 'TH.bindCode getObjectFile $ \objFile'`.
--
embed :: Native -> ObjectR Native -> CodeQ (ExecutableR Native)
embed _target (ObjectR nms libPath) =
  TH.bindCode sharedLibraryQ $ \lib ->
    [|| unsafePerformIO $
          -- This linking needs to be done runtime, when the 'ExecutableR
          -- Native' gets evaluated. Since may not even run on the same machine
          -- 'embed' was run on we can't use @_target@, but since we're only
          -- linking to a pre-compiled shared library this shouldn't make any
          -- difference.
          withRawSharedObject (takeFileName libPath) lib $ \embeddedLibPath ->
            evalNative defaultTarget . link $! ObjectR $$(nmsQ) embeddedLibPath
     ||]
  where
    sharedLibraryQ :: Q ByteString
    sharedLibraryQ = TH.runIO $! B.readFile libPath

    nmsQ :: CodeQ [ShortByteString]
    nmsQ = listE $ map (\fn -> [|| $$(liftSBS fn) ||]) nms

    listE :: [CodeQ a] -> CodeQ [a]
    listE xs = TH.unsafeCodeCoerce (TH.listE (map TH.unTypeCode xs))
