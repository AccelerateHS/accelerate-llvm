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

import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Embed

import Data.Array.Accelerate.LLVM.Native.Compile
import Data.Array.Accelerate.LLVM.Native.Compile.Cache
import Data.Array.Accelerate.LLVM.Native.Link
import Data.Array.Accelerate.LLVM.Native.Plugin.Annotation
import Data.Array.Accelerate.LLVM.Native.State
import Data.Array.Accelerate.LLVM.Native.Target

import Control.Concurrent.Unique
import Control.Monad
import Data.Hashable
import Foreign.Ptr
import Language.Haskell.TH.Extra                                    ( Q, CodeQ )
import Numeric
import System.FilePath                                              ( (<.>) )
import System.IO.Unsafe
import qualified Language.Haskell.TH.Extra                          as TH
import qualified Language.Haskell.TH.Syntax                         as TH

import Data.Maybe
import qualified Data.Set                                           as Set


instance Embed Native where
  embedForTarget = embed

-- Add the given object code to the set of files to link the executable with,
-- and generate FFI declarations to access the external functions of that file.
-- The returned ExecutableR references the new FFI declarations.
--
embed :: Native -> ObjectR Native -> CodeQ (ExecutableR Native)
embed target (ObjectR uid nms !_ _) =
  TH.bindCode getObjectFile $ \objFile ->
    [|| NativeR (unsafePerformIO $ newLifetime (FunctionTable $$(listE $ makeTable objFile nms))) ||]
  where
    listE :: [CodeQ a] -> CodeQ [a]
    listE xs = TH.unsafeCodeCoerce (TH.listE (map TH.unTypeCode xs))

    makeTable :: FilePath -> [ShortByteString] -> [CodeQ (ShortByteString, FunPtr ())]
    makeTable objFile = map (\fn -> [|| ( $$(liftSBS fn), $$(makeFFI fn objFile) ) ||])

    makeFFI :: ShortByteString -> FilePath -> CodeQ (FunPtr ())
    makeFFI (S8.unpack -> fn) objFile = TH.bindCode go (TH.unsafeCodeCoerce . return)
      where
        go = do
          i   <- TH.runIO newUnique
          fn' <- TH.newName ("__accelerate_llvm_native_" ++ showHex (hash i) [])
          dec <- TH.forImpD TH.CCall TH.Unsafe ('&':fn) fn' [t| FunPtr () |]
          ann <- TH.pragAnnD (TH.ValueAnnotation fn') [| (Object objFile) |]
          TH.addTopDecls [dec, ann]
          TH.varE fn'

    -- Note: [Template Haskell and raw object files]
    --
    -- We can only addForeignFilePath once per object file, otherwise the
    -- linker will complain about duplicate symbols. To work around this,
    -- we use putQ/getQ to keep track of which object files have already
    -- been encountered during compilation _of the current module_. This
    -- means that we might still run into problems if runQ is invoked at
    -- multiple modules.
    --
    getObjectFile :: Q FilePath
    getObjectFile = do
      cachePath  <- TH.runIO (evalNative target (cacheOfUID uid))
      let objFile = cachePath <.> staticObjExt
#if __GLASGOW_HASKELL__ >= 806
      objSet     <- fromMaybe Set.empty <$> TH.getQ
      unless (Set.member objFile objSet) $ do
        TH.addForeignFilePath TH.RawObject objFile
        TH.putQ (Set.insert objFile objSet)
#endif
      return objFile

-- The file extension for static libraries
--
staticObjExt :: String
#if   defined(mingw32_HOST_OS)
staticObjExt = "obj"
#else
staticObjExt = "o"
#endif

