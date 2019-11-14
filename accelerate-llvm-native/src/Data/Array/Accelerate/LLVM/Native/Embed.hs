{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Embed
-- Copyright   : [2017..2019] The Accelerate Team
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
import Data.ByteString.Short.Internal                               as BS

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
import Language.Haskell.TH                                          ( Q, TExp )
import Numeric
import System.IO.Unsafe
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH

#if __GLASGOW_HASKELL__ >= 806
import Data.Maybe
import qualified Data.Set                                           as Set
#endif


instance Embed Native where
  embedForTarget = embed

-- Add the given object code to the set of files to link the executable with,
-- and generate FFI declarations to access the external functions of that file.
-- The returned ExecutableR references the new FFI declarations.
--
embed :: Native -> ObjectR Native -> Q (TExp (ExecutableR Native))
embed target (ObjectR uid nms !_) = do
  objFile <- getObjectFile
  funtab  <- forM nms $ \fn -> return [|| ( $$(liftSBS (BS.take (BS.length fn - 65) fn)), $$(makeFFI fn objFile) ) ||]
  --
  [|| NativeR (unsafePerformIO $ newLifetime (FunctionTable $$(listE funtab))) ||]
  where
    listE :: [Q (TExp a)] -> Q (TExp [a])
    listE xs = TH.unsafeTExpCoerce (TH.listE (map TH.unTypeQ xs))

    makeFFI :: ShortByteString -> FilePath -> Q (TExp (FunPtr ()))
    makeFFI (S8.unpack -> fn) objFile = do
      i   <- TH.runIO newUnique
      fn' <- TH.newName ("__accelerate_llvm_native_" ++ showHex (hash i) [])
      dec <- TH.forImpD TH.CCall TH.Unsafe ('&':fn) fn' [t| FunPtr () |]
      ann <- TH.pragAnnD (TH.ValueAnnotation fn') [| (Object objFile) |]
      TH.addTopDecls [dec, ann]
      TH.unsafeTExpCoerce (TH.varE fn')

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
      this <- TH.runIO (evalNative target (cacheOfUID uid))
#if __GLASGOW_HASKELL__ >= 806
      rest <- fromMaybe Set.empty <$> TH.getQ
      if Set.member this rest
         then return ()
         else do
           TH.addForeignFilePath TH.RawObject this
           TH.putQ (Set.insert this rest)
#endif
      return this

