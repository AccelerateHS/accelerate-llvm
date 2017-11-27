{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Embed
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
import GHC.Ptr                                                      ( Ptr(..) )
import Language.Haskell.TH                                          ( Q, TExp )
import Numeric
import System.IO.Unsafe
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH


instance Embed Native where
  embedForTarget = embed

-- Add the given object code to the set of files to link the executable with,
-- and generate FFI declarations to access the external functions of that file.
-- The returned ExecutableR references the new FFI declarations.
--
embed :: Native -> ObjectR Native -> Q (TExp (ExecutableR Native))
embed target (ObjectR uid nms !_) = do
  objFile <- TH.runIO (evalNative target (cacheOfUID uid))
  funtab  <- forM nms $ \fn -> return [|| ( $$(liftSBS (BS.take (BS.length fn - 17) fn)), $$(makeFFI fn objFile) ) ||]
  --
  [|| NativeR (unsafePerformIO $ newLifetime (FunctionTable $$(listE funtab))) ||]
  where
    listE :: [Q (TExp a)] -> Q (TExp [a])
    listE xs = TH.unsafeTExpCoerce (TH.listE (map TH.unTypeQ xs))

    liftSBS :: ShortByteString -> Q (TExp ShortByteString)
    liftSBS bs =
      let bytes = BS.unpack bs
          len   = BS.length bs
      in
      [|| unsafePerformIO $ BS.createFromPtr $$( TH.unsafeTExpCoerce [| Ptr $(TH.litE (TH.StringPrimL bytes)) |]) len ||]

    makeFFI :: ShortByteString -> FilePath -> Q (TExp (FunPtr ()))
    makeFFI (S8.unpack -> fn) objFile = do
      i   <- TH.runIO newUnique
      fn' <- TH.newName ("__accelerate_llvm_native_" ++ showHex (hash i) [])
      dec <- TH.forImpD TH.CCall TH.Unsafe ('&':fn) fn' [t| FunPtr () |]
      ann <- TH.pragAnnD (TH.ValueAnnotation fn') [| (Object objFile) |]
      TH.addTopDecls [dec, ann]
      TH.unsafeTExpCoerce (TH.varE fn')

