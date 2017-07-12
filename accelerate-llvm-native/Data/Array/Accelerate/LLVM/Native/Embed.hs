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
import Data.ByteString.Short.Internal                               as BS

import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Embed
import Data.Array.Accelerate.LLVM.Plugin.Annotation

import Data.Array.Accelerate.LLVM.Native.Compile
import Data.Array.Accelerate.LLVM.Native.Compile.Cache
import Data.Array.Accelerate.LLVM.Native.Link
import Data.Array.Accelerate.LLVM.Native.State
import Data.Array.Accelerate.LLVM.Native.Target

import Control.Monad
import Foreign.Ptr
import GHC.Ptr                                                      ( Ptr(..) )
import Language.Haskell.TH                                          ( Q, TExp )
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
  funtab  <- forM nms $ \fn -> return [|| ( $$(liftSBS (S8.takeWhile (/= '_') fn)), $$(makeFFI fn objFile) ) ||]
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
      fn' <- TH.newName ("__accelerate_llvm_native_" ++ fn)
      dec <- TH.forImpD TH.CCall TH.Unsafe ('&':fn) fn' [t| FunPtr () |]
      ann <- TH.pragAnnD (TH.ValueAnnotation fn') [| (Object objFile) |]
      TH.addTopDecls [dec, ann]
      TH.unsafeTExpCoerce (TH.varE fn')

