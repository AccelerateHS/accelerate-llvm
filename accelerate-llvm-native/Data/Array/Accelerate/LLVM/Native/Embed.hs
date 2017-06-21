{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Embed

import Data.Array.Accelerate.LLVM.Native.Compile
import Data.Array.Accelerate.LLVM.Native.Link
import Data.Array.Accelerate.LLVM.Native.Target

import Data.FileEmbed
import Language.Haskell.TH
import System.IO.Unsafe
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH


instance Embed Native where
  embedForTarget = embed


embed :: ObjectR Native -> Q (TExp (ExecutableR Native))
embed (ObjectR uid obj) = do
  name <- TH.newName "__acc_native_obj"
  TH.addTopDecls =<< sequence
    [ TH.pragInlD name TH.NoInline TH.FunLike TH.AllPhases
    , TH.sigD name [t| ExecutableR Native |]
    , TH.valD (TH.varP name) (TH.normalB [| unsafePerformIO $ link (ObjectR uid $(bsToExp obj)) |]) []
    ]
  TH.unsafeTExpCoerce (TH.varE name)

