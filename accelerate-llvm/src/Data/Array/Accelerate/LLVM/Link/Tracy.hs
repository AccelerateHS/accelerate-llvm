{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Link.Tracy
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Link.Tracy ( symtab )
  where

import Data.ByteString                                              ( ByteString )
import Data.HashMap.Strict                                          ( HashMap )
import Foreign.Ptr
import qualified Data.HashMap.Strict                                as HashMap

import Language.Haskell.TH
import Data.Array.Accelerate.Debug.Internal


-- Symbol table for static tracy profiler functions. Using TH because these
-- addresses are only available when accelerate is compiled in profiling
-- mode.
--
runQ $ do
  let funs = [ "___tracy_alloc_srcloc"
             , "___tracy_alloc_srcloc_name"
             , "___tracy_emit_zone_begin_alloc"
             , "___tracy_emit_zone_begin"
             , "___tracy_emit_zone_end"
             , "___tracy_emit_zone_text"
             , "___tracy_emit_zone_name"
             , "___tracy_emit_zone_color"
             , "___tracy_emit_zone_value"
             ]

      foreignD f = return $ ForeignD (ImportF CCall Unsafe ('&':f) (mkName f) (ConT (mkName "FunPtr") `AppT` TupleT 0))

      symtab = [d|
          symtab :: HashMap ByteString (FunPtr ())
          symtab = $( if profilingIsEnabled
                         then [| HashMap.fromList $(listE [tupE [litE (stringL f), varE (mkName f)] | f <- funs]) |]
                         else [| HashMap.empty |] )
        |]

  if profilingIsEnabled
    then concat <$> sequence [ symtab, mapM foreignD funs ]
    else symtab

