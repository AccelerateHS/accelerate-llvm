{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Debug
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Debug (

  module Data.Array.Accelerate.Debug.Internal,
  module Data.Array.Accelerate.LLVM.Native.Debug,

) where

import Data.Array.Accelerate.Debug.Internal                         hiding ( elapsed )
import qualified Data.Array.Accelerate.Debug.Internal               as Debug

import Data.Text.Format
import Data.Text.Lazy.Builder
import qualified Data.Text.Buildable                                as B

import Control.Monad.Trans
import Data.Char
import Foreign.C.Types
import Foreign.C.String
import GHC.Ptr
import Language.Haskell.TH


-- | Display elapsed wall and CPU time, together with speedup fraction
--
{-# INLINEABLE elapsedP #-}
elapsedP :: Double -> Double -> Builder
elapsedP wallTime cpuTime =
  build "{} (wall), {} (cpu), {} x speedup"
    ( showFFloatSIBase (Just 3) 1000 wallTime "s"
    , showFFloatSIBase (Just 3) 1000 cpuTime  "s"
    , fixed 2 (cpuTime / wallTime) )

-- | Display elapsed wall and CPU time
--
{-# INLINEABLE elapsedS #-}
elapsedS :: Double -> Double -> Builder
elapsedS = Debug.elapsed


data Phase = Compile | Link | Execute

instance B.Buildable Phase where
  build Compile = "compile"
  build Link    = "link"
  build Execute = "execute"

runQ $
  [d|
      ___run :: CString
      ___run = Ptr $(litE (stringPrimL (map (fromIntegral . ord) "run\0")))

      cstring_of_phase :: Phase -> (CString, CSize)
      cstring_of_phase Compile = (Ptr $(litE (stringPrimL (map (fromIntegral . ord) "compile\0"))), 7)
      cstring_of_phase Link    = (Ptr $(litE (stringPrimL (map (fromIntegral . ord) "link\0"))), 4)
      cstring_of_phase Execute = (Ptr $(litE (stringPrimL (map (fromIntegral . ord) "execute\0"))), 7)
    |]

phase :: MonadIO m => Phase -> (Double -> Double -> Builder) -> m a -> m a
phase p fmt go = do
  let (cp, cpsz) = cstring_of_phase p
  srcloc <- liftIO $ alloc_srcloc_name 0 nullPtr 0 nullPtr 0 cp cpsz
  zone   <- liftIO $ emit_zone_begin srcloc 1
  result <- timed dump_phases (\wall cpu -> build "phase {}: {}" (p, fmt wall cpu)) go
  _      <- liftIO $ emit_zone_end zone
  return result

