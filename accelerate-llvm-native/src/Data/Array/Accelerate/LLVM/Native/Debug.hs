{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
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

