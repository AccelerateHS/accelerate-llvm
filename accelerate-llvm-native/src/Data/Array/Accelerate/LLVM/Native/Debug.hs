{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Debug
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Debug (

  module Data.Array.Accelerate.Debug,
  module Data.Array.Accelerate.LLVM.Native.Debug,

) where

import Data.Array.Accelerate.Debug                                  hiding ( elapsed )
import qualified Data.Array.Accelerate.Debug                        as Debug

import Text.Printf


-- | Display elapsed wall and CPU time, together with speedup fraction
--
{-# INLINEABLE elapsedP #-}
elapsedP :: Double -> Double -> String
elapsedP wallTime cpuTime =
  printf "%s (wall), %s (cpu), %.2f x speedup"
    (showFFloatSIBase (Just 3) 1000 wallTime "s")
    (showFFloatSIBase (Just 3) 1000 cpuTime  "s")
    (cpuTime / wallTime)

-- | Display elapsed wall and CPU time
--
{-# INLINEABLE elapsedS #-}
elapsedS :: Double -> Double -> String
elapsedS = Debug.elapsed

