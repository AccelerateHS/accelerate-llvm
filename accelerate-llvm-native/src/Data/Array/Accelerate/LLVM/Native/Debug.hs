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

phase :: MonadIO m => Phase -> (Double -> Double -> Builder) -> m a -> m a
phase p fmt go = timed dump_phases (\wall cpu -> build "phase {}: {}" (p, fmt wall cpu)) go

{--
phase :: (MonadIO m, HasCallStack) => Phase -> (Double -> Double -> Builder) -> m a -> m a
phase p fmt go = do
  let (p_phase, sz_phase) = case p of
                              Compile -> (Ptr $(litE (stringPrimL (map (fromIntegral . ord) "compile\0"))), 7)
                              Link    -> (Ptr $(litE (stringPrimL (map (fromIntegral . ord) "link\0"))),    4)
                              Execute -> (Ptr $(litE (stringPrimL (map (fromIntegral . ord) "execute\0"))), 7)
      (line, file, fun)   = case getCallStack callStack of
                              []        -> (0, [], [])
                              ((f,l):_) -> (srcLocStartLine l, srcLocFile l, f)
  --
  zone   <- liftIO $
    withCStringLen file $ \(p_file, sz_file) ->
    withCStringLen fun  $ \(p_fun,  sz_fun)  -> do
      srcloc <- alloc_srcloc_name (fromIntegral line) p_file (fromIntegral sz_file) p_fun (fromIntegral sz_fun) p_phase sz_phase
      zone   <- emit_zone_begin srcloc 1
      return zone

  result <- timed dump_phases (\wall cpu -> build "phase {}: {}" (p, fmt wall cpu)) go
  _      <- liftIO $ emit_zone_end zone

  return result
--}

