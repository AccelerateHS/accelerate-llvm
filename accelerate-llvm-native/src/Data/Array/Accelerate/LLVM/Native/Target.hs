{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Target
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Target (

  module Data.Array.Accelerate.LLVM.Target,
  module Data.Array.Accelerate.LLVM.Native.Target,
  nativeTargetTriple,
  nativeCPUName,

) where

-- accelerate
import Data.Array.Accelerate.LLVM.Native.Link.Cache                 ( LinkCache )
import Data.Array.Accelerate.LLVM.Native.Execute.Scheduler          ( Workers )
import Data.Array.Accelerate.LLVM.Target                            ( Target(..) )
import Data.Array.Accelerate.LLVM.Target.ClangInfo


-- | Native machine code JIT execution target
--
data Native = Native
  { linkCache     :: !LinkCache
  , workers       :: !Workers
  }

instance Target Native where
  targetTriple     = Just nativeTargetTriple
  targetDataLayout = Nothing  -- LLVM will fill it in just fine for CPU targets
