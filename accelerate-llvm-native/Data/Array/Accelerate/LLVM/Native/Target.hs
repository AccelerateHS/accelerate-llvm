{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Target
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Target (

  module Data.Array.Accelerate.LLVM.Target,
  module Data.Array.Accelerate.LLVM.Native.Target

) where

-- llvm-general
import LLVM.Target                                                  hiding ( Target )
import LLVM.AST.DataLayout                                          ( DataLayout )

-- accelerate
import Data.Array.Accelerate.Error                                  ( internalError )

import Data.Array.Accelerate.LLVM.Target                            ( Target(..) )
import Control.Parallel.Meta                                        ( Executable )

-- standard library
import Control.Monad.Except
import System.IO.Unsafe


-- | Native machine code JIT execution target
--
data Native = Native {
    gangSize    :: {-# UNPACK #-} !Int
  , fillS       :: {-# UNPACK #-} !Executable
  , fillP       :: {-# UNPACK #-} !Executable
  }

instance Target Native where
  targetTriple     _ = Just nativeTargetTriple
  targetDataLayout _ = Just nativeDataLayout


-- | String that describes the native target
--
{-# NOINLINE nativeTargetTriple #-}
nativeTargetTriple :: String
nativeTargetTriple = unsafePerformIO $
    -- A target triple suitable for loading code into the current process
    getProcessTargetTriple

-- | A description of the various data layout properties that may be used during
-- optimisation.
--
{-# NOINLINE nativeDataLayout #-}
nativeDataLayout :: DataLayout
nativeDataLayout
  = unsafePerformIO
  $ fmap (either ($internalError "nativeDataLayout") id)
  $ runExceptT (withNativeTargetMachine getTargetMachineDataLayout)


-- | Bracket the creation and destruction of a target machine for the native
-- backend running on this host.
--
withNativeTargetMachine
    :: (TargetMachine -> IO a)
    -> ExceptT String IO a
withNativeTargetMachine = withHostTargetMachine

