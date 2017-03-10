{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link (

  module Link,

) where

import Data.Array.Accelerate.LLVM.Native.Link.Object                as Link
#if   defined(darwin_HOST_OS)
import Data.Array.Accelerate.LLVM.Native.Link.MachO                 as Link
#elif defined(linux_HOST_OS)
import Data.Array.Accelerate.LLVM.Native.Link.ELF                   as Link
#elif defined(mingw32_HOST_OS)
import Data.Array.Accelerate.LLVM.Native.Link.COFF                  as Link
#else
#error "Runtime linking not supported on this platform"
#endif

