{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.Object
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link.Object
  where

import Data.List
import Foreign.Ptr
import Formatting

import Data.ByteString.Short.Char8                                  ( ShortByteString, unpack )
import Data.Array.Accelerate.Lifetime

#if defined(mingw32_HOST_OS)
import System.Win32.DLL
import System.Win32.Types
#else
import System.Posix.DynamicLinker
#endif


-- | The function table is a list of function names together with a pointer in
-- the target address space containing the corresponding executable code.
--
data FunctionTable  = FunctionTable { functionTable :: [Function] }
type Function       = (ShortByteString, FunPtr ())

instance Show FunctionTable where
  showsPrec _ f
    = showString "<<"
    . showString (intercalate "," [ unpack n | (n,_) <- functionTable f ])
    . showString ">>"

formatFunctionTable :: Format r (FunctionTable -> r)
formatFunctionTable = later $ \f ->
  bformat (angled (angled (commaSep string))) [ unpack n | (n,_) <- functionTable f ]

-- | Object code consists of a handle to dynamically loaded code, managed
-- by the system linker.
--
type ObjectCode    = Lifetime LibraryHandle

#if defined(mingw32_HOST_OS)
type LibraryHandle = HINSTANCE
#else
type LibraryHandle = DL
#endif

