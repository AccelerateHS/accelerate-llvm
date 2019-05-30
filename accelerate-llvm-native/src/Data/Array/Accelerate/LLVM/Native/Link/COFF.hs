{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.COFF
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link.COFF (

  loadObject,

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.Native.Link.Object

import Data.ByteString                                    ( ByteString )


-- Dynamic object loading
-- ----------------------

-- Load a COFF object file and return pointers to the executable functions
-- defined within. The executable sections are aligned appropriately, as
-- specified in the object file, and are ready to be executed on the target
-- architecture.
--
loadObject :: ByteString -> IO (FunctionTable, ObjectCode)
loadObject _obj =
  $internalError "loadObject" "not implemented yet: https://github.com/AccelerateHS/accelerate/issues/395"

