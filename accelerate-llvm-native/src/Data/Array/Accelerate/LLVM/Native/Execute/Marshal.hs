{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute.Marshal
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute.Marshal ( module M )
  where

-- accelerate
import Data.Array.Accelerate.LLVM.Execute.Marshal               as M
import Data.Array.Accelerate.Array.Unique

import Data.Array.Accelerate.LLVM.Native.Execute.Async          () -- instance Async Native
import Data.Array.Accelerate.LLVM.Native.Target

-- libraries
import qualified Data.DList                                     as DL
import qualified Foreign.LibFFI                                 as FFI

import Language.Haskell.TH                                      ( runQ )
import Data.Bits                                                ( FiniteBits( finiteBitSize ) )
import Data.Array.Accelerate.Array.Data                         ( HTYPE_INT )

runQ [d|
         argInt :: HTYPE_INT -> FFI.Arg
         {-# INLINABLE argInt #-}
         argInt = $(case Data.Bits.finiteBitSize (undefined :: Int) of
            32 -> [| FFI.argInt32 |]
            64 -> [| FFI.argInt64 |]
            _  -> error "Failed to detect Int byte size."
          )
      |]

instance Marshal Native where
  type ArgR Native = FFI.Arg

  marshalInt = argInt . fromIntegral
  marshalScalarData' _ = return . DL.singleton . FFI.argPtr . unsafeUniqueArrayPtr

