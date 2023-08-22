{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
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

import Data.Array.Accelerate.LLVM.Execute.Marshal               as M
import Data.Array.Accelerate.Array.Unique

import Data.Array.Accelerate.LLVM.Native.Execute.Async          () -- instance Async Native
import Data.Array.Accelerate.LLVM.Native.Target

import Data.Bits
import qualified Data.DList                                     as DL
import qualified Foreign.LibFFI                                 as FFI


instance Marshal Native where
  type ArgR Native = FFI.Arg
  marshalInt = $( case finiteBitSize (undefined::Int) of
                    32 -> [| FFI.argInt32 . fromIntegral |]
                    64 -> [| FFI.argInt64 . fromIntegral |]
                    _  -> error "I don't know what architecture I am" )
  marshalScalarData' _ = return . DL.singleton . FFI.argPtr . unsafeUniqueArrayPtr

