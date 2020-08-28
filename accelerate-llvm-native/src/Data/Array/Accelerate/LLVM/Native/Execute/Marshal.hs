{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

-- accelerate
import Data.Array.Accelerate.LLVM.Execute.Marshal               as M
import Data.Array.Accelerate.Array.Unique

import Data.Array.Accelerate.LLVM.Native.Execute.Async          () -- instance Async Native
import Data.Array.Accelerate.LLVM.Native.Target

-- libraries
import qualified Data.DList                                     as DL
import qualified Foreign.LibFFI                                 as FFI

instance Marshal Native where
  type ArgR Native = FFI.Arg

  marshalInt = FFI.argInt
  marshalScalarData' _ = return . DL.singleton . FFI.argPtr . unsafeUniqueArrayPtr

