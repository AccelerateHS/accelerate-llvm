{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Type
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Type
  where

import Data.Array.Accelerate.Type


class IsSigned dict where
  signed   :: dict a -> Bool
  unsigned :: dict a -> Bool

  signed   = not . unsigned
  unsigned = not . signed

instance IsSigned IntegralType where
  signed t =
    case t of
      TypeInt _    -> True
      TypeInt8 _   -> True
      TypeInt16 _  -> True
      TypeInt32 _  -> True
      TypeInt64 _  -> True
      TypeCShort _ -> True
      TypeCInt _   -> True
      TypeCLong _  -> True
      TypeCLLong _ -> True
      _            -> False

instance IsSigned FloatingType where
  signed _ = True

instance IsSigned NumType where
  signed (IntegralNumType t) = signed t
  signed (FloatingNumType t) = signed t

